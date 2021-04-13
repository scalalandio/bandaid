package bandaid.ce2

import cats.arrow.FunctionK
import cats.{ ~>, Applicative, Defer, Functor, Monad }
import cats.data.{ EitherT, Kleisli }
import cats.effect.{ ApplicativeThrow, Concurrent, ExitCase, Fiber, MonadThrow, Sync }
import cats.implicits._
import cats.effect.implicits._

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.runtime.universe._

/**
  * Builds your ZUO module by fixing your IO monad of choice.
  *
  * Intended to use like:
  * {{{
  * // my/domain/io/package.scala
  * package my.domain
  * package object io extends bandaid.ce2.ZUOModule[cats.effect.IO]
  * }}}
  *
  * @tparam F the IO monad
  */
trait ZUOModule[F[_]] extends ZIOModuleLowPriority[F] {

  type =>?[-A, +B] = PartialFunction[A, B]

  private[ce2] type Inner[-I, +E, +O] = I => F[Either[E, O] @uncheckedVariance]

  implicit private[ce2] def wrap[I, E, O](inner: Inner[I, E, O]) = new ZUO(inner)

  /**
    * `ZUO[F, I, E, O]` is a wrapper around `I => F[Either[E, O]]`, which makes it easier to:
    *
    *  - map
    *  - flatMap
    *  - recover
    *  - combine
    *  - etc
    *
    * your IO monad (handling side-effects, laziness, and exceptions), when your `F` values:
    *
    *  - are created from some input `I`
    *  - can fail not only due to exceptions (infrastructure error/error unrelated to your domain), but also due do business rules which
    *    enumerate possible errors as valid results and part of your domain (`E`), which aren't part of your happy path and its output (`O`)
    *
    * Normally, you would have to either use [[cats.data.Kleisli]] and [[cats.data.EitherT]] directly, or rely on tagless final style with
    * MTL or Tagless Final libraries and their type classes.
    *
    * This library only requires the basic type classes for the IO monad (usually already available in scope if their type is known) and
    * only when they are needed. This makes it friendlier to IDEs and increases discoverability.
    *
    * @param unwrap inner representation
    * @tparam I an input used to generate the result
    * @tparam E a typed error that can be handled independently of [[java.lang.Throwable]]
    * @tparam O an output of the successful computation
    */
  final class ZUO[-I, +E, +O](private val unwrap: Inner[I, E, O]) {

    // Contravariant methods

    def contramap[I2](f: I2 => I): ZUO[I2, E, O] = f.andThen(unwrap)

    // Bifunctor methods

    def bimap[E2, O2](fe: E => E2)(fo:         O => O2)(implicit F: Functor[F]): ZUO[I, E2, O2] = unwrap.andThen(_.map(_.bimap(fe, fo)))
    def map[O2](f:        O => O2)(implicit F: Functor[F]): ZUO[I, E, O2] = bimap(identity)(f)
    def mapError[E2](f:   E => E2)(implicit F: Functor[F]): ZUO[I, E2, O] = bimap(f)(identity)
    def as[O2](value:     O2)(implicit F:      Functor[F]): ZUO[I, E, O2] = map(_ => value)

    /** Adjust output as DI (required by [[provideDIFrom]]). */
    def asDI[O2 >: O](implicit F: Functor[F], tag: TypeTag[O2]): ZUO[I, E, DI[O2]] = map(DI(_))

    // Applicative methods

    def map2[I2 <: I, E2 >: E, O2, O3](zuo: ZUO[I2, E2, O2])(f: (O, O2) => O3)(implicit F: Applicative[F]): ZUO[I2, E2, O3] =
      (i: I2) =>
        unwrap(i).map2(zuo.unwrap(i)) {
          case (Right(l), Right(r)) => f(l, r).asRight[E2]
          case (Left(e), _)         => (e: E2).asLeft[O3]
          case (_, Left(e))         => (e: E2).asLeft[O3]
        }
    def left[I2 <: I, E2 >: E, O2](zuo:  ZUO[I2, E2, O2])(implicit F: Applicative[F]): ZUO[I2, E2, O]       = map2(zuo)((l, _) => l)
    def right[I2 <: I, E2 >: E, O2](zuo: ZUO[I2, E2, O2])(implicit F: Applicative[F]): ZUO[I2, E2, O2]      = map2(zuo)((_, r) => r)
    def both[I2 <: I, E2 >: E, O2](zuo:  ZUO[I2, E2, O2])(implicit F: Applicative[F]): ZUO[I2, E2, (O, O2)] = map2(zuo)((l, r) => l -> r)

    // Monad methods

    def flatMap[I2 <: I, E2 >: E, O2](f: O => ZUO[I2, E2, O2])(implicit F: Monad[F]): ZUO[I2, E2, O2] =
      (i: I2) => unwrap(i).flatMap(_.fold((_: E2).asLeft[O2].pure[F], f(_).unwrap(i)))
    def flatTap[I2 <: I, E2 >: E, O2](f:           O => ZUO[I2, E2, O2])(implicit F: Monad[F]): ZUO[I2, E2, O]  = flatMap(o => f(o).as(o))
    def flatten[I2 <: I, E2 >: E, O2](implicit ev: O <:< ZUO[I2, E2, O2], F:         Monad[F]): ZUO[I2, E2, O2] = flatMap(identity(_))
    def former[I2 <: I, E2 >: E, O2](zuo:          ZUO[I2, E2, O2])(implicit F:      Monad[F]): ZUO[I2, E2, O]  = flatTap(_ => zuo)
    def later[I2 <: I, E2 >: E, O2](zuo:           ZUO[I2, E2, O2])(implicit F:      Monad[F]): ZUO[I2, E2, O2] = flatMap(_ => zuo)

    // The rest concurrent and async methods available via the worst Concurrent implementation

    def race[I2 <: I, E2 >: E, O2 >: O](zuo: ZUO[I2, E2, O2])(implicit F: Concurrent[F]): ZUO[I2, E2, Either[O, O2]] =
      (i: I2) =>
        unwrap(i).race(zuo.unwrap(i)).map {
          case Left(Left(ll))   => Left(ll)
          case Left(Right(lr))  => Right(Left(lr))
          case Right(Left(rl))  => Left(rl)
          case Right(Right(rr)) => Right(Right(rr))
        }

    // Typed errors handling

    def handleError[O2 >: O](f: E => O2)(implicit F: Functor[F]): DIO[I, O2] =
      unwrap.andThen(_.map(_.fold(f, identity).asRight))
    def handleSomeError[O2 >: O](f: E =>? O2)(implicit F: Functor[F]): ZUO[I, E, O2] =
      unwrap.andThen(_.map(_.recover(f)))
    def handleErrorWith[I2 <: I, O2 >: O](f: E => ZUO[I2, Nothing, O2])(implicit F: Monad[F]): DIO[I2, O2] =
      (i: I2) => unwrap(i).flatMap(_.fold(e => f(e).unwrap(i), (_: O2).asRight.pure[F]))
    def handleSomeErrorWith[I2 <: I, E2 >: E, O2 >: O](f: E2 =>? ZUO[I2, E2, O2])(implicit F: Monad[F]): ZUO[I2, E2, O2] =
      (i: I2) =>
        unwrap(i).flatMap {
          case Left(error) if f.isDefinedAt(error) => f(error).unwrap(i)
          case either                              => either.pure[F].widen[Either[E2, O2]]
        }

    // Exceptions handling

    def handleException[E2 >: E](f: Throwable => E2)(implicit F: ApplicativeThrow[F]): ZUO[I, E2, O] =
      unwrap.andThen(_.widen[Either[E2, O]].handleError(f(_).asLeft[O]))
    def handleSomeException[E2 >: E](f: Throwable =>? E2)(implicit F: ApplicativeThrow[F]): ZUO[I, E2, O] =
      unwrap.andThen(_.widen[Either[E2, O]].recover(f(_).asLeft[O]))
    def handleExceptionWith[I2 <: I, E2 >: E, O2 >: O](f: Throwable => ZUO[I2, E2, O2])(implicit F: MonadThrow[F]): ZUO[I2, E2, O2] =
      (i: I2) => unwrap(i).widen[Either[E2, O2]].handleErrorWith(f(_).unwrap(i))
    def handleSomeExceptionWith[I2 <: I, E2 >: E, O2 >: O](f: Throwable =>? ZUO[I2, E2, O2])(implicit F: MonadThrow[F]): ZUO[I2, E2, O2] =
      (i: I2) => unwrap(i).widen[Either[E2, O2]].recoverWith(f(_).unwrap(i))

    // Conversions typed error <-> throwable

    def errorToThrowable(f: E => Throwable)(implicit F: MonadThrow[F]): DIO[I, O] =
      handleErrorWith[I, O](f.andThen(ZUO.raiseException(_)))
    def throwableToError[E2 >: E](f: Throwable => E2)(implicit F: MonadThrow[F]): ZUO[I, E2, O] =
      handleExceptionWith[I, E2, O](f.andThen(ZUO.raiseError(_)))

    // Kleisli methods

    def andThen[E2 >: E, O2](appended: ZUO[O, E2, O2])(implicit F: Monad[F]): ZUO[I, E2, O2] =
      unwrap.andThenF {
        case Left(error)  => error.asLeft[O2].pure[F].widen[Either[E2, O2]]
        case Right(value) => appended.unwrap(value).widen[Either[E2, O2]]
      }
    def compose[I2, E2 >: E](prepended: ZUO[I2, E2, I])(implicit F: Monad[F]): ZUO[I2, E2, O] =
      prepended.unwrap.andThenF {
        case Left(error)  => error.asLeft[O].pure[F].widen[Either[E2, O]]
        case Right(value) => unwrap(value).widen[Either[E2, O]]
      }

    // Running

    def run: I => F[Either[E @uncheckedVariance, O @uncheckedVariance]] = unwrap

    // Dependency injection

    /** Provide whole `I` at once. */
    def provide(i: I): BIO[E, O] = (_: Any) => unwrap(i)

    /** Provide one `DI[Sth]` out of whole `DI[X] with DI[Y] with ...` chain. Requires manual resolution of I1 and D. :( */
    def provideDI[D1 <: DI[_], D2 <: DI[_]](d1: D1)(implicit ev: (D1 with D2) <:< I): ZUO[D2, E, O] = (d2: D2) => unwrap(ev(d1 ++ [D2] d2))

    /** Assuming `I` is `DI[X] with DI[Y] with ...` takes some `ZUO[DI..., E2, DI...]` to fill provide some of `DI`s. */
    def provideDIFrom[I2 <: DI[_], E2 >: E, O2 <: DI[_]](
      zuo:         ZUO[I2, E2, O2]
    )(implicit ev: (O2 with I2) <:< I, F: Monad[F]): ZUO[I2, E2, O] =
      (i2: I2) =>
        zuo.unwrap(i2).flatMap[Either[E2, O]] {
          case Left(error) => (error: E2).asLeft[O].pure[F]
          case Right(o2)   => unwrap(ev(o2 ++ [I2] i2)).widen
        }
  }
  type IO[+O]      = ZUO[Any, Nothing, O] // Succeed with an `O`, might throw                , no requirements.
  type BIO[+E, +O] = ZUO[Any, E, O] //       Succeed with an `O`, may fail with `E` or throw , no requirements.
  type DIO[-I, +O] = ZUO[I, Nothing, O] //   Succeed with an `O`, might throw                , requires an `I`.

  object ZUO {

    def apply[I, E, O](f: I => F[Either[E, O]]): ZUO[I, E, O] = f

    def pure[O](value:   O)(implicit F: Applicative[F]): IO[O] = fromEither(value.asRight)
    def unit(implicit F: Applicative[F]): IO[Unit] = pure(())

    def raiseError[E](error: E)(implicit F: Applicative[F]): BIO[E, Nothing] = fromEither(error.asLeft)

    def raiseException(throwable: Throwable)(implicit F: ApplicativeThrow[F]): IO[Nothing] =
      fromEitherF(throwable.raiseError[F, Either[Nothing, Nothing]])

    def defer[I, E, O](zuo: => ZUO[I, E, O])(implicit F: Monad[F], defer: Defer[F]): ZUO[I, E, O] =
      ((_: I) => defer.defer(zuo.asRight.pure[F])).flatten
    def delay[O](o: => O)(implicit F: Sync[F]): IO[O] = (_: Any) => F.delay(o.asRight)

    def pass[I](implicit F: Applicative[F]): DIO[I, I] = (_: I).asRight.pure[F]

    def fromEither[E, O](either:   Either[E, O])(implicit F: Applicative[F]): BIO[E, O] = fromEitherF(either.pure[F])
    def fromEitherF[E, O](eitherF: F[Either[E, O]]): BIO[E, O] = (_: Any) => eitherF

    def liftF[O](fo:               F[O])(implicit F:              Functor[F]):     IO[O]        = fromEitherF(fo.map(_.asRight))
    def liftFunction[I, O](f:      I => O)(implicit F:            Applicative[F]): DIO[I, O]    = pass[I].map(f)
    def liftValidation[I, E, O](f: I => Either[E, O])(implicit F: Monad[F]):       ZUO[I, E, O] = pass[I].flatMap(i => fromEither(f(i)))

    def liftCats[I, E, O](f: Kleisli[EitherT[F, E, *] @uncheckedVariance, I, O @uncheckedVariance]): ZUO[I, E, O] = f.mapF(_.value).run

    def liftK(implicit F: Functor[F]): F ~> IO = {
      def fun[A](fa: F[A]) = liftF(fa)
      FunctionK.lift[F, IO](fun)
    }
  }

  final class ZUOConcurrent[I, E](implicit F: Concurrent[F]) extends Concurrent[ZUO[I, E, *]] {

    override def start[A](fa: ZUO[I, E, A]): ZUO[I, E, Fiber[ZUO[I, E, *], A]] = ZUO { i =>
      PassError.in { passed: PassError[E] =>
        passed
          .hide(fa.run(i))
          .start
          .map(_.mapK(new (F ~> ZUO[I, E, *]) { def apply[A1](fa: F[A1]): ZUO[I, E, A1] = ZUO(_ => passed.show(fa)) }).asRight[E])
      }
    }

    override def racePair[A, B](
      fa: ZUO[I, E, A],
      fb: ZUO[I, E, B]
    ): ZUO[I, E, Either[(A, Fiber[ZUO[I, E, *], B]), (Fiber[ZUO[I, E, *], A], B)]] = ZUO { i =>
      PassError.in { passed1: PassError[E] =>
        PassError.in { passed2: PassError[E] =>
          passed1
            .hide(fa.run(i))
            .racePair(passed2.hide(fb.run(i)))
            .map[Either[(A, Fiber[ZUO[I, E, *], B]), (Fiber[ZUO[I, E, *], A], B)]] {
              case Left((a, fiberB)) =>
                Left(a -> fiberB.mapK(new (F ~> ZUO[I, E, *]) { def apply[A1](fa: F[A1]): ZUO[I, E, A1] = ZUO(_ => passed1.show(fa)) }))
              case Right((fiberA, b)) =>
                Right(fiberA.mapK(new (F ~> ZUO[I, E, *]) { def apply[A1](fa: F[A1]): ZUO[I, E, A1] = ZUO(_ => passed2.show(fa)) }) -> b)
            }
            .map(_.asRight[E])
        }
      }
    }

    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): ZUO[I, E, A] = ZUO.liftF(F.async(k))

    override def asyncF[A](k: (Either[Throwable, A] => Unit) => ZUO[I, E, Unit]): ZUO[I, E, A] = ZUO { i =>
      PassError.in { passed: PassError[E] => passed.show(F.asyncF(k.andThen(_.run(i)).andThen(passed.hide(_)))) }
    }

    override def suspend[A](thunk: => ZUO[I, E, A]): ZUO[I, E, A] = ZUO(i => F.defer(thunk.run(i)))

    override def bracketCase[A, B](
      acquire: ZUO[I, E, A]
    )(use:     A => ZUO[I, E, B])(
      release: (A, ExitCase[Throwable]) => ZUO[I, E, Unit]
    ): ZUO[I, E, B] = ZUO[I, E, B] { i =>
      PassError.in { passed: PassError[E] =>
        passed.hide(acquire.run(i)).bracketCase(use(_).run(i))((a, ec) => passed.hide(release(a, ec).run(i))).recover {
          case passed(error) => error.asLeft[B]
        }
      }
    }

    override def raiseError[A](e: Throwable): ZUO[I, E, A] = ZUO.raiseException(e)

    override def handleErrorWith[A](fa: ZUO[I, E, A])(f: Throwable => ZUO[I, E, A]): ZUO[I, E, A] = fa.handleExceptionWith(f)

    override def flatMap[A, B](fa: ZUO[I, E, A])(f: A => ZUO[I, E, B]): ZUO[I, E, B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => ZUO[I, E, Either[A, B]]): ZUO[I, E, B] = ZUO { i =>
      a.tailRecM(
        // map into outer: Left = continue, Right = failed or finished
        f(_).run(i).map[Either[A, Either[E, B]]] {
          case Left(error)           => error.asLeft[B].asRight[A]
          case Right(Left(continue)) => continue.asLeft[Either[E, B]]
          case Right(Right(result))  => result.asRight[E].asRight[A]
        }
      )
    }

    override def pure[A](x: A): ZUO[I, E, A] = ZUO.pure(x)
  }
}
trait ZIOModuleLowPriority[F[_]] { self: ZUOModule[F] =>

  // FIXME
  //implicit def adjustZUOTypeclass[TC[_], I, E](implicit tc: TC[ZUO[Any, Nothing, *]]): TC[ZUO[I, E, *]] = tc.asInstanceOf[TC[ZUO[I, E, *]]]
}

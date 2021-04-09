package bandaid.ce2

import cats.arrow.FunctionK
import cats.{ ~>, Applicative, Defer, Functor, Monad }
import cats.data.{ EitherT, Kleisli }
import cats.effect.Sync
import cats.implicits._

import scala.annotation.unchecked.uncheckedVariance

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
  * Normally, you would have to either use [[cats.data.Kleisli]] and [[cats.data.EitherT]] directly, or rely on tagless final style with MTL
  * or Tagless Final libraries and their type classes.
  *
  * This library only requires the basic type classes for the IO monad (usually already available in scope if their type is known) and only
  * when they are needed. This makes it friendlier to IDEs and increases discoverability.
  *
  * @param unwrap inner representation
  * @tparam F the IO monad
  * @tparam I an input used to generate the result
  * @tparam E a typed error that can be handled independently of [[java.lang.Throwable]]
  * @tparam O an output of the successful computation
  */
final class ZUO[F[_], -I, +E, +O](private val unwrap: Inner[F, I, E, O]) extends AnyVal with ZUOAliases[F] {

  // Contravariant methods

  def contramap[I2](f: I2 => I): ZUO[I2, E, O] = f.andThen(unwrap).wrap

  // Bifunctor methods

  def bimap[E2, O2](fe: E => E2)(fo:         O => O2)(implicit F: Functor[F]): ZUO[I, E2, O2] = unwrap.andThen(_.map(_.bimap(fe, fo))).wrap
  def map[O2](f:        O => O2)(implicit F: Functor[F]): ZUO[I, E, O2] = bimap(identity(_))(f)
  def mapError[E2](f:   E => E2)(implicit F: Functor[F]): ZUO[I, E2, O] = bimap(f)(identity(_))
  def as[O2](value:     O2)(implicit F:      Functor[F]): ZUO[I, E, O2] = map(_ => value)

  // Applicative methods

  def map2[I2 <: I, E2 >: E, O2, O3](zuo: ZUO[I2, E2, O2])(f: (O, O2) => O3)(implicit F: Applicative[F]): ZUO[I2, E2, O3] = { (i: I2) =>
    unwrap(i).map2(zuo.unwrap(i)) {
      case (Right(l), Right(r)) => f(l, r).asRight[E2]
      case (Left(e), _)         => (e: E2).asLeft[O3]
      case (_, Left(e))         => (e: E2).asLeft[O3]
    }
  }.wrap
  def left[I2 <: I, E2 >: E, O2](zuo:  ZUO[I2, E2, O2])(implicit F: Applicative[F]): ZUO[I2, E2, O]       = map2(zuo)((l, _) => l)
  def right[I2 <: I, E2 >: E, O2](zuo: ZUO[I2, E2, O2])(implicit F: Applicative[F]): ZUO[I2, E2, O2]      = map2(zuo)((_, r) => r)
  def both[I2 <: I, E2 >: E, O2](zuo:  ZUO[I2, E2, O2])(implicit F: Applicative[F]): ZUO[I2, E2, (O, O2)] = map2(zuo)((l, r) => l -> r)

  // TODO: zipping and parallel computations >>, <<, par*

  // Monad methods

  def flatMap[I2 <: I, E2 >: E, O2](f: O => ZUO[I2, E2, O2])(implicit F: Monad[F]): ZUO[I2, E2, O2] =
    ((i: I2) => unwrap(i).flatMap(_.fold((_: E2).asLeft[O2].pure[F], f(_).unwrap(i)))).wrap
  def flatten[I2 <: I, E2 >: E, O2](implicit ev: O <:< ZUO[I2, E2, O2], F: Monad[F]): ZUO[I2, E2, O2] =
    flatMap(identity(_))

  // Typed errors handling

  def handleError[O2 >: O](f: E => O2)(implicit F: Functor[F]): DIO[I, O2] =
    unwrap.andThen(_.map(_.fold(f, identity).asRight)).wrap
  def handleSomeError[O2 >: O](f: E =>? O2)(implicit F: Functor[F]): ZUO[I, E, O2] =
    unwrap.andThen(_.map(_.recover(f))).wrap
  def handleErrorWith[I2 <: I, O2 >: O](f: E => ZUO[I2, Nothing, O2])(implicit F: Monad[F]): DIO[I2, O2] =
    ((i: I2) => unwrap(i).flatMap(_.fold(e => f(e).unwrap(i), (_: O2).asRight.pure[F]))).wrap
  def handleSomeErrorWith[I2 <: I, E2 >: E, O2 >: O](f: E2 =>? ZUO[I2, E2, O2])(implicit F: Monad[F]): ZUO[I2, E2, O2] = { (i: I2) =>
    unwrap(i).flatMap {
      case Left(error) if f.isDefinedAt(error) => f(error).unwrap(i)
      case either                              => either.pure[F].widen[Either[E2, O2]]
    }
  }.wrap

  // Exceptions handling

  def handleException[E2 >: E](f: Throwable => E2)(implicit F: ApplicativeThrow[F]): ZUO[I, E2, O] =
    unwrap.andThen(_.widen[Either[E2, O]].handleError(f(_).asLeft[O])).wrap
  def handleSomeException[E2 >: E](f: Throwable =>? E2)(implicit F: ApplicativeThrow[F]): ZUO[I, E2, O] =
    unwrap.andThen(_.widen[Either[E2, O]].recover(f(_).asLeft[O])).wrap
  def handleExceptionWith[I2 <: I, E2 >: E, O2 >: O](f: Throwable => ZUO[I2, E2, O2])(implicit F: MonadThrow[F]): ZUO[I2, E2, O2] =
    ((i: I2) => unwrap(i).widen[Either[E2, O2]].handleErrorWith(f(_).unwrap(i))).wrap
  def handleSomeExceptionWith[I2 <: I, E2 >: E, O2 >: O](f: Throwable =>? ZUO[I2, E2, O2])(implicit F: MonadThrow[F]): ZUO[I2, E2, O2] =
    ((i: I2) => unwrap(i).widen[Either[E2, O2]].recoverWith(f(_).unwrap(i))).wrap

  // Conversions typed error <-> throwable

  def errorToThrowable(f: E => Throwable)(implicit F: MonadThrow[F]): DIO[I, O] =
    handleErrorWith[I, O](f.andThen(ZUO.raiseException[F](_)))
  def throwableToError[E2 >: E](f: Throwable => E2)(implicit F: MonadThrow[F]): ZUO[I, E2, O] =
    handleExceptionWith[I, E2, O](f.andThen(ZUO.raiseError[F](_)))

  // Kleisli methods

  def andThen[E2 >: E, O2](appended: ZUO[O, E2, O2])(implicit F: Monad[F]): ZUO[I, E2, O2] =
    unwrap.andThenF {
      case Left(error)  => error.asLeft[O2].pure[F].widen[Either[E2, O2]]
      case Right(value) => appended.unwrap(value).widen[Either[E2, O2]]
    }.wrap
  def compose[I2, E2 >: E](prepended: ZUO[I2, E2, I])(implicit F: Monad[F]): ZUO[I2, E2, O] =
    prepended.unwrap.andThenF {
      case Left(error)  => error.asLeft[O].pure[F].widen[Either[E2, O]]
      case Right(value) => unwrap(value).widen[Either[E2, O]]
    }.wrap

  // Running

  def run: I => F[Either[E @uncheckedVariance, O @uncheckedVariance]] = unwrap

  def provide(i: I): BIO[E, O] = ((_: Any) => unwrap(i)).wrap

  // TODO: promenade

  // TODO: something like ZIO layer?
}

object ZUO {

  final private[ce2] class PureBuilder[F[_]] {
    def apply[O](value: O)(implicit F: Applicative[F]): ZUO[F, Any, Nothing, O] =
      ((_: Any) => value.asRight.pure[F]).wrap
  }
  def pure[F[_]]: PureBuilder[F] = new PureBuilder[F]
  def unit[F[_]: Applicative]: ZUO[F, Any, Nothing, Unit] = pure[F](())

  final private[ce2] class ErrorBuilder[F[_]] {
    def apply[E](error: E)(implicit F: Applicative[F]): ZUO[F, Any, E, Nothing] =
      ((_: Any) => error.asLeft.pure[F]).wrap
  }
  def raiseError[F[_]]: ErrorBuilder[F] = new ErrorBuilder[F]

  final private[ce2] class ExceptionBuilder[F[_]] {
    def apply(throwable: Throwable)(implicit F: ApplicativeThrow[F]): ZUO[F, Any, Nothing, Nothing] =
      ((_: Any) => F.raiseError[Either[Nothing, Nothing]](throwable)).wrap
  }
  def raiseException[F[_]]: ExceptionBuilder[F] = new ExceptionBuilder[F]

  final private[ce2] class DeferBuilder[F[_]] {
    def apply[I, E, O](zuo: => ZUO[F, I, E, O])(implicit F: Monad[F], defer: Defer[F]): ZUO[F, I, E, O] =
      ((_: I) => defer.defer(zuo.asRight.pure[F])).wrap.flatten
  }
  def defer[F[_]]: DeferBuilder[F] = new DeferBuilder[F]

  final private[ce2] class DelayBuilder[F[_]] {
    def apply[O](o: => O)(implicit F: Sync[F]): ZUO[F, Any, Nothing, O] =
      ((_: Any) => F.delay(o.asRight)).wrap
  }
  def delay[F[_]]: DelayBuilder[F] = new DelayBuilder[F]

  def lift[F[_], I, E, O](f: I => F[Either[E, O]]): ZUO[F, I, E, O] = f.wrap

  def fromCats[F[_], I, E, O](
    f: Kleisli[EitherT[F, E, *] @uncheckedVariance, I, O @uncheckedVariance]
  ): ZUO[F, I, E, O] = f.mapF(_.value).run.wrap

  final private[ce2] class EitherBuilder[F[_]] {
    def apply[E, O](either: Either[E, O])(implicit F: Applicative[F]): ZUO[F, Any, E, O] =
      ((_: Any) => either.pure[F]).wrap
  }
  def fromEither[F[_]]: EitherBuilder[F] = new EitherBuilder[F]

  final private[ce2] class FBuilder[F[_]] {
    def apply[O](fo: F[O])(implicit F: Functor[F]): ZUO[F, Any, Nothing, O] =
      ((_: Any) => fo.map(_.asRight)).wrap
  }
  def liftF[F[_]]: FBuilder[F] = new FBuilder[F]

  def liftK[F[_]: Functor]: F ~> ZUO[F, Any, Nothing, *] = {
    def fun[A](fa: F[A]) = liftF(fa)
    FunctionK.lift[F, ZUO[F, Any, Nothing, *]](fun)
  }

  // TODO: provide type classes
}

package bandaid.ce2

import bandaid.ce2.IEO._
import cats.arrow.FunctionK
import cats.{ ~>, Applicative, ApplicativeError, Defer, Functor, Monad, MonadError }
import cats.data.{ EitherT, Kleisli }
import cats.effect.Sync
import cats.implicits._

import scala.annotation.unchecked.uncheckedVariance

/**
  * `IEO[F, I, E, O]` is a wrapper around `I => F[Either[E, O]]`, which makes it easier to:
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
  *  - can fail not only due to exceptions (infrastructure error/error unrelated to your domain), but also due do
  *    business rules which enumerate possible errors as valid results and part of your domain (`E`),
  *    which aren't part of your happy path and its output (`O`)
  *
  * Normally, you would have to either use [[cats.data.Kleisli]] and [[cats.data.EitherT]] directly, or rely on tagless
  * final style with MTL or Tagless Final libraries and their type classes.
  *
  * This library only requires the basic type classes for the IO monad (usually already available in scope if their type
  * is known) and only when they are needed. This makes it friendlier to IDEs and increases discoverability.
  *
  * @param unwrap inner representation
  * @tparam F the IO monad
  * @tparam I an input used to generate the result
  * @tparam E a typed error that can be handled independently of [[java.lang.Throwable]]
  * @tparam O an output of the successful computation
  */
final class IEO[F[_], -I, +E, +O](private val unwrap: Inner[F, I, E, O]) extends AnyVal {

  def as[O2](value: O2)(implicit F: Functor[F]): IEO[F, I, E, O2] = unwrap.andThen(_.map(_.map(_ => value))).wrap

  def contramap[I2](f: I2 => I): IEO[F, I2, E, O] = (f andThen unwrap).wrap

  def map[O2](f:        O => O2)(implicit F: Functor[F]): IEO[F, I, E, O2] = unwrap.andThen(_.map(_.map(f))).wrap
  def mapError[E2](f:   E => E2)(implicit F: Functor[F]): IEO[F, I, E2, O] = unwrap.andThen(_.map(_.leftMap(f))).wrap
  def bimap[E2, O2](fe: E => E2)(fo:         O => O2)(implicit F: Functor[F]): IEO[F, I, E2, O2] =
    unwrap.andThen(_.map(_.bimap(fe, fo))).wrap

  def flatMap[I2 <: I, E2 >: E, O2](f: O => IEO[F, I2, E2, O2])(implicit F: Monad[F]): IEO[F, I2, E2, O2] =
    ((i: I2) => unwrap(i).flatMap(_.fold((_: E2).asLeft[O2].pure[F], f(_).unwrap(i)))).wrap
  def flatten[I2 <: I, E2 >: E, O2](implicit ev: O <:< IEO[F, I2, E2, O2], F: Monad[F]): IEO[F, I2, E2, O2] =
    flatMap(identity(_))

  def handleError[O2 >: O](f: E => O2)(implicit F: Functor[F]): IEO[F, I, Nothing, O2] =
    unwrap.andThen(_.map(_.fold(e => f(e).asRight, _.asRight))).wrap
  def handleSomeError[O2 >: O](f: PartialFunction[E, O2])(implicit F: Functor[F]): IEO[F, I, E, O2] =
    unwrap.andThen(_.map(_.recover(f))).wrap
  def handleErrorWith[I2 <: I, O2 >: O](
    f: E => IEO[F, I2, Nothing, O2]
  )(
    implicit F: Monad[F]
  ): IEO[F, I2, Nothing, O2] = ((i: I2) => unwrap(i).flatMap(_.fold(e => f(e).unwrap(i), (_: O2).asRight.pure[F]))).wrap
  def handleSomeErrorWith[I2 <: I, E2 >: E, O2 >: O](
    f: PartialFunction[E2, IEO[F, I2, E2, O2]]
  )(
    implicit F: Monad[F]
  ): IEO[F, I2, E2, O2] = { (i: I2) =>
    unwrap(i).flatMap {
      case Left(error) if f.isDefinedAt(error) => f(error).unwrap(i)
      case either                              => either.pure[F].widen[Either[E2, O2]]
    }
  }.wrap

  def handleException[E2 >: E](f: Throwable => E2)(implicit F: ApplicativeError[F, Throwable]): IEO[F, I, E2, O] =
    unwrap.andThen(_.widen[Either[E2, O]].handleError(f(_).asLeft[O])).wrap
  def handleSomeException[E2 >: E](
    f:          PartialFunction[Throwable, E2]
  )(implicit F: ApplicativeError[F, Throwable]): IEO[F, I, E2, O] =
    unwrap.andThen(_.widen[Either[E2, O]].recover(f(_).asLeft[O])).wrap
  def handleExceptionWith[I2 <: I, E2 >: E, O2 >: O](
    f:          Throwable => IEO[F, I2, E2, O2]
  )(implicit F: MonadError[F, Throwable]): IEO[F, I2, E2, O2] =
    ((i: I2) => unwrap(i).widen[Either[E2, O2]].handleErrorWith(f(_).unwrap(i))).wrap
  def handleSomeExceptionWith[I2 <: I, E2 >: E, O2 >: O](
    f:          PartialFunction[Throwable, IEO[F, I2, E2, O2]]
  )(implicit F: MonadError[F, Throwable]): IEO[F, I2, E2, O2] =
    ((i: I2) => unwrap(i).widen[Either[E2, O2]].recoverWith(f(_).unwrap(i))).wrap

  def errorToThrowable(f: E => Throwable)(implicit F: MonadError[F, Throwable]): IEO[F, I, Nothing, O] =
    handleErrorWith[I, O](f.andThen(IEO.raiseException[F](_)))
  def throwableToError[E2 >: E](f: Throwable => E2)(implicit F: MonadError[F, Throwable]): IEO[F, I, E2, O] =
    handleExceptionWith[I, E2, O](f.andThen(IEO.raiseError[F](_)))

  // TODO: zipping and parallel computations

  def andThen[E2 >: E, O2](appended: IEO[F, O, E2, O2])(implicit F: Monad[F]): IEO[F, I, E2, O2] =
    unwrap.andThenF {
      case Left(error)  => error.asLeft[O2].pure[F].widen[Either[E2, O2]]
      case Right(value) => appended.unwrap(value).widen[Either[E2, O2]]
    }.wrap

  def compose[I2, E2 >: E](prepended: IEO[F, I2, E2, I])(implicit F: Monad[F]): IEO[F, I2, E2, O] =
    prepended.unwrap.andThenF {
      case Left(error)  => error.asLeft[O].pure[F].widen[Either[E2, O]]
      case Right(value) => unwrap(value).widen[Either[E2, O]]
    }.wrap

  def provide(i: I): IEO[F, Any, E, O] = ((_: Any) => unwrap(i)).wrap

  // TODO: something like ZIO layer?

  def runToFEither(i: I): F[Either[E @uncheckedVariance, O @uncheckedVariance]] = unwrap(i)
}

object IEO {

  private[ce2] type Inner[F[_], -I, +E, +O] = I => F[Either[E, O] @uncheckedVariance]
  implicit private[ce2] class InnerWrap[F[_], -I, +E, +O](private val inner: Inner[F, I, E, O]) extends AnyVal {
    def wrap: IEO[F, I, E, O] = new IEO(inner)
  }

  final private[ce2] class PureBuilder[F[_]] {
    def apply[O](value: O)(implicit F: Applicative[F]): IEO[F, Any, Nothing, O] =
      ((_: Any) => value.asRight.pure[F]).wrap
  }
  def pure[F[_]]: PureBuilder[F] = new PureBuilder[F]
  def unit[F[_]: Applicative]: IEO[F, Any, Nothing, Unit] = pure[F](())

  final private[ce2] class ErrorBuilder[F[_]] {
    def apply[E](error: E)(implicit F: Applicative[F]): IEO[F, Any, E, Nothing] =
      ((_: Any) => error.asLeft.pure[F]).wrap
  }
  def raiseError[F[_]]: ErrorBuilder[F] = new ErrorBuilder[F]

  final private[ce2] class ExceptionBuilder[F[_]] {
    def apply(throwable: Throwable)(implicit F: ApplicativeError[F, Throwable]): IEO[F, Any, Nothing, Nothing] =
      ((_: Any) => F.raiseError[Either[Nothing, Nothing]](throwable)).wrap
  }
  def raiseException[F[_]]: ExceptionBuilder[F] = new ExceptionBuilder[F]

  final private[ce2] class DeferBuilder[F[_]] {
    def apply[I, E, O](ieo: => IEO[F, I, E, O])(implicit F: Monad[F], defer: Defer[F]): IEO[F, I, E, O] =
      ((_: I) => defer.defer(ieo.asRight.pure[F])).wrap.flatten
  }
  def defer[F[_]]: DeferBuilder[F] = new DeferBuilder[F]

  final private[ce2] class DelayBuilder[F[_]] {
    def apply[O](o: => O)(implicit F: Sync[F]): IEO[F, Any, Nothing, O] =
      ((_: Any) => F.delay(o.asRight)).wrap
  }
  def delay[F[_]]: DelayBuilder[F] = new DelayBuilder[F]

  def lift[F[_], I, E, O](f: I => F[Either[E, O]]): IEO[F, I, E, O] = f.wrap

  def fromCats[F[_], I, E, O](
    f: Kleisli[EitherT[F, E, *] @uncheckedVariance, I, O @uncheckedVariance]
  ): IEO[F, I, E, O] = f.mapF(_.value).run.wrap

  final private[ce2] class EitherBuilder[F[_]] {
    def apply[E, O](either: Either[E, O])(implicit F: Applicative[F]): IEO[F, Any, E, O] =
      ((_: Any) => either.pure[F]).wrap
  }
  def fromEither[F[_]]: EitherBuilder[F] = new EitherBuilder[F]

  final private[ce2] class FBuilder[F[_]] {
    def apply[O](fo: F[O])(implicit F: Functor[F]): IEO[F, Any, Nothing, O] =
      ((_: Any) => fo.map(_.asRight)).wrap
  }
  def liftF[F[_]]: FBuilder[F] = new FBuilder[F]

  def liftK[F[_]: Functor]: F ~> IEO[F, Any, Nothing, *] = {
    def fun[A](fa: F[A]) = liftF(fa)
    FunctionK.lift[F, IEO[F, Any, Nothing, *]](fun)
  }

  // TODO: provide type classes
}

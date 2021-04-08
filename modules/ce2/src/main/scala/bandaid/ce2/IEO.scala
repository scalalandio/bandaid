package bandaid.ce2

import bandaid.ce2.IEO._
import cats.data.{EitherT, Kleisli}
import cats.{Applicative, Functor, Monad}

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
  *  - can fail not only due to exceptions (infrastructure error, unrelated to your domain), but also due do business
  *    rules which enumerate possible errors as valid results and part of your domain (`E)`,
  *    which aren't part of your happy path and its output (`O`)
  *
  * Normally, you would have to either use [[cats.data.Kleisli]] and [[cats.data.EitherT]] directly, or rely on tagless
  * final style with MTL or Tagless Final libraries and their type classes.
  *
  * This library only requires the basic type classes for the IO monad (usually already available in scope if their type
  * is known) and only when they are needed. This makes it friendlier to IDEs and increases discoverability.
  *
  * @param unwrap inner representation using [[cats.data.Kleisli]] and [[cats.data.EitherT]]
  * @tparam F the IO monad
  * @tparam I an input used to generate the result
  * @tparam E a typed error that can be handled independently of [[java.lang.Throwable]]
  * @tparam O an output of the successful computation
  */
final class IEO[F[_], -I, +E, +O](
  private val unwrap: Inner[F, I, E, O]
) extends AnyVal {

  def contramap[I2](f: I2 => I): IEO[F, I2, E, O] = unwrap.local(f).wrap

  def map[O2](f:        O => O2)(implicit F: Functor[F]): IEO[F, I, E, O2] = unwrap.map(f).wrap
  def mapError[E2](f:   E => E2)(implicit F: Functor[F]): IEO[F, I, E2, O] = unwrap.mapF(_.leftMap(f)).wrap
  def bimap[E2, O2](fe: E => E2)(fo:         O => O2)(implicit F: Functor[F]): IEO[F, I, E2, O2] =
    unwrap.mapF(_.bimap(fe, fo)).wrap

  def flatMap[I2 <: I, E2 >: E, O2](f: O => IEO[F, I2, E2, O2])(implicit F: Monad[F]): IEO[F, I2, E2, O2] =
    (unwrap: Inner[F, I2, E2, O]).flatMap[O2, I2](f(_).unwrap).wrap

  // TODO: handleErrorWith, handleSomeErrorWith
  def handleError[O2 >: O](f: E => O2)(implicit F: Monad[F]): IEO[F, I, Nothing, O2] =
    (unwrap: Inner[F, I, E, O2]).mapF(_.biflatMap[Nothing, O2](e => EitherT.pure(f(e)), EitherT.pure(_))).wrap
  def handleSomeError[O2 >: O](f: PartialFunction[E, O2])(implicit F: Functor[F]): IEO[F, I, E, O2] =
    (unwrap: Inner[F, I, E, O2]).mapF(_.recover(f)).wrap

  // TODO: handleExceptions, handleSomeExceptions using ApplicativeError/MonadError
  // TODO: handleExceptionsWith, handleSomeExceptionsWith using ApplicativeError/MonadError

  // TODO: error to throwable
  // TODO: throwable to error

  // TODO: zipping an parallel computations

  def andThen[E2 >: E, O2](appended: IEO[F, O, E2, O2])(implicit F: Monad[F]): IEO[F, I, E2, O2] =
    (unwrap: Inner[F, I, E2, O]).andThen(appended.unwrap).wrap
  def compose[I2, E2 >: E](prepended: IEO[F, I2, E2, I])(implicit F: Monad[F]): IEO[F, I2, E2, O] =
    (unwrap: Inner[F, I, E2, O]).compose(prepended.unwrap).wrap

  def provide(i: I): IEO[F, Any, E, O] = Kleisli.liftF(unwrap.run(i)).wrap

  def runToFEither(i: I): F[Either[E @uncheckedVariance, O @uncheckedVariance]] = unwrap.run(i).value
}

object IEO {

  private type Inner[F[_], -I, +E, +O] = Kleisli[EitherT[F, E, *] @uncheckedVariance, I, O @uncheckedVariance]
  implicit class InnerWrap[F[_], -I, +E, +O](private val inner: Inner[F, I, E, O]) extends AnyVal {
    def wrap: IEO[F, I, E, O] = new IEO(inner)
  }

  final class PureBuilder[F[_]] {
    def apply[A](value: A)(implicit F: Applicative[F]): IEO[F, Any, Nothing, A] =
      Kleisli.liftF(EitherT.pure(value)).wrap
  }
  def pure[F[_]]: PureBuilder[F] = new PureBuilder[F]
  def unit[F[_]: Applicative]: IEO[F, Any, Nothing, Unit] = pure[F](())

  // TODO: raiseError
  // TODO: raiseException
  // TODO: delay
  // TODO: defer

  // TODO: lifting utils

  // TODO: provide type classes
}

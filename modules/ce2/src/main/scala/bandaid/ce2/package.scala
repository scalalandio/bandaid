package bandaid

import cats.{ ApplicativeError, MonadError }

import scala.annotation.unchecked.uncheckedVariance

package object ce2 { // scalastyle:ignore package.object.name

  type ApplicativeThrow[F[_]] = ApplicativeError[F, Throwable]
  type MonadThrow[F[_]]       = MonadError[F, Throwable]

  private[ce2] type =>?[-A, +B] = PartialFunction[A, B]

  private[ce2] type Inner[F[_], -I, +E, +O] = I => F[Either[E, O] @uncheckedVariance]

  implicit private[ce2] class InnerWrap[F[_], -I, +E, +O](private val inner: Inner[F, I, E, O]) extends AnyVal {
    def wrap: ZUO[F, I, E, O] = new ZUO(inner)
  }

  type IO[F[_], +O]      = ZUO[F, Any, Nothing, O] // Succeed with an `O`, might throw                , no requirements.
  type BIO[F[_], +E, +O] = ZUO[F, Any, E, O] //       Succeed with an `O`, may fail with `E` or throw , no requirements.
  type DIO[F[_], -I, +O] = ZUO[F, I, Nothing, O] //   Succeed with an `O`, might throw                , requires an `I`.
}

package bandaid

import cats.{ ApplicativeError, MonadError }

import scala.annotation.unchecked.uncheckedVariance

package object ce2 { // scalastyle:ignore package.object.name

  type ApplicativeThrow[F[_]] = ApplicativeError[F, Throwable]
  type MonadThrow[F[_]]       = MonadError[F, Throwable]

  type =>?[-A, +B] = PartialFunction[A, B]
}

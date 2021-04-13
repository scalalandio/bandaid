package bandaid.ce2

import cats.effect.{ ApplicativeThrow, MonadThrow }
import cats.implicits._

import scala.util.control.ControlThrowable

private[ce2] final class PassError[E](tag: java.util.UUID) { passed =>

  def apply(error: E): Throwable = PassError.OpaqueError(error, tag)

  def unapply(throwable: Throwable): Option[E] = throwable match {
    case PassError.OpaqueError(error, `tag`) => Some(error.asInstanceOf[E])
    case _                                   => None
  }

  def hide[F[_]: MonadThrow, A](fea: F[Either[E, A]]): F[A] = fea.flatMap {
    case Left(error)  => passed(error).raiseError[F, A]
    case Right(value) => value.pure[F]
  }

  def show[F[_]: ApplicativeThrow, A](fa: F[A]): F[Either[E, A]] =
    fa.map(_.asRight[E]).recover { case passed(error) => error.asLeft[A] }
}
private[ce2] object PassError {

  private final case class OpaqueError(error: Any, tag: java.util.UUID) extends ControlThrowable("This error should not be caught")

  def in[E, A](passed: PassError[E] => A): A = passed(new PassError[E](java.util.UUID.randomUUID()))
}

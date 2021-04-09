package bandaid.ce2

import cats.data.{ EitherT, Kleisli }
import cats.effect.Sync
import cats.{ ~>, Applicative, Defer, Functor, Monad }

/**
  * Inspired by ZIO (like whole bandaid, really).
  *
  * Intended to use like:
  * {{{
  * // my/domain/io/package.scala
  * package my.domain
  * package object io extends bandaid.ce.ZUOCompanion[cats.effect.IO] {
  *   implicit val csio:             ContextShift[cats.effect.IO] = cats.effect.IO.contextShift(ExecutionContext.global)
  *   implicit val concurrentEffect: ConcurrentEffect[effect.IO]  = cats.effect.IO.ioConcurrentEffect
 *  }
  * }}}
  *
  * @tparam F the IO monad
  */
abstract class ZUOCompanion[F[_]] extends ZUOAliases[F] {

  object ZUO {

    def pure[O](value:   O)(implicit F: Applicative[F]): IO[O] = bandaid.ce2.ZUO.pure[F](value)
    def unit(implicit F: Applicative[F]): IO[Unit] = bandaid.ce2.ZUO.unit[F]

    def raiseError[E](error: E)(implicit F: Applicative[F]): BIO[E, Nothing] = bandaid.ce2.ZUO.raiseError[F](error)

    def raiseException(throwable: Throwable)(implicit F: ApplicativeThrow[F]): IO[Nothing] = bandaid.ce2.ZUO.raiseException[F](throwable)

    def defer[I, E, O](zuo: => ZUO[I, E, O])(implicit F: Monad[F], defer: Defer[F]): ZUO[I, E, O] = bandaid.ce2.ZUO.defer[F](zuo)
    def delay[O](o:         => O)(implicit F:            Sync[F]): IO[O] = bandaid.ce2.ZUO.delay[F](o)

    def lift[I, E, O](f: I => F[Either[E, O]]): ZUO[I, E, O] = bandaid.ce2.ZUO.lift(f)

    def fromCats[I, E, O](f: Kleisli[EitherT[F, E, *], I, O]): ZUO[I, E, O] = bandaid.ce2.ZUO.fromCats(f)

    def fromEither[E, O](either: Either[E, O])(implicit F: Applicative[F]): BIO[E, O] = bandaid.ce2.ZUO.fromEither(either)

    def liftF[O](fo: F[O])(implicit F: Functor[F]): IO[O] = bandaid.ce2.ZUO.liftF[F](fo)

    def liftK(implicit F: Functor[F]): F ~> IO = bandaid.ce2.ZUO.liftK[F]
  }

  // TODO: objects accompanying aliases IO, BIO, DIO
}

package bandaid.ce2

/**
  * Inspired by ZIO (like whole bandaid, really).
  *
  * Intended to use like:
  * {{{
  * // my/domain/io/package.scala
  * package my.domain
  * package object io extends bandaid.ce.ZUOCompanion[cats.effect.IO]
  * }}}
  *
  * @tparam F the IO monad
  */
trait ZUOCompanion[F[_]] {

  // overshades ZUO and fixes F
  type ZUO[-I, +E, +O] = bandaid.ce2.ZUO[F, I, E, O]

  // copy-paste-edit of ZIO aliases

  type IO[+E, +O]   = ZUO[Any, E, O] //         Succeed with an `O`, may fail with `E`        , no requirements.
  type Task[+O]     = ZUO[Any, Throwable, O] // Succeed with an `O`, may fail with `Throwable`, no requirements.
  type RIO[-I, +O]  = ZUO[I, Throwable, O] //   Succeed with an `O`, may fail with `Throwable`, requires an `I`.
  type UIO[+O]      = ZUO[Any, Nothing, O] //   Succeed with an `O`, cannot fail              , no requirements.
  type URIO[-I, +O] = ZUO[I, Nothing, O] //     Succeed with an `O`, cannot fail              , requires an `I`.

  // TODO: objects accompanying aliases
}

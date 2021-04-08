package bandaid.ce2

/**
  * Inspired by ZIO (like whole bandaid really).
  *
  * Intended to use like:
  * {{{
  * // my/domain/io/package.scala
  * package my.domain
  * package object io extends bandaid.ce.AliasesAndUtils[cats.effect.IO]
  * }}}
  *
  * @tparam F the IO monad
  */
trait AliasesAndUtils[F[_]] {

  // overshades IEO and fixes F
  type IEO[-I, +E, +O] = bandaid.ce2.IEO[F, I, E, O]

  // copy-paste-edit of ZIO aliases

  type IO[+E, +O]   = IEO[Any, E, O] // Succeed with an `O`, may fail with `E`        , no requirements.
  type Task[+O]     = IEO[Any, Throwable, O] // Succeed with an `O`, may fail with `Throwable`, no requirements.
  type RIO[-I, +O]  = IEO[I, Throwable, O] // Succeed with an `O`, may fail with `Throwable`, requires an `I`.
  type UIO[+O]      = IEO[Any, Nothing, O] // Succeed with an `O`, cannot fail              , no requirements.
  type URIO[-I, +O] = IEO[I, Nothing, O] // Succeed with an `O`, cannot fail              , requires an `I`.

  // TODO: objects accompanying aliases
}

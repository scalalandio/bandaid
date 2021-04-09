package bandaid.ce2

/**
  * Provides useful aliases and pinning your F.
  * @tparam F the IO monad
  */
trait ZUOAliases[F[_]] extends Any {

  type ZUO[-I, +E, +O] = bandaid.ce2.ZUO[F, I, E, O] // overshades ZUO and fixes F for convenience

  type IO[+O]      = ZUO[Any, Nothing, O] // Succeed with an `O`, might throw                , no requirements.
  type BIO[+E, +O] = ZUO[Any, E, O] //       Succeed with an `O`, may fail with `E` or throw , no requirements.
  type DIO[-I, +O] = ZUO[I, Nothing, O] //   Succeed with an `O`, might throw                , requires an `I`.
}

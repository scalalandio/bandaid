package bandaid.ce2

/**
  * Provides useful aliases and pinning your F.
  * @tparam F the IO monad
  */
trait ZUOAliases[F[_]] extends Any {

  type ZUO[-I, +E, +O] = bandaid.ce2.ZUO[F, I, E, O] // overshades ZUO and fixes F for convenience
  type IO[+O]          = bandaid.ce2.IO[F, O] //        Succeed with an `O`, might throw                , no requirements.
  type BIO[+E, +O]     = bandaid.ce2.BIO[F, E, O] //    Succeed with an `O`, may fail with `E` or throw , no requirements.
  type DIO[-I, +O]     = bandaid.ce2.DIO[F, I, O] //    Succeed with an `O`, might throw                , requires an `I`.
}

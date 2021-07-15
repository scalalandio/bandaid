package bandaid

package object ce2 { // scalastyle:ignore package.name

  type =>?[-A, +B] = PartialFunction[A, B]

  type IO[F[_], +O]      = ZUO[F, Any, Nothing, O] // Succeed with an `O`, might throw                , no requirements.
  type BIO[F[_], +E, +O] = ZUO[F, Any, E, O] //       Succeed with an `O`, may fail with `E` or throw , no requirements.
  type DIO[F[_], -I, +O] = ZUO[F, I, Nothing, O] //   Succeed with an `O`, might throw                , requires an `I`.
}

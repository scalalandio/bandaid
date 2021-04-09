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
trait ZUOCompanion[F[_]] extends ZUOAliases[F] {

  // TODO: objects accompanying aliases
}

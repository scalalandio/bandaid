package bandaid

import cats.effect
import cats.effect.{ ConcurrentEffect, ContextShift }

import scala.concurrent.ExecutionContext

package object iotest extends bandaid.ce2.ZUOCompanion[cats.effect.IO] {

  implicit val csio:             ContextShift[cats.effect.IO] = cats.effect.IO.contextShift(ExecutionContext.global)
  implicit val concurrentEffect: ConcurrentEffect[effect.IO]  = cats.effect.IO.ioConcurrentEffect
}

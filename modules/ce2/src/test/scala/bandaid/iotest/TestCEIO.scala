package bandaid.iotest

import cats.effect.ContextShift

class TestCEIO(implicit cs: ContextShift[cats.effect.IO]) {

  val result: ZUO[Int, String, Int] = for {
    o1 <- ZUO.delay(2 + 2)
    _ <- ZUO.raiseError("failure").handleSomeError(_ => "another")
    o2 <- ZUO.lift((i: Int) => cats.effect.IO(Right(i)))
  } yield o1 + o2
}

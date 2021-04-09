package bandaid.iotest

import cats.effect.{ ContextShift, Sync }

class TestCEIO(implicit sync: Sync[cats.effect.IO], contextShift: ContextShift[cats.effect.IO]) {

  val result: ZUO[Int, String, Int] = for {
    o1 <- ZUO.delay(2 + 2)
    _ <- ZUO.raiseError("failure").handleSomeError(_ => "another")
    o2 <- ZUO.liftDI((i: Int) => i)
  } yield o1 + o2
}

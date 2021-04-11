package bandaid.iotest

import bandaid.ce2.DI
import bandaid.iotest.TestCEIO.{ ProvideDatabase, ProvideEventBus }
import cats.effect.{ ContextShift, Sync }

class TestCEIO(implicit sync: Sync[cats.effect.IO], contextShift: ContextShift[cats.effect.IO]) {

  val result: ZUO[Int, String, Int] = for {
    o1 <- ZUO.delay(2 + 2)
    _ <- ZUO.raiseError("failure").handleSomeError(_ => "another")
    o2 <- ZUO.liftFunction((i: Int) => i)
  } yield o1 + o2

  val diTest: ZUO[DI[ProvideEventBus] with DI[ProvideDatabase], Nothing, Unit] = for {
    _ <- ZUO.liftFunction(DI.lift { db: ProvideDatabase => () })
    _ <- ZUO.liftFunction(DI.lift { eb: ProvideEventBus => () })
  } yield ()

  // requires manual intervention :/
  diTest.provideDI[DI[ProvideDatabase], DI[ProvideEventBus]](DI(new ProvideDatabase { def db: String = "database" }))

  // not optimal but at least inference works
  val layerTest   = ZUO.liftFunction(DI.lift((_: ProvideDatabase) => new ProvideEventBus { def eventBus: String = "eventbus" })).asDI
  val provideTest = diTest.provideDIFrom(layerTest)
}
object TestCEIO {

  trait ProvideDatabase { def db:       String }
  trait ProvideEventBus { def eventBus: String }
}

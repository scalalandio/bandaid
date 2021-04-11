package bandaid.ce2

import scala.reflect.runtime.universe._

/**
  * Shameless ripoff of a Has concept from ZIO.
  * @param map collection of all values that we can DI, its content is traced with `TypeSet`
  * @tparam Tpe type of injected dependency - remember that the whole type would be `DI[A] with DI[B] ...` listing all dependencies in `map`
  */
final class DI[Tpe] private (private val map: Map[TypeTag[_], Any]) {

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def get[Out >: Tpe: TypeTag]: Out = map(typeTag[Out]).asInstanceOf[Out]
}
object DI {

  def apply[Tpe: TypeTag](value: Tpe): DI[Tpe] = new DI(Map(typeTag[Tpe] -> value))

  /** Wraps function's input with DI. */
  def lift[Tpe: TypeTag, Out](f: Tpe => Out): DI[Tpe] => Out = di => f(di.get[Tpe])

  implicit class DIOps[D1 <: DI[_]](private val d1: D1) extends AnyVal {

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def ++[D2 <: DI[_]](d2: D2): D1 with D2 = new DI(d1.map ++ d2.map).asInstanceOf[D1 with D2] // scalastyle:ignore method.name
  }
}

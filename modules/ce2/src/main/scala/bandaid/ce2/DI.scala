package bandaid.ce2

import scala.reflect.runtime.universe._

/**
  * Shameless ripoff of a Has concept from ZIO.
  * @param map collection of all values that we can DI, its content is traced with `TypeSet`
  * @tparam TypeSet type-level set of types (e.g. `Postgres with Kafka with S3`) that traces what kind of values are stored in `map`
  */
@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
final class DI[TypeSet] private (private val map: Map[TypeTag[_], Any]) extends AnyVal {

  def get[Tpe >: TypeSet: TypeTag]: Tpe = map(typeTag[Tpe]).asInstanceOf[Tpe]

  def ++[D <: DI[_]](di: D): DI[TypeSet] with D = // scalastyle:ignore method.name
    new DI(map ++ di.map).asInstanceOf[DI[TypeSet] with D]
}
object DI {

  def apply[Tpe: TypeTag](value: Tpe): DI[Tpe] = new DI(Map(typeTag[Tpe] -> value))

  def lift[Tpe: TypeTag, Out](f: Tpe => Out): DI[Tpe] => Out = di => f(di.get[Tpe])
}

import com.gensler.scalavro.types._
import scala.tools.reflect.ToolBox

object MyApp extends App {

  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{ currentMirror => cm }
  import scala.reflect._
  import scala.reflect.api._

  def cdef() = q"case class C(v: String)"
  val tb = cm.mkToolBox()
  val csym = tb.define(cdef())
  def newc(csym: Symbol) = q"""new ${csym}("hi")"""
  val name = tq"""${newTypeName("C")}"""

  val obj = tb.eval(newc(csym))

  // adapted from http://stackoverflow.com/questions/22970209/get-typetaga-from-classa/22972751#22972751
  val mirror = runtimeMirror(obj.getClass.getClassLoader) // obtain runtime mirror
  val sym = mirror.staticClass(obj.getClass.getName) // obtain class symbol for `c`
  val tpe = sym.selfType // obtain type object for `c`
  // create a type tag which contains above type object
  val tt = TypeTag(mirror, new TypeCreator {
    def apply[U <: Universe with Singleton](m: api.Mirror[U]) =
      if (m eq mirror) tpe.asInstanceOf[U#Type]
      else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
  })
  println(tt)

  val method_apply$ = obj.getClass.getMethod("apply", "".getClass)
  val instantiated$ = method_apply$.invoke(obj, "hello")
  type Type = instantiated$.type //value as a type

  println(AvroType[Type](tb, csym).schema)

}

import com.gensler.scalavro.types._
import scala.tools.reflect.ToolBox

object Example extends App {

  val typeProvider = new ToolBoxTypeProvider
  import typeProvider.runtime._

  val avroType = AvroType[TYPE](typeProvider.tb)
  val avroSchema = avroType.schema
  val serialized = avroType.io(typeProvider.tb).writeJson(typeProvider.runtimeObj.asInstanceOf[TYPE])
  val deserialized = avroType.io(typeProvider.tb).readJson(serialized)

  println("schema: " + avroSchema)
  println("serialize: " + serialized)
  println("deserialize: " + deserialized)

}

class ToolBoxTypeProvider {

  import scala.reflect.runtime.{ currentMirror => cm }
  import scala.reflect.runtime.universe._
  import scala.reflect._
  import scala.reflect.api._

  val tb = cm.mkToolBox()
  def cdef() = q"case class C(v: String)"
  val csym: reflect.runtime.universe.Symbol = tb.define(cdef())
  def newc(csym: Symbol) = q"""new ${csym.asClass}("hello")"""
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

  class TypeCheckDummy {
    type TYPE = AnyRef
    implicit val tag: TypeTag[TYPE] = tt.asInstanceOf[TypeTag[TYPE]]
    implicit val manifest: Manifest[TYPE] = Manifest.classType(obj.getClass)
  }

  val runtime = new TypeCheckDummy;

  //Also offer an instance that was generated from the new class, but using scala reflection
  val classMirror = mirror.reflectClass(sym)
  val ctor = tpe.decl(termNames.CONSTRUCTOR).asMethod
  val ctorMirror = classMirror.reflectConstructor(ctor)
  val runtimeObj = ctorMirror("Instantiating a Type at Runtime")

}

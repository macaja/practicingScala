import shapeless._

case class Employee(name: String, age: Int, enable: Boolean)
case class IceCream(typeOf:String,id: Int, enable: Boolean )

val genericEmployee = Generic[Employee].to(Employee("mauricio",20,true))
val genericIceCream = Generic[IceCream].to(IceCream("oreo",1,true))

def genericCsv(gen: String :: Int :: Boolean :: HNil): List[String] = {
  List(gen(0),gen(1).toString,gen(2).toString)
}

val gEmployee = genericCsv(genericEmployee)
val gIceCream = genericCsv(genericIceCream)
/*Reto
case class ADT(sgffsg:Int,sgs:Int,sggsf:Int,sgfgs:Int,sfggfgs:Int,sgfsg:Int,sfgfg:Int,sdfgfg:Int,sdgfsdg:Int,adfdsaf:Int,afsdaf:Int,asfd:Int,afsd:Int,asfd:Int,safd:Int,as:Int)
val adt = ADT(1,2,3,4,5,6,76,7,78,8,8,9,9,90,6,7)
val r = ADT.unapply(adt)

Crear un tipo F[A]{ def f[A]: String => G[A] } tal que:
assert( F[ADT](r) == G(adt) )
G[_] puede ser ID, Future, etc. (a convenir)
*/


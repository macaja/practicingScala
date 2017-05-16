import akka.util.ByteString
import cats.data.NonEmptyList

//Funcion parcialmente aplicada
def log(a:String,b:String): String = a+".."+b
val mauricio = "Mauricio"
val fpa = log(mauricio, _ : String)
fpa("bacano")
val fpa2 = log _
fpa2(mauricio,"bacano")

// Currying
def multiplicar(x: Int,y: Int) = x * y

val multiplicarCurried: (Int) => (Int) => Int = (multiplicar _).curried

multiplicar(2,3)
multiplicarCurried(2)(3)

val mCurriedUno = multiplicarCurried(2)
mCurriedUno(3)

//Funciones parciales
val doubleEvens: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {
  //States that this partial function will take on the task
  def isDefinedAt(x: Int): Boolean = x % 2 == 0

  //What we do if this partial function matches
  def apply(v1: Int): Int = v1 * 2
}
val tripleOdds: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {
  def isDefinedAt(x: Int) = x % 2 != 0

  def apply(v1: Int) = v1 * 3
}
def sumarCinco = (x:Int) => x+ 5

val whatToDo: PartialFunction[Int, Int] = doubleEvens orElse tripleOdds andThen sumarCinco
whatToDo(3)
whatToDo(2)

val fraction: PartialFunction[Int,Int] = {
  case d: Int if d%2 ==0 => 42 / d
}
val frac = fraction(4)
val fraction2: PartialFunction[Int,Int] = new PartialFunction[Int,Int]{
  def isDefinedAt(x: Int) = x % 2 == 0
  def apply(x: Int) = x * 2
}
val f2 = fraction2.isDefinedAt(5)

//lazy evaluation

def mult(a: Int,b: =>Int) = a *a
def loop(x: Int): Int = loop(x)

println(mult(2,loop(2)))

implicit val nombre = "Mauricio"
def getNombre(implicit n:String): String = {
   n
}
println(getNombre)

val jsonResponse = ByteString(
  s"""
     |{
     | "id": 1,
     |  "name": "william",
     |  "age": 25,
     |  "gender": "H"
     |}
           """.stripMargin)

val twice: Int => Int =
  x => x * 2

val countCats: Int => String =
  x => if (x == 1) "1 cat" else s"$x cats"

val twiceAsManyCats: Int => String =
  twice andThen countCats // equivalent to: countCats compose twice

twiceAsManyCats(1)

sealed trait Discount
case class Percentage(i: Int) extends Discount
case class Value(v: Double) extends Discount

object Discount{
  def apply(p: Int): Discount = Percentage(p)

  def apply(v: Double): Discount = Value(v)
}

val percentage = Discount(10)
val value = Discount(10.3)

case class Pro(id: Int, num: Int)


implicit val dos:Int =  2
//implicit val tres:Int =  3 //genera ambiguedad en el implicit de la funcion sumar

def sumar(implicit x: Int) = x + x

val r = sumar

sealed trait Error extends Product with Serializable{
  val message: String
}
case class Invalid(message: String) extends Error

NonEmptyList

def ensayo(a:Int,b:String,c:Int): Option[(Int, String, Int)] = {
  Option(
    (a,b,c)
  )
}
def devuelve(a:Int,b:String,c:Int): Option[String] = {
  Option(
  s"a es: $a , b es: $b y c es: $c"
  )
}
val s = for{
  (a,b,c)<- ensayo(1,"hola",2)
  r <- devuelve(a,b,c)
}yield r

println(s)

val q = Option(Option(1))
val rr = q.map(o => o.map(i => 2))

val qw = List(1,2,3)
val qw1 = qw::1::2::3::Nil

def sumFive(option: Option[Int]): Int = {
  option.fold(0){
    n => n + 5
  }
}

val ds = Option(Option(Option(5)))
val re = ds.flatMap(e => Some(5))

val sss = sumFive(Option(4))

//// Suma de elementos de una lista usando _*
def sum(args: Int*) = args.sum

val rst = sum(1)
val rste = sum(1,2,3)

val list = List(1,2,3,4)
val sumlist = sum(list: _*)

case class Item(itenNUmber: String,variants:Map[String,String],itemType:String)

case class Variant(key:String, value:String)

val item = Item("1",Map("1" -> "2"),"coffee")

val m = item.variants.map(a => Variant(a._1,a._2)).toSeq

case class Merchant(id: String, itemNumbers: Set[String])


val merchant1: Set[Merchant] = Set(Merchant("1",Set("10","20","30")),Merchant("2",Set("40","50","60")),Merchant("3",Set("70","80","90")))

val rft: Map[String, Set[String]] = merchant1.groupBy(_.id).mapValues(_.flatMap(_.itemNumbers))

val res: Set[String] = Set("1","2","3","4")
val rtos = res.toString()


case class ProductVarian(key: String, value:String)

val l = Seq(ProductVarian("1","jajaj"))

val lmap = Map(l map(a => a.key -> a.value): _*)
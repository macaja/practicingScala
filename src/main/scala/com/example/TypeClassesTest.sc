import JsonWriterInstances._
import JsonSyntax._

import PrintableFormatInstances._
import PrintSyntax._

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends  Json
final case class JsInt(get: Int) extends Json

trait JsonWriter[A]{
  def write(value: A) : Json
}

final case class Person(name: String, email:String)

object JsonWriterInstances {
  implicit val stringJsonWriter = new JsonWriter[String] {
    def write(value: String): Json =
      JsString(value)
  }
  implicit val personJsonWriter = new JsonWriter[Person] {
    def write(value: Person): Json =
      JsObject(Map("name" -> JsString(value.name),
        "email" -> JsString(value.email)
      ))
  }
  implicit val intJsonWriter = new JsonWriter[Int] {
    def write(value: Int): Json = {
      JsInt(value)
    }
  }
}

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

Json.toJson(Person("Mauricio","cardona@gmail.com"))
Json.toJson("Mauricio")
Json.toJson(15)

Person("Mauricio","a@g.com").toJson
"Mauricio".toJson

trait Printable[A]{
  def format(value: A):String
}

final case class Cat(name: String, age: Int, color:String)

object PrintableFormatInstances{
  implicit val stringToPrintable = new Printable[String] {
    def format(value: String):String=value
  }
  implicit val intToPrintable = new Printable[Int] {
    def format(value: Int):String = value.toString
  }
  implicit val catToPrintable = new Printable[Cat] {
    def format(value: Cat): String = value.name + " is a "+ value.age.formatString + " year-old " + value.color + " cat."
  }
}

object PrintSyntax{
  implicit class PrintFormatOps[A](value: A){
    def formatString(implicit p: Printable[A]): String = {
      p.format(value)
    }
    def print(implicit p: Printable[A]): Unit = {
      println(formatString)
    }
  }
}

val cadena = "Mauricio".formatString
val entero = 1.formatString
val cat = Cat("tom",15,"white and black")

val formated = cat.formatString


case class Merchant(id: String, itemNumbers: Set[String])

val merchant1: Set[Merchant] = Set(Merchant("1",Set("10","20","30")),Merchant("2",Set("40","50","60")),Merchant("3",Set("70","80","90")))

val rft: Map[String, Set[String]] = merchant1.groupBy(_.id).mapValues(_.flatMap(_.itemNumbers))

val r: Set[String] = Set("1","2","3","4")
val rtos = r.toString()


case class ProductVarian(key: String, value:String)

val l = Seq(ProductVarian("1","jajaj"))

val lmap = Map(l map(a => a.key -> a.value): _*)


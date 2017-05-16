//ADVANCED SCALA BOOK
//First Example of Type Classes, the same of the book
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

import JsonWriterInstances._
import JsonSyntax._

Person("Mauricio","a@g.com").toJson
"Mauricio".toJson

////Another example of TypeClasses

trait Printable[A]{
  def format(value: A):String
}

final case class Cat(name: String, age: Int, color:String)

object PrintableFormatInstances extends PrintSyntax{
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

trait PrintSyntax{
  implicit class PrintFormatOps[A](value: A){
    def formatString(implicit p: Printable[A]): String = {
      p.format(value)
    }
    def print(implicit p: Printable[A]): Unit = {
      println(formatString)
    }
  }
}
import PrintableFormatInstances._


val cadena = "Mauricio".formatString
val entero = 1.formatString
val cat = Cat("tom",15,"white and black")

val formated = cat.formatString

//With cats
import cats.instances.int._
import cats.instances.boolean._
import cats.syntax.show._
import cats.Show

case class MyClass(a: Int, b:String, c:Boolean)

implicit val myclassToShow: Show[MyClass] =
  Show.show(r => s"MyClass es MyClass("+r.a.show+","+r.b+","+r.c.show+")")

val myclass = MyClass(1,"mauricio",true).show
val r = 123.show
val boo = true.show

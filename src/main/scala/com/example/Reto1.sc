
case class ADT1(v1: String, v2: String)
case class ADT2(v1:String,v2:String)

trait ADTTransformer[A,B]{
  def convert(value: A): B
}

object ADTTransformerInstances extends ADTOpsSyntax{
  implicit val tuple2ToADT = new ADTTransformer[(String,String),ADT1] {
    def convert(t:(String,String)) = ADT1(t._1,t._2)
  }

  implicit val tuple2ToADT2 = new ADTTransformer[(String,String),ADT2] {
    def convert(t:(String,String)) = ADT2(t._1,t._2)
  }
}

trait ADTOpsSyntax{
  implicit class toADT[A](value:A){
    def toADT[B](implicit adtT:ADTTransformer[A,B]):B = {
      adtT.convert(value)
    }
  }
}

import ADTTransformerInstances._

val r:ADT1 = ("1","2").toADT[ADT1]
val r2:ADT2 = ("1","2").toADT[ADT2]


val nom = "".split(" ",2)


val lista: List[String] = List("happy","hour")
val lis = lista
val string: String = lista.foldLeft(" ")(_ + _)


val bool = "".isEmpty

val opt = Option(2)
val fo = opt.fold("Fue un none")(i=> i.toString)


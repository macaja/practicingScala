package com.example.roulette


import scala.util.Random

object RouletteMain extends App {

  case class Result(tabValue: Int, cash: Int)

  def randomNumber: Int = Random.nextInt(37)

  implicit class IntOptionConverter(val a: Int) extends AnyVal{
    def toOption = if(a == 0) None else Some(a)
  }

  def result(tabValue:Int,myRecords:List[List[Int]],number: Int): Int =
    myRecords.count(bet => bet.contains(number)).toOption.fold(-(myRecords.size * tabValue))(s => s * (tabValue * 9))

  def foldW(bet: List[List[Int]],cash: Int,tabValue:Int, number: Int): Result = {
    val r = result(tabValue,bet,number)
    val finalCash = cash + r
    if(r > 0) Result(tabValue * 2,finalCash) else Result(100,finalCash)
  }

  def foldL(bet: List[List[Int]],cash: Int,tabValue:Int, number: Int): Result = {
    val r = result(tabValue,bet,number)
    val finalCash = cash + r
    if(r < 0) Result(tabValue * 2,finalCash) else Result(tabValue,finalCash)
  }

  def play(cash: Int) = {
    val myRecords:List[List[Int]] = List(List(1,2,4,5),List(2,3,5,6),List(4,5,7,8),List(5,6,8,9),List(7,8,10,11),List(8,9,11,12))

    var finalCash = cash
    var tabValue = 100
    var a = 1
    while(a <= 100){
      val number = randomNumber
      if(finalCash > 0){
        if(finalCash < 60000){
          val f = foldW(myRecords,finalCash,tabValue, number)
          tabValue = f.tabValue
          finalCash = f.cash
          println(s"W ---  Intento # $a ---- numero !! $number  !! ---- valor de ficha => $tabValue ----- dinero => $finalCash ")
        }else{
          val f = foldL(myRecords,finalCash,tabValue, number)
          tabValue = f.tabValue
          finalCash = f.cash
          println(s"L ---  Intento # $a ---- numero !! $number  !! ---- valor de ficha => $tabValue ----- dinero => $finalCash ")
        }
      }else{
        println(s"Perdiste en el intento # => $a")
        a = 100
      }
      a = a + 1
    }

    println(s"quedaste con ==> $finalCash")
  }
  play(20000)
}

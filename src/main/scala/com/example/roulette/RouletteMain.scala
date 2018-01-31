package com.example.roulette


import scala.collection.mutable.ListBuffer
import scala.util.Random

object RouletteMain extends App {

  def randomNumber: Int = Random.nextInt(37)

  def result(valueByRecord:Int,myRecords:List[List[Int]],number: Int) : Int =
    myRecords.count(bet => bet.contains(number)) match {
      case 0 => -(myRecords.size * valueByRecord)
      case s => s * (valueByRecord * 9)
    }

  def startFoldingWhenWin(bet:List[List[Int]],cash:Int, tabValueAtFirst:Int, retries:Int): Int = {
    var finalCash = cash
    var tabValue = tabValueAtFirst
    var a = 1
    while(a <= retries){
      val numberThatFell = randomNumber
      val r = result(tabValue,bet,numberThatFell)
      if(r > 0){
        tabValue = tabValue * 2
      }else tabValue = 100
      finalCash = finalCash + r
      if(finalCash < 0){
        println(s"Perdio en el intento $a")
        a = 101
      }else{
        println(s"Intento # $a ----> cayó el número => $numberThatFell  => dinero =>    $finalCash    valor de la ficha => $tabValue")
        a = a + 1
      }
    }
    finalCash
  }

  def startFoldingBetWhenLost(bet:List[List[Int]],cash:Int, tabValueAtFirst:Int, retries:Int): Int = {
    var finalCash = cash
    var tabValue = tabValueAtFirst
    var a = 1
    while(a <= retries){
      val numberThatFell = randomNumber
      val r = result(tabValue,bet,numberThatFell)
      if(r < 0){
        tabValue = tabValue * 2
      }else tabValue = tabValue
      finalCash = finalCash + r
      if(finalCash < 0){
        println(s"Perdio en el intento $a => valor de la ficha => $tabValue")
        a = 101
      }else{
        println(s"Intento # $a  cayo el numero $numberThatFell cash ----> $finalCash => valor de la ficha => $tabValue")
        a = a + 1
      }
    }
    finalCash
  }

  def greater(f:Int,s:Int):Int = if(f > s) f else s

  def play = {
    val cashToBet = 20000
    val tabValue = 200
    val myRecords:List[List[Int]] = List(List(1,2,4,5),List(2,3,5,6),List(4,5,7,8),List(5,6,8,9),List(7,8,10,11),List(8,9,11,12))
    val s = startFoldingWhenWin(myRecords,cashToBet,tabValue,100)

    //val t = startFoldingBetWhenLost(myRecords, cashToBet, tabValue, 100)
    println(s"quedaste con ==> $s")
  }
  play
}

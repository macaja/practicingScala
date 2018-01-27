import cats.Monoid
import cats.kernel.Semigroup

import scala.util.Random

val un = Monoid[String].combine("Hola"," Mauricio")
val em = Monoid[String].empty

val stringResult = "Hola" |+| "Mauricio" |+| Monoid[String].empty

def add(list: List[Int]): Int = {
  list.foldLeft(0)(_ + _)
}

val lista:List[Int] = List(1,2,3,4,5,6,7,8,90)
val listaAdd = add(lista)

import cats.implicits._

Semigroup[Int].combine(1,2)

Semigroup[Int].combine(1, 2)
Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6))
Semigroup[Option[Int]].combine(Option(1), Option(2))
Semigroup[Option[Int]].combine(Option(1), None)
Semigroup[Int ⇒ Int]
  .combine({ (x: Int) ⇒
    x + 1
  }, { (x: Int) ⇒
    x * 10
  })
  .apply(6)

Random.nextInt(37)

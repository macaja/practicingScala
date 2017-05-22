//Informally a functor is anything with map method

val op: Option[Int] = Option(2).map(i => i *2)

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
val fu: Future[Int] = Future(2).map(i => i * 2)


/*Formally a functor is a type F[A] with an operation
  * map A=> B that return a type F[B]
  **/

import cats.instances.function._
import cats.syntax.functor._

val f = (x: Int) => x.toDouble
val g = (y: Double) => y.toString
val h = f.map(g)

h(1) //function composition by calling map
g(f(1)) //function composition written out by hand

val fuf = (x: Int) => x *2
val f2 = fuf andThen fuf
val r = f2(5)

//With Type CLasses

sealed trait Tree[+A]
final case class Branch[A](left:Tree[A],right:Tree[A])extends Tree[A]
final case class Leaf[A](value:A) extends Tree[A]

trait Functor[F[_]]{
  def mapF[A,B](fa:F[A])(f:A => B): F[B]
}

object FunctorInstances extends FunctorSyntax{

  implicit val optionFunctor = new Functor[Option] {
    override def mapF[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa.map(f)
  }
  implicit val treeFunctor = new Functor[Tree] {
    override def mapF[A, B](fa: Tree[A])(function: (A) => B): Tree[B] = fa match {
      case Branch(left,right) => Branch(mapF(left)(function),mapF(right)(function))
      case Leaf(a) => Leaf(function(a))
    }
  }
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

trait FunctorSyntax{
  implicit class bla[F[_],A](m: F[A]){
    def mapWith[B](f: A ⇒ B)(implicit functor: Functor[F]): F[B]= functor.mapF(m)(f)
  }
}

import FunctorInstances._
val use = Option(2) mapWith(_.toString)
val useTree = branch(leaf(10),branch(leaf(15),leaf(20))).mapWith(_.toString)

//With Cats

import cats.Functor
import cats.instances.list._
import cats.instances.option._

val list1 = List(1,2,4)
val list2 = Functor[List].map(list1)(_ * 3)
val option1 = Functor[Option].map(Option(2))(_ * 3)


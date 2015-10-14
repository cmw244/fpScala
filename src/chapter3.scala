package clen.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
 
  def mkString[A](lst: List[A]): String = {
    @tailrec
    def mkStringHelper[A](l: List[A], acc: String): String = l match {
     case Nil => acc.dropRight(1) // Remove trailing ','
     case Cons(hd,tl) => mkStringHelper(tl, acc + hd.toString + ",")
    }
    mkStringHelper(lst,"")
  }

  // Exercise 2
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil 
    case Cons(hd,tl) => tl
  }

  // Exercise 3
  def setHead[A](item: A, lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(hd,tl) => Cons(item, tl)
  }

  // Exercise 4.  Note: does not handle case where n is larger than l.length
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case x => drop(tail(l), n-1)
  }

  // Exercise 5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(hd, tl) if f(hd) => dropWhile(tl,f)
    case _ => l
  }

  // Exercise 6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,Nil) => Nil
    case Cons(hd,tl) => Cons(hd,init(tl))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // Exercise 9
  def length[A](l: List[A]): Int = foldRight(l,0)((_,b) => b+1)

  // Exercise 10
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(hd,tl) => foldLeft(tl, f(z,hd))(f)
  }

  // Exercise 11
  def sumFL(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def productFL(l: List[Int]) = foldLeft(l, 1)(_ * _)
}

object Main extends App {
  import List._
  // Exercise 2
  println("2,3,4,5,6 -> " + List.mkString(List.tail(List(1,2,3,4,5,6))))
  // Exercise 3
  println("3,2,3 -> " + List.mkString(List.setHead(3, List(1,2,3))))
  // Exercise 4
  println("4,5,6 -> " + List.mkString(List.drop(List(1,2,3,4,5,6), 3)))
  // Exercise 5
  println("4,5,6 -> " + List.mkString(List.dropWhile(List(1,2,3,4,5,6), (a:Int) => a < 4)))
  // Exercise 6
  println("1,2,3 -> " + List.mkString(List.init(List(1,2,3,4))))
  // Exercise 9
  println("6 -> " + List.length(List(1,2,3,4,5,6)))

  val lst = List(1,2,3,4,5)
  println("15 -> " + List.sumFL(lst))
  println("120 -> " + List.productFL(lst))

}

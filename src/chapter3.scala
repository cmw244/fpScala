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

  // Exercise 12
  def reverse[A](l: List[A]): List[A] = foldLeft(l,List[A]())((a,b) => Cons(b,a)) 

  // Exercise 13
  def foldLeftUsingRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (b:B) => b)((a,c) => b => c(f(b,a)))(z)
  def foldRightUsingLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a,b) => f(b,a))

  // Exercise 14
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_,_))
  
  // Exercise 15
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  // Exercise 16
  def addOne(l: List[Int]): List[Int] = {
    @tailrec
    def addOneHelper(l: List[Int], acc: List[Int]): List[Int] = l match {
      case Nil => acc
      case Cons(hd,tl) => addOneHelper(tl, Cons(hd+1, acc))
    }
    addOneHelper(l, Nil)
  }
  // Using fold
  // foldRight(l, List[Int]())((h,t) => Cons(t+1, h))

  // Exercise 17
  def doubleToString(l: List[Double]): List[String] = {
             //Alternative to List[String]()
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString, t))
  }

  // Exercise 18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h), t))
  }
 
  // Exercise 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((h,t) => if( f(h) )  Cons(h,t) else  t )
  }

  // Exercise 20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  // Exercise 21
  def filterFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if(f(a)) List(a) else Nil)

  // Exercise 22
  def twoListAdd(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
    case (Nil,_) => Nil
    case (_, Nil) => Nil
    case (Cons(hdA,tlA), Cons(hdB, tlB)) => Cons(hdA+hdB, twoListAdd(tlA, tlB)) 
  }

  // Exercise 23
  def twoListAddGeneral[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = (l,r) match {
    case (Nil,_) => Nil
    case (_, Nil) => Nil
    case (Cons(hdA,tlA), Cons(hdB, tlB)) => Cons(f(hdA,hdB), twoListAddGeneral(tlA, tlB)(f)) 
  }

  // Exercise 24
  // def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = 

}

object Tree {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maxTree(t: Tree[Int]): Int = t  match {
    case Leaf(x) => x // can you do Leaf(_) => _?
    case Branch(l,r) => maxTree(l) max maxTree(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
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
  println("5,4,3,2,1 => " + List.mkString(List.reverse(lst)))
  val intra = List(List(1,2), List(3,4), List(5,6))
  println("1,2,3,4,5,6 => " + List.mkString(List.concat(intra)))
}

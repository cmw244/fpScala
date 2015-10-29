package clen.errors
import scala.annotation.tailrec

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(x) => f(x)
    }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this 
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object option {

def mean(xs: Seq[Double]): Option[Double] = xs.isEmpty match {
    case true => None
    case false => Some(xs.sum / xs.length)
}

// Variance can actually be computed in one pass, but for pedagogical purposes 
// we will compute it using two passes. The first will compute the mean of the
// data set, and the second will compute the mean squared difference from this mean.
def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x-m, 2))))

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (Some(x), Some(y)) => Some(f(x,y))
    case _ => None
}
def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    None      
    // Need included functions?  
}

import java.util.regex._
def pattern(s: String): Option[Pattern] =
    try {
        Some(Pattern.compile(s))
    } catch {
        case e: PatternSyntaxException => None
    }



def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case hd::tl => map2(f(hd), traverse(tl)(f))(_ :: _)
}
 
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(ee) => Left(ee)
        case Right(a) => f(a)
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(_) => b
        case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] = 
        for { 
            a <- this;
            b1 <- b
        } yield f(a,b1)

    def traverse[E,A,B](es: List[A])(f: A => Either[E,B]): Either[E,List[B]] = es match {
        case Nil => Right(Nil)
        case hd::tl => (f(hd) map2 traverse(tl)(f))(_ :: _)
    }

    def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(x=>x)
}

}

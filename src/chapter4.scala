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

}

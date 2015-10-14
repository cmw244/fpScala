package clen.curry

import scala.annotation.tailrec

// Exercise 1
object Factorial {
    def fib(n: Int): Int = { 
        @tailrec
        def fibTail(p1: Int, p2: Int, i: Int): Int = i match {
            case 0 => p1
            case _ => fibTail(p2, p2+p1, i-1)
        }
        fibTail(0, 1, n)
    }
}

// Exercise 2
object IsSorted {
    @tailrec
    def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
        as.toList match {
            case Nil => true
            case _::Nil => true
            case a::b::_ if gt(a,b) => isSorted(as.tail, gt)
            case _ => false
        }
    }
}

// Exercise 3, 4, and 5
object Curry {
    // Note that '=>' associates to the right
    def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a,b)
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)
    def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}


object HelloWorld extends App {
    // Exercise 1
    println("Fib:")
    1 to 10 foreach { x => print(Factorial.fib(x) + " ") }

    println()
    println()
    
    // Exercise 2
    println("isSorted: 1 3 4 5 7 8 -> " + IsSorted.isSorted(Array(1,3,4,5,7,8), (x: Int, y: Int) => x < y))
    println("isSorted: 1 5 3 4 -O> " + IsSorted.isSorted(Array(1,5,3,4), (x:Int, y:Int) => x < y))
    println("isSorted: 1 -> " + IsSorted.isSorted(Array(1), (x:Int, y:Int) => x < y))
    println("isSorted: -> " + IsSorted.isSorted(Array(), (x:Int, y:Int) => x < y))

    println()
    println()
    
    // Exercise 3, 4, and 5
    def func(a: Int, b: Double) = a + b
    val curryed = Curry.curry(func) 
    println("42.0 -> " + curryed(4)(38.0))
    val plus5 = curryed(5);
    println("42.0 -> " + plus5(37.0))
    val original = Curry.uncurry(curryed)
    println("42.0 -> " + original(21, 21.0))
    def aToB(a: Int) = a + 1.0
    def bToC(b: Double) = b + 1
    val composed = Curry.compose(bToC, aToB)
    println("42.0 -> " + composed(40)) 
}




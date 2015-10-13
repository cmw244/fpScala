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
    def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
        as.toList match {
            case Nil => true
            case _::Nil => true
            case a::b::_ if gt(a,b) => isSorted(as.tail, gt)
            case _ => false
        }
    }
}



object HelloWorld extends App {
    println("Fib:")
    1 to 10 foreach { x => print(Factorial.fib(x) + " ") }

    println()
    println()
    
    println("isSorted: 1 3 4 5 7 8 -> " + IsSorted.isSorted(Array(1,3,4,5,7,8), (x: Int, y: Int) => x < y))
    println("isSorted: 1 5 3 4 -> " + IsSorted.isSorted(Array(1,5,3,4), (x:Int, y:Int) => x < y))
    println("isSorted: 1 -> " + IsSorted.isSorted(Array(1), (x:Int, y:Int) => x < y))
    println("isSorted: -> " + IsSorted.isSorted(Array(), (x:Int, y:Int) => x < y))
}




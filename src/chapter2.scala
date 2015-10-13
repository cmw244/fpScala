import scala.annotation.tailrec

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

object HelloWorld extends App {
    1 to 10 foreach { x => println(Factorial.fib(x)) }
}




object MyModule {

  def abs(n: Int): Int =
    if (n < 0)
      -n
    else
      n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else go(n - 1, acc * n)
    }

    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "Absolutna velju of %d is %d"
    msg.format(x, abs(x))
  }


  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }


  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  /**
    * Exercise 2.1
    * Calculates the n-th Fibonacci number
    */
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, prev2: Int): Int = {
      if (n == 0) prev
      else go(n - 1, prev2, prev + prev2)
    }

    go(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatResult("factorial", 5, factorial))
    println(formatResult("abs", -45, abs))
  }

}

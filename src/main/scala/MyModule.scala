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

  def findFirst[A](array: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n > array.length) -1
      else if (p(array(n))) n
      else loop(n + 1)

    loop(0)
  }

  /**
    * Excercise 2.2
    *
    * @param array
    * @param ordered predicate determining if element a <= b
    * @return
    */
  def isSorted[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def isSortedUpTo(n: Int): Boolean = {
      if (n == array.length) true
      else if (!ordered(array(n - 1), array(n))) false
      else isSortedUpTo(n + 1)
    }

    isSortedUpTo(1)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatResult("factorial", 5, factorial))
    println(formatResult("abs", -45, abs))

    val a = Array(4, 8, 7, 3)
    val b = Array(1, 2, 3, 4)
    def isLessOrEqual(x: Int, y: Int) = x <= y
    println(isSorted(a, isLessOrEqual))
    println(isSorted(b, isLessOrEqual))
  }

}

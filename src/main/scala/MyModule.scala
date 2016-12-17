object MyModule {

  def abs(n: Int): Int =
    if (n < 0)
      -n
    else
      n

  private def formatAbs(x: Int) = {
    val msg = "Absolutna velju of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else go (n - 1, acc * n)
    }

    go(n, 1)
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(factorial(5))

}

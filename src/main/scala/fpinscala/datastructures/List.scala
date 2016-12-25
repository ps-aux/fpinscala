package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 3.2
    */
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  /**
    * Exercise 3.3
    */
  def setHead[A](list: List[A], head: A): List[A] = {
    list match {
      case Nil => List(head)
      case Cons(_, xs) => Cons(head, xs)
    }
  }

  /**
    * Exercise 3.4
    */
  def drop[A](list: List[A], count: Int): List[A] = {
    if (count == 0)
      list
    else list match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, count - 1)
    }
  }

  /**
    * Exercise 3.5
    */
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case Cons(x, xs) => if (!f(x)) list else dropWhile(xs, f)
    }
  }


  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4)

    println(tail(l))
    println(setHead(l, 55))
    println(drop(l, 2))
    println(dropWhile(l, (x: Int) => x < 4))
  }
}
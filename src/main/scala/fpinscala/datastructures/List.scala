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
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  /**
    * Exercise 3.3
    */
  def setHead[A](l: List[A], head: A): List[A] =
    l match {
      case Nil => List(head)
      case Cons(_, xs) => Cons(head, xs)
    }

  /**
    * Exercise 3.4
    */
  def drop[A](l: List[A], count: Int): List[A] =
    if (count == 0)
      l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, count - 1)
    }

  /**
    * Exercise 3.5
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (!f(x)) l else dropWhile(xs, f)
    }

  /**
    * Exercise 3.6
    */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Cannot be empty")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }


  /*
  -----------------
   */
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  /**
    * Exercise 3.9
    */
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, a) => a + 1)

  /**
    * Exercise 3.10
    */
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
    * Exercise 3.11
    */

  def sum311(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product311(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length311[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  /**
    * Exercise 3.13
    */

  def append[A](l1: List[A], l2: List[A]): List[A]
  = foldRight(l1, l2)(Cons(_, _))

  /**
    * Exercise 3.16
    */

  def add1(l: List[Int]): List[Int]
  = foldRight(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  /**
    * 3.17
    */
  def mapToString(l: List[Double]): List[String]
  = foldRight(l, Nil: List[String])((a, acc) => Cons(a.toString, acc))

  /**
    * 3.18
    */
  def map[A, B](as: List[A])(f: A => B): List[B]
  = foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  /**
    * 3.19
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A]
   = foldRight(as, Nil:List[A])((a,acc) => if (f(a)) Cons(a, acc) else acc)



  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4)
    val l2 = List(100, 200)
    println(init(l))
    println(foldLeft(l, 1000)(_ + _))
    println(add1(l))
    println(mapToString(List(0.6, 4.5, 3.4)))
    println(filter(List(0.6, 4.5, 3.4))(_ > 1))
  }
}
package fpinscala.errorhandling

sealed trait Option[+A] {
  /** 4.1 **/

  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None


  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }


  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob


  def filter(f: A => Boolean): Option[A] =
    this match {
      case None => None
      case Some(a) => if (f(a)) this else None
    }


}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }


  /** 4.3 **/

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }

  def map2_2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa.flatMap(a => ob.map(b => f(a, b)))


  /** 4.3 **/
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

  }


}


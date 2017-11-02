import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  self=>

  //Additional point: You must use match case only in map and getOrElse 
  def map[B](f: A => B): Option[B] = ???

  def getOrElse[B>:A](default: => B): B = ???

  def foreach[U] (f:A=>U):Unit= ???

  def flatMap[B](f: A => Option[B]): Option[B] = ???

  def flatMap_1[B](f: A => Option[B]): Option[B] = ???

  def orElse[B>:A](ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = ???

  def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): Option[B] = self filter p map f
    def flatMap[B](f: A => Option[B]): Option[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
}
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}

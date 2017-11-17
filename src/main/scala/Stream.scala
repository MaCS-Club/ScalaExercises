trait Stream[+A] {

  def toListRecursive: List[A] = this match {
    case Cons(h,t) => h() :: t().toListRecursive
    case _ => List()
  }

  
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = ??? 

  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) 
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = ???
  
  def forAll(f: A => Boolean): Boolean = ???

  def headOption: Option[A] = ???

  def map[B](f: A => B): Stream[B] = ???

  def filter(f: A => Boolean): Stream[A] = ???

  def append[B>:A](s: => Stream[B]): Stream[B] = ???

  def flatMap[B](f: A => Stream[B]): Stream[B] = ???

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = ???
    

  def startsWith[A](s: Stream[A]): Boolean = ???

  def tails: Stream[Stream[A]] = ???

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = ???

  def from(n: Int): Stream[Int] = ???

  val fibs = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}

sealed trait List[+A]{
  self=>

  def foreach[U] (f:A=>U):Unit= ???
    
  def flatMap[B] (f:A=>List[B]):List[B]= ???
    
  def map[B] (f:A=>B):List[B]= ???
    
  def filter(f:A=>Boolean):List[A]= ???
    

  def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): List[B] = self filter p map f
    def flatMap[B](f: A => List[B]): List[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }
}
case object Nil extends List[Nothing]
case class Cons[+A] (head:A, tail:List[A]) extends List[A]


object List{
  def sum(ints: List[Int]): Int = {
    foldRight(ints,0) ((x,y)=>x+y)
  }
  def product(ds: List[Double]): Double = {
    foldRight(ds,1.0) (_*_)
  }
  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B):B={
    @annotation.tailrec
    def loop (l:List[A],b:B): B = l match{
      case Nil => b
      case Cons (x,xs) => loop(xs,f(b,x))
    }      
    loop(as,z)
  }
  def foldRight[A,B](as:List[A],z:B) (f:(A,B)=>B): B=
    foldLeft(as,(b:B)=>b) ((g,a)=>(b=>g(f(a,b)))) (z)

  def append[A](a1: List[A],a2: List[A]):List[A]=
    foldRight(a1,a2) (Cons(_,_))

}

package ex5

sealed trait Stream[+A] {
  
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h)
  }

  def toList(): List[A] = this match {
    case Empty      => List()
    case Cons(h, t) => List(h) ::: t().toList()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => Stream.cons(h, t().take(n-1))
    case _                     => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _                     => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty      => Stream.empty
    case Cons(h, t) => if (p(h)) Stream.cons( h, t().takeWhile(p)) else t().takeWhile(p)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty      => false
    case Cons(h, t) => p(h) || t().exists(p)
  }

  def forall(p: A => Boolean): Boolean = this match {
    case Empty      => true
    case Cons(h, t) => p(h) && t().forall(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h, t().foldRight(z)(f))
      case _          => z
    }
  }

  class ConsWrapper[A](tl: => Stream[A]) {
    println("ConsWrapper is created")
    def #::(hd: A): Stream[A] = Stream.cons(hd, tl) 
  }
  
  implicit def consWrapper[A](stream: () => Stream[A]): ConsWrapper[A] =
    new ConsWrapper[A](stream())

}

case object Empty extends Stream[Nothing] {
  println("Empty is created")
}
case class Cons[+A](h: A, tl: () => Stream[A]) extends Stream[A] {
  println("class Cons is created")
}
  
object Stream {

  object cons {
    def apply[A](head: A, tail: => Stream[A]): Stream[A] = {
      println("cons() is called")
      lazy val t = tail
  
      new Cons( head, () => t)
    }
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
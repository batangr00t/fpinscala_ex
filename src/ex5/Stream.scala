package ex5

import Stream.Empty
import Stream.Cons

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def toList(): List[A] = this match {
    case Empty      => List()
    case Cons(h, t) => List(h()) ::: t().toList()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => Cons(h, () => t().take(n - 1))
    case _                     => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _                     => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty      => Empty
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else t().takeWhile(p)
  }

  //def foldRight( )

  def exists(p: A => Boolean): Boolean = this match {
    case Empty      => false
    case Cons(h, t) => p(h()) || t().exists(p)
  }

  def forall(p: A => Boolean): Boolean = this match {
    case Empty      => true
    case Cons(h, t) => if (p(h())) { println("forall true"); t().forall(p) } else false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }
  
  //def map[B](f: A => B): Stream[B] = foldRight( Empty )( (a, b) => Cons( f(a), b ))

}

object Stream {
  private case object Empty extends Stream[Nothing] {
    println("Empty is created")

    override def toString = "Nil"
  }

  private case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    println("Cons is created")
  }

  //  override def toString = this match {
  //    def loop(n:Int, stream:Stream[A] ) = stream match {
  //      case Empty => this.toString()
  //      case Cons(h,t) => if ( n <= 0 ) "?" else h() + "," + ( loop(n-1, t()) ) 
  //    }
  //      
  //    loop( 10, this)   
  //  }

  def apply[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    new Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else apply(as.head, apply(as.tail: _*))
}
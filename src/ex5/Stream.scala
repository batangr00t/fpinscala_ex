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

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile(p))
    case _                      => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Stream(): Stream[A])((a, b) => if (p(a)) Stream(a, b) else Empty)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty      => false
    case Cons(h, t) => p(h()) || t().exists(p)
  }

  def forall(p: A => Boolean): Boolean = this match {
    case Empty      => true
    case Cons(h, t) => p(h()) && t().forall(p)
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream(): Stream[B])((a, b) => Stream(f(a), b))
  }

  //def hasSubsequence(): Boolean = headOption != None

  //  class ConsWrapper[A](tl: => Stream[A]) {
  //    println("ConsWrapper is created")
  //    def #::(hd: A): Stream[A] = Cons(hd, tl) 
  //  }
  //  
  //  implicit def consWrapper[A](stream: () => Stream[A]): ConsWrapper[A] =
  //    new ConsWrapper[A](stream())

}

object Stream {

  private case object Empty extends Stream[Nothing] {
    println("Empty is created")

    override def toString = "Nil"
  }

  private case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    //println("Cons is created")
    //override def toString = foldRight("")( (a, b) => print(a)+b )
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None         => Empty
    case Some((a, s)) => Stream(a, unfold(s)(f))
  }
}
package ex3

/**
 * @author batangr00t@daum.net
 */

sealed trait List[+A] {
  def head(): A
  def tail(): List[A]

  def setHead[U >: A](h: U): List[U] = this match {
    case Nil         => List(h)
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop(n: Int): List[A] = this match {
    case Nil         => Nil
    case Cons(x, xs) => if (n <= 0) this else xs.drop(n - 1)
  }

  def dropWhile(f: A => Boolean): List[A] = this match {
    case Cons(x, xs) if (f(x)) => xs.dropWhile(f)
    case _                     => this
  }

  def append[U >: A](l: List[U]): List[U] = foldRight(l)(Cons(_,_))
  
  def append2[U >: A](l: List[U]): List[U] = this match {
    case Nil         => l
    case Cons(x, xs) => Cons(x, xs.append2(l))
  }

  def init(): List[A] = this match {
    case Nil                   => Nil
    case Cons(x, Nil)          => Nil
    case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
    case Cons(x, xs)           => Cons(x, Nil) append xs.init
  }

  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Nil         => z
    case Cons(x, xs) => f(x, xs.foldRight2(z)(f))
  }

  // it is not same as foldRight, see append() which is implemented by foldXxxx method
  def foldRight2[B](z: B)(f: (A, B) => B): B = foldLeft(z)((a,b)=>f(b,a))
  
  def length(): Int = foldLeft(0)((b, _) => b + 1)

  def isEmpty(): Boolean

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    // copy from scala source
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.head)
      these = these.tail
    }
    acc
  }

  def reverse(): List[A] = foldLeft(Nil:List[A])((xs, x) => Cons(x, xs))

  def reverse2(): List[A] = this match {
    case Nil         => Nil
    case Cons(x, xs) => xs.reverse() append Cons(x, Nil)
  }
}

case object Nil extends List[Nothing] {
  def tail() = Nil
  def head() = throw new Error("Nil.head")
  def isEmpty() = true
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  def isEmpty() = false
}

object List {
  def sum(ints: List[Int]): Int = ints.foldLeft(0)(_ + _)

  def products(ds: List[Double]): Double = ds.foldLeft(1.0)(_ * _)

  // todo, get rid of stack overflow
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}



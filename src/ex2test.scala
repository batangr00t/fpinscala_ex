import cho.util

object ex2test extends App {

  // ex2.1
  def fib1(n: Int): Int = {
    @annotation.tailrec
    def loop(c: Int, a: Int, b: Int): Int = {
      if (c <= 1) a
      else loop(c - 1, b, a + b)
    }

    loop(n, 0, 1)
  }

  def fib2(n: BigInt): BigInt = {
    @annotation.tailrec
    def loop(c: BigInt, a: BigInt, b: BigInt): BigInt = {
      if (c <= 1) a
      else loop(c - 1, b, a + b)
    }

    loop(n, 0, 1)
  }

  // test
  util time fib1(100)
  util time fib2(100)

  // ex2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false
    }
    loop(1)
  }
  
  // test
  val as1 = Array(1, 2, 3, 4, 5)
  val as2 = Array(1, 2, 3, 4, 1)

  util time isSorted[Int](as1, _ < _)
  util time isSorted[Int](as2, _ < _)

  // ex2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a, b))
  }

  // test
  def sum(a: Int, b: Int): Int = a + b

  util time curry(sum)(1) 
  util time curry(sum)(1)(2)

  // ex2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // test
  def g(a: Int): (Int) => (Int) = {
    (b: Int) => (a * 2) + b
  }
  util time g(1)
  util time g(1)(2)
  util time uncurry(g)(1, 2)

  def h(a: Int)(b: Int): Int = {
    a * 2 + b
  }
  //util time h(1)(_)
  util time h(1)(2)
  util time uncurry(h)(1, 2)

  // ex2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  // test
  def g2(n: Int): Int = n * 2
  def h2(n: Int): Int = n + 2
  util time compose(h2, g2)(1)

}
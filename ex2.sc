import cho.util.time

object ex2 {

  // ex2.1
  def fib1(n: Int): Int = {
    @annotation.tailrec
    def loop(c: Int, a: Int, b: Int): Int = {
      if (c <= 1) a
      else loop(c - 1, b, a + b)
    }

    loop(n, 0, 1)
  }                                               //> fib1: (n: Int)Int

  def fib2(n: BigInt): BigInt = {
    @annotation.tailrec
    def loop(c: BigInt, a: BigInt, b: BigInt): BigInt = {
      if (c <= 1) a
      else loop(c - 1, b, a + b)
    }

    loop(n, 0, 1)
  }                                               //> fib2: (n: BigInt)BigInt

  // test
  time(fib1(100))                                 //> 0.029859 msec
                                                  //| -889489150
  time(fib2(100))                                 //> 1.148798 msec
                                                  //| 218922995834555169026

  // ex2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false
    }
    loop(1)
  }                                               //> isSorted: [A](as: Array[A], ordered: (A, A) => Boolean)Boolean

  // test
  val as1 = Array(1, 2, 3, 4, 5)                  //> as1  : Array[Int] = Array(1, 2, 3, 4, 5)
  val as2 = Array(1, 2, 3, 4, 1)                  //> as2  : Array[Int] = Array(1, 2, 3, 4, 1)

  isSorted[Int](as1, _ < _)                       //> res0: Boolean = true
  isSorted[Int](as2, _ < _)                       //> res1: Boolean = false

  // ex2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a, b))
  }                                               //> curry: [A, B, C](f: (A, B) => C)A => (B => C)

  // test
  def sum(a: Int, b: Int): Int = a + b            //> sum: (a: Int, b: Int)Int

  curry(sum)(1)                                   //> res2: Int => Int = <function1>
  curry(sum)(1)(2)                                //> res3: Int = 3

  // ex2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }                                               //> uncurry: [A, B, C](f: A => (B => C))(A, B) => C

  // test
  def g(a: Int): (Int) => (Int) = {
    (b: Int) => (a * 2) + b
  }                                               //> g: (a: Int)Int => Int
  g(1)                                            //> res4: Int => Int = <function1>
  g(1)(2)                                         //> res5: Int = 4
  uncurry(g)(1, 2)                                //> res6: Int = 4

  def h(a: Int)(b: Int): Int = {
    a * 2 + b
  }                                               //> h: (a: Int)(b: Int)Int
  h(1)(_)                                         //> res7: Int => Int = <function1>
  h(1)(2)                                         //> res8: Int = 4
  uncurry(h)(1, 2)                                //> res9: Int = 4

  // ex2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }                                               //> compose: [A, B, C](f: B => C, g: A => B)A => C

  // test
  def g2(n: Int): Int = n * 2                     //> g2: (n: Int)Int
  def h2(n: Int): Int = n + 2                     //> h2: (n: Int)Int
  compose(h2, g2)(1)                              //> res10: Int = 4

}
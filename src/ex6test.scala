import ex6._
import cho._
import scala.actors.threadpool.helpers.NanoTimer

object ex6test extends App {
  val rng = SimpleRNG(System.nanoTime()) //SimpleRNG(42) 

  println(rng)

  val (n1, rng2) = rng.nextInt
  println(n1, rng2)

  println("ex6.1----------------------------------")
  println(Int.MaxValue)
  println(Int.MinValue, Int.MinValue ^ -1)
  println(-2 ^ -1, -1 ^ -1)
  println(-(-2 + 1), -(-1 + 1))

  // slower
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    if (n >= 0) (n, nextRng) else (n ^ -1, nextRng)
  }

  // faster
  def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    if (n >= 0) (n, nextRng) else (-(n + 1), nextRng)
  }

  util time (for (i <- 1 to 100000) nonNegativeInt(rng))
  util time (for (i <- 1 to 100000) nonNegativeInt2(rng))
  util time (for (i <- 1 to 100000) nonNegativeInt(rng))
  util time (for (i <- 1 to 100000) nonNegativeInt2(rng))

  println("ex6.2----------------------------------")
  for (n <- 1 to 100) println(n.toDouble / Int.MaxValue - (n - 1).toDouble / Int.MaxValue)
  println(Double.MaxValue, Double.MinValue)
  println(1.toDouble / Long.MaxValue)
  println(Long.MaxValue.toHexString)

  def double(rng: RNG): (Double, RNG) = {
    val (n, newRng) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, newRng)
  }

  def doubleLoop(n: Int, rng: RNG) {
    if (n >= 0) {
      val (rd, newRng) = double(rng)
      println(rd)
      doubleLoop(n - 1, newRng)
    }
  }

  doubleLoop(10, rng)

  println("ex6.3----------------------------------")
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, newRng1) = rng.nextInt
    val (d, newRng2) = double(newRng1)
    ((i, d), newRng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, newRng1) = double(rng)
    val (i, newRng2) = newRng1.nextInt

    ((d, i), newRng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, newRng1) = double(rng)
    val (d2, newRng2) = double(newRng1)
    val (d3, newRng3) = double(newRng2)

    ((d1, d2, d3), newRng3)
  }

  def genRanLoop(n: Int, rng: RNG) {
    if (n >= 0) {
      val ((i1, d1), newRng1) = intDouble(rng)
      val ((d2, i2), newRng2) = doubleInt(newRng1)
      val ((d3, d4, d5), newRng3) = double3(newRng2)
      println((i1, d1), (d2, i2), (d3, d4, d5))
      genRanLoop(n - 1, newRng3)
    }
  }

  genRanLoop(10, rng)

  println("ex6.4----------------------------------")
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, l: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n <= 0) {
        (l, rng)
      } else {
        val (ranInt, newRng) = rng.nextInt
        loop(n - 1, ranInt :: l, newRng)
      }
    }

    loop(count, List(), rng)
  }

  val (l, newRng) = ints(10)(rng)

  println(l)

  println("ex6.5----------------------------------")
  type Rand[A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng =>
      {
        val (a, newRng) = s(rng)
        (f(a), newRng)
      }
  }

  def double2: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)
  println(double2(rng))

  println("ex6.6----------------------------------")
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, rng1) = ra(rng)
        val (b, rng2) = rb(rng1)
        (f(a, b), rng2)
      }
  }

  val int: Rand[Int] = rng => rng.nextInt
  def both[A, B](ra: Rand[A], rb: Rand[B]) = map2(ra, rb)((_, _))

  println(both(int, double2)(rng))
  println(both(double2, int)(rng))

  println("ex6.7----------------------------------")
  //  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  //    def loop(initRng: RNG, lr: List[Rand[A]], la: List[A]): Rand[List[A]] = {
  //      lr match {
  //        case Nil => { rng => (la, initRng) }
  //        case x :: xs => {
  //          val (a, newRnd) = x(initRng)
  //          loop(newRnd, xs, a :: la)
  //        }
  //      }
  //    }
  //
  //    loop(rng, fs, List())
  //  }

  val (l661, newRng661) = unit(List[Int](1, 2))(rng)
  println(l661, newRng661)
  val (l662, newRng662) = map2(int, unit(l661))(_ :: _)(newRng661)
  println(l662, newRng662)
  val (l663, newRng663) = map2(int, unit(l662))(_ :: _)(newRng662)
  println(l663, newRng663)

  val r00 = map(nonNegativeInt)(_ % 10)
  val r10 = map(nonNegativeInt)(_ % 10 + 10)
  val r20 = map(nonNegativeInt)(_ % 10 + 20)
  val l664 = List(r00, r10, r20)
  l664.map(f => println(f(rng)))

  println(map2(r00, unit(List()))(_ :: _)(rng))
  println(l664.foldRight(unit(List[Int]()))((r, l) => map2(r, l)(_ :: _))(rng))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((r, l) => map2(r, l)(_ :: _))
  }

  def genIntList(n: Int) = sequence(List.fill(n)(int))

  util time genIntList(10)(rng)

  println("ex6.8----------------------------------")
  def nonNegativeLessThan1(n: Int): Rand[Int] = {
    rng =>
      {
        val (i, rng1) = nonNegativeInt(rng)
        val mod = i % n
        if (i - mod + (n - 1) >= 0) (mod, rng1)
        else nonNegativeLessThan1(n)(rng1)
      }
  }

  println(nonNegativeLessThan1(10)(rng))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      {
        val (a, newRng) = f(rng)
        g(a)(newRng)
      }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i - mod + (n - 1) >= 0) unit(mod) //rng => (mod, rng)
      else nonNegativeLessThan(n)
    }
  }

  val (i1, newRng681) = nonNegativeLessThan(10)(rng)
  println(i1, newRng681)
  val (i2, newRng682) = nonNegativeLessThan(10)(newRng681)
  println(i2, newRng682)

  println("ex6.9----------------------------------")
  //  def newmap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
  //    flatMap(s)(i => rng => (f(i), rng))
  //  }
  def newmap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }
  
  println(newmap(nonNegativeInt)(_ % 10)(newRng682))
  
  //  def newmap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  //    flatMap(rng => {
  //      val (a, rng1) = ra(rng)
  //      val (b, rng2) = rb(rng1)
  //      ((a, b), rng2)
  //    })(
  //      pair =>
  //        rng => (f(pair._1, pair._2), rng))
  //  }

  def multiple(n:Int) = map(nonNegativeInt)( i => ( i % 10) * n )
  def multipleList(n:Int) = sequence(List.fill(10)(multiple(n)))
  
  println( flatMap(r00)( n => multipleList(n) )(rng) )    
    
  def newmap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => newmap(rb)(b => f(a, b)))
  }

  println(newmap2(int, double)((_, _))(newRng682))
  println(newmap2(double, int)((_, _))(rng))

  //  println("ex6.10---------------------------------")
  //  val s: State[RNG, Int] = State(rng => rng.nextInt)
  //
  //  println(s.map(_ % 10).run(rng))
  //
  //  println("ex6.11---------------------------------")
  //  //def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] 
}
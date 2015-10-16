import ex6._
import cho.util

object ex6test extends App {
  val rng = SimpleRNG(42)

  println(rng)

  val (n1, rng2) = rng.nextInt
  println(n1, rng2)

  println("ex6.1----------------------------------")
  println(Int.MaxValue)
  println(Int.MinValue, Int.MinValue ^ -1)
  println(-2 ^ -1, -1 ^ -1, 0 ^ -1, 1 ^ -1)
  println((-2 + 1), -(-1 + 1), 0, 1)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    if (n >= 0) (n, nextRng) else (n ^ -1, nextRng)
  }

  def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    if (n >= 0) (n, nextRng) else (-(n + 1), nextRng)
  }

  util time ( for ( i <- 1 to 100000 ) nonNegativeInt(rng) )
  util time ( for ( i <- 1 to 100000 ) nonNegativeInt2(rng) )
  util time ( for ( i <- 1 to 100000 ) nonNegativeInt(rng) )
  util time ( for ( i <- 1 to 100000 ) nonNegativeInt2(rng) )

  

}
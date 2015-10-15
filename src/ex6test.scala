import ex6._

object ex6test extends App {
  val rng = SimpleRNG(42)

  println(rng)

  val (n1, rng2) = rng.nextInt
  println (n1, rng2)
  
  println("ex6.1----------------------------------")
  println( Int.MaxValue )
}
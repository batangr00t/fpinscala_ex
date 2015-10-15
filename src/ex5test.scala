import ex5._
import cho.util

/**
 * @author batangr00t@daum.net
 */
object ex5test extends App {
  def if2[A](cond: Boolean, onTrue: (Int) => A, a: Int, onFalse: () => A): A = {
    if (cond) onTrue(a) else onFalse()
  }

  def onTrue(n: Int) = println("true" + n)
  if2(false, onTrue, 2, () => println("false"))

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
  }

  if2(true, println("true"), println("false"))

  // ex5.1
  println("ex5.1----------------------------------")
  val l = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(l.toList())

  println("ex5.2----------------------------------")
  val l2 = l.take(7)
  println(l2)
  println(l2.toList())
  println(l.take(11).toList())

  println(l.drop(1))
  println(l.drop(1).toList())
  println(l.drop(7).toList())
  println(l.drop(11).toList())

  println("ex5.3----------------------------------")
  val l3 = l.takeWhile(_ < 7)
  println(l3)
  //println( l3.toList() )

  println("ex5.4----------------------------------")
  val ones: Stream[Int] = Stream(1, ones)
  println(ones.take(10))
  println(ones.exists(_ == 1))
  println(ones.forall(_ < 1))

  println(ones.take(5).toList)
  //println( ones.map( _ + 1).exists( _ % 2 == 1))

  println("ex5.5----------------------------------")
  def nat(n: Int): Stream[Int] = Stream(n, nat(n + 1))

  println(nat(1).take(10).toList)
  println(nat(1).takeWhile(_ < 15).toList)

  val to10 = nat(1).takeWhile(_ <= 10)
  util time (to10.foldRight(0)((x, y) => x + y))
  util time (to10.foldRight(List(): List[Int])((x, xs) => x :: xs))
  util time (nat(1).foldRight(Stream(): Stream[Int])((x, y) => Stream(x, y))).take(10)
  util time (nat(1).foldRight(Stream(): Stream[Int])((x, y) => Stream(x, y)).take(10).toList)
  println(nat(1).foldRight(Stream(): Stream[Int])((x, y) => Stream(x, y)).take(10))

  println(nat(1).takeWhile2(_ <= 2).toList)

  println("ex5.7----------------------------------")
  println(nat(1).map(_ * 2.0).take(10).toList())

  println("ex5.8----------------------------------")
  def constant[A](a: A): Stream[A] = Stream(a, constant(a))

  println(constant('A').take(10).toList)
  println(constant(BigInt(100)).take(10).toList)
  println(constant(List("haha", "world")).take(3).toList)
  println(constant((a: Int, b: Int) => a * b).take(5))

  println("ex5.9----------------------------------")
  def from(n: Int): Stream[Int] = Stream(n, from(n + 1))

  println("ex5.10---------------------------------")
  def fibs(a: Int, b: Int): Stream[Int] = Stream(a, fibs(b, a + b))

  println(fibs(0, 1).takeWhile(_ <= 100).toList)

  println("ex5.11---------------------------------")
  val unf1 = Stream.unfold[Int, Int](0)(x => Some((x, x + 1)))
  util time unf1.take(5).toList

  val fib2 = Stream.unfold[Int, (Int, Int)]((0, 1))(x => Some((x._1, (x._2, x._1 + x._2))))
  util time fib2.takeWhile(_ <= 100).toList

  def fibsUnfold(a: Int, b: Int): Stream[Int] = {
    Stream.unfold[Int, (Int, Int)]((a, b))(x => Some((x._1 , (x._2, x._1 + x._2))))
  }
  util time fibsUnfold(0, 1).takeWhile(_<= 100).toList
  
  println("ex5.12---------------------------------")
  def fromUnfold(n: Int): Stream[Int] = Stream.unfold(n)(x => Some( (x, x+1) ) )
  
  util time ( fromUnfold(10).take(10).toList )
  util time ( from(10).take(10).toList )
  
  def constantUnfold[T](c: T): Stream[T] = Stream.unfold(c)( _ => Some( (c, c) ) )
  
  util time ( constantUnfold("Hi").take(10).toList )
  util time ( constant("Co").take(10).toList )
  
}
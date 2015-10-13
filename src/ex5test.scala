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
  println(ones)
  println(ones.exists(_ == 1))
  println(ones.forall(_ < 1))
  
  println( ones.take(5).toList )
  //println( ones.map( _ + 1).exists( _ % 2 == 1))

}
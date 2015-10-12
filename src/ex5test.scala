import ex5._
import cho.util

/**
 * @author batangr00t@daum.net
 */
object ex5test extends App {
  
  // ex5.1
  println("ex5.1----------------------------------")
  val l = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println( l.toList() )

  println("ex5.2----------------------------------")
  val l2 = l.take(7)
  println( l2 )
  println( l2.toList() )
  println( l.take(11).toList() )
  
  println( l.drop(1) )
  println( l.drop(1).toList() )
  println( l.drop(7).toList() )
  println( l.drop(11).toList() )
  
  println("ex5.3----------------------------------")
  val l3 = l.takeWhile(_ < 7)
  println( l3 )
  //println( l3.toList() )
  
  println("ex5.4----------------------------------")
  val ones: Stream[Int] = Stream(scala.util.Random.nextInt(), ones)
  println( ones )
  //println( ones.forall(_<1) )
}
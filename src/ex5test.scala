import ex5._
//import cho.util

/**
 * @author batangr00t@daum.net
 */
object ex5test extends App {
  
  // ex5.1
  println("ex5.1----------------------------------")
  //val l = 0 #:: Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println( l ) 

  println("ex5.2----------------------------------")
  val l2 = l.take(7)

  println( l2.toList )
  //println( l.take(11).toList )
  //println( l.drop(1) )
  //println( l.drop(1).toList )
  println( l.drop(7).toList )
  //println( l.drop(11).toList )
  
  println("ex5.3----------------------------------")
  val l3 = l.takeWhile(_ < 7)
  println( l3 )
  //println( l3.toList )
  
  println("ex5.4----------------------------------")
  
  val l4: Stream[Int] = {
    def loop(v: => Int): Stream[Int] = if ( v < 100 ) v #:: loop(v+1) else Stream.empty
    loop(1)
  } 
  println( l4.forall(_ < 7) )

//  def pairIf[A](b:Boolean, x: => A) = {
//    //lazy val y = x
//    if (b) (x,x)
//  }
//  
//  pairIf( false, { println("xx"); 3 } ) 
}
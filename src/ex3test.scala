import cho.util
import ex3._

/**
 * @author batangr00t@daum.net
 */
object ex3test extends App {

  // ex3.2
  println("ex3.2----------------------------------")
  val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  l.tail
  println(l.tail)

  val l2 = List()
  l2.tail
  println(l2.tail)

  // ex3.3
  println("ex3.3----------------------------------")
  util time l2.setHead(2)

  // ex3.4
  println("ex3.4----------------------------------")
  println("l = " + l)
  util time l.drop(3)
  util time l.drop(6)
  util time l.drop(9)

  // ex3.5
  println("ex3.5----------------------------------")
  println("l = " + l)
  util time l.dropWhile(_ <= 1)
  util time l.dropWhile(_ <= 1)
  util time l.dropWhile(_ <= 7)

  // ex3.6
  println("ex3.6----------------------------------")
  val l6 = List(Range(1, 100): _*)
  println(l6)
  util time l6.init

  // ex3.7
  println("ex3.7----------------------------------")
  util time l.foldRight(1.0)(_ * _)
  val l7 = List(0, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  util time l7.foldRight(1.0)(_ * _)

  // ex3.8
  println("ex3.8----------------------------------")
  util time l.foldRight(Nil: List[Int])(Cons(_, _))

  // ex3.9
  println("ex3.9----------------------------------")
  util time List().length
  util time l.length
  
  // ex3.10
  println("ex3.10----------------------------------")
  val range = Range(1,5000)
  val l10 = List( range: _* )
  //util time l10.foldRight(0)( _ + _ )
  util time l10.foldLeft(0)( _ + _ )
  
  // ex3.11
  println("ex3.11----------------------------------")
  util time List.sum(l)
  val l11 = List(1.0, 2.0, 3.0 )
  util time List.products(l11)
  util time l.length
  
  // ex3.12
  println("ex3.12----------------------------------")
  util time l.reverse
  util time l.reverse
  util time l.reverse2
  
  // ex3.13
  println("ex3.13----------------------------------")
  util time l.foldRight(0)(_+_)
  util time l.foldRight(0)(_+_)
  util time l.foldRight2(0)(_+_)
  
  // ex3.13
  println("ex3.14----------------------------------")
  util time l.append(l)
  util time l.append2(l)
}
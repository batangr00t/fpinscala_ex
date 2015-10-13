package ex4

/**
 * @author batangr00t@daum.net
 */
sealed trait Option[+A] {
//  def map[B](f: A => B): Option[B]
//  def flatMap[B](f: A => Option[B]): Option[B]
//  def getOrElse[B >: A](default: => B): B
//  def orElse[U >: A](ob: => Option[U]): Option[U]
//  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get:A) extends Option[A]

case object None extends Option[Nothing]
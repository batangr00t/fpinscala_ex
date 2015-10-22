package ex7

import java.util.concurrent._

trait ExecuteService {
  def summit[A](a: Callable[A]): Future[A]
}

trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

object Par {

  type Par[A] = ExecuteService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def get(timeout: Long, unit: TimeUnit) = get
    def cancel(evenIfRunning: Boolean): Boolean = false
    def isDone = true
    def isCancelled = false
  }

  def unit[A](a: A): Par[A] = (es: ExecuteService) => UnitFuture(a)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    es => UnitFuture(f(pa(es).get))
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es =>
      {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }
  }

  def fork[A](a: => Par[A]): Par[A] = {
    es =>
      es.summit(new Callable[A] {
        def call = a(es).get
      })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a: A => lazyUnit(f(a))
  }

  def run[A](s: ExecuteService)(a: Par[A]): Future[A] = a(s)
}
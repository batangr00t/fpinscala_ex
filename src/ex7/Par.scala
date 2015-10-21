package ex7

trait ExecuteService {
  def summit[A](a: Callable[A]): Future[A]
}

trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

object Par {

  type Par[A] = ExecuteService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def get(timeout: Long) = get
    def cancel(evenIfRunning: Boolean): Boolean = false
    def isDone = true
    def isCancelled = false
  }

  def unit[A](a: A): Par[A] = (es: ExecuteService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecuteService) =>
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
  
  def run[A](s: ExecuteService)(a: Par[A]): Future[A] = a(s)
}
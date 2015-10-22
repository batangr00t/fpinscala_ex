import ex7._

object ex7test extends App {
  val co = new Callable[Int]{ def call = 100 }
  println( co )
  println( co.call )
  
  val es = new ExecuteService {
    def summit[Int](c:Callable[Int]) = {
      new Future[Int] {
        def get = c.call
        def get(timeout: Long, unit: java.util.concurrent.TimeUnit) = get
        def cancel(evenIfRunning: Boolean) = false
        def isDone = true
        def isCancelled = false
      }
    }
  }
  
  println( es )
  println( es.summit( co ) )
  println( es.summit( co ).get )
  
  val par = Par.unit(1)
  println( par )
  
  println( Par.map( par )( _ * 3 )(es) )
  
  
  println("ex7.1----------------------------------")
}
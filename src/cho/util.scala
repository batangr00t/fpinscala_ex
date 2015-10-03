package cho

/**
 * @author batangr00t@daum.net
 */
object util {
  def time(f: => Any) = {
    val start = System.nanoTime()
    val result = f
    val msec: Double = (System.nanoTime() - start) / 1000.0 / 1000.0

    println("%f msec".format(msec))
    println(result)
  }
}
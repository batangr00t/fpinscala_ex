package ex6

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, newS) = run(s)
      g(a).run(newS)
    })
  }

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

}


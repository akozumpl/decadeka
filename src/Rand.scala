import cats.data.State

final case class Rand(long: Long) {
  // lifted from https://typelevel.org/cats/datatypes/state.html,
  // implements https://en.wikipedia.org/wiki/Linear_congruential_generator.
  def next = Rand(long * 6364136223846793005L + 1442695040888963407L)
}

object Rand {
  type T[A] = State[Rand, A]

  val long: State[Rand, Long] = State(rand => (rand.next, rand.long))

  val boolean: State[Rand, Boolean] = long.map(d => d % 2 >= 0)

  def maxInt(i: Int): State[Rand, Int] = long.map(_ % i).map(_.toInt)
}

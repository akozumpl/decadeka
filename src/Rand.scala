import cats.data.State

import java.lang.Math

final case class Rand(seed: Long) {
  // lifted from https://typelevel.org/cats/datatypes/state.html,
  // implements https://en.wikipedia.org/wiki/Linear_congruential_generator.
  def next = Rand(seed * 6364136223846793005L + 1442695040888963407L)
}

object Rand {

  /** Builder, discards the seed immediately */
  def build(seed: Long): Rand = Rand(seed).next

  val long: State[Rand, Long] = State(rand => (rand.next, rand.seed))

  val boolean: State[Rand, Boolean] = long.map(d => d % 2 >= 0)

  /** Yields a non-negative integer smaller or equal to `i` with a reduced
    * probability of 0.
    */
  def aSmallInt(max: Int): State[Rand, Int] =
    for {
      first <- long.map(l => Math.floorMod(l, max + 1))
      second <-
        if (first == 0) long.map(l => Math.floorMod(l, max + 1))
        else State.pure(first)
    } yield second

}

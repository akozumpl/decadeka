package decadeka

import cats.data.State

import java.lang.Math

final case class Rand(seed: Long) {
  // lifted from https://typelevel.org/cats/datatypes/state.html,
  // implements https://en.wikipedia.org/wiki/Linear_congruential_generator.
  def next = Rand(seed * 6364136223846793005L + 1442695040888963407L)
}

object Rand {

  private def magnitude(i: Int): Int =
    i match {
      case 0  => 3
      case 1  => 3
      case 2  => 4
      case 5  => 8
      case 10 => 2
      case _  => 10
    }

  private def buildDistribution(max: SmallInt): Vector[Int] =
    0
      .to(max.value)
      .foldLeft(List.empty[Int]) { case (l, i) =>
        List.fill(magnitude(i))(i) ::: l
      }
      .toVector

  /** Builder, discards the seed immediately */
  def build(seed: Long): Rand = Rand(seed).next

  val long: State[Rand, Long] = State(rand => (rand.next, rand.seed))

  val boolean: State[Rand, Boolean] = long.map(d => d % 2 >= 0)

  /** Yields a non-negative integer smaller or equal to `i` with reduced
    * probabilities of 0, 1, 2, 5 and 10.
    */
  def aSmallInt(max: SmallInt): State[Rand, Int] = {
    val distribution = buildDistribution(max)
    long.map { randLong =>
      val idx = Math.floorMod(randLong, distribution.length)
      distribution(idx)
    }
  }

}

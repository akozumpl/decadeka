package decadeka

import cats.data.State
import cats.data.StateT
import cats.effect.IO

import cats.Show

case class Multiply(
    left: Int,
    right: Int
) {
  val ask: String = s"$left × $right = "
  val expectedResult = left * right

  def isCorrect(answer: String): Boolean =
    answer.strip.toIntOption.map(_ == expectedResult).getOrElse(false)
}

object Multiply {
  import Rand._

  given Show[Multiply] = Show.show { mul => s"${mul.left} × ${mul.right}"}

  val LeftMax = SmallInt.static(10)

  def random(rightMax: SmallInt): State[Rand, Multiply] = for {
    swapLeftRight <- boolean
    left <- aSmallInt(LeftMax)
    right <- aSmallInt(rightMax)
  } yield if (swapLeftRight) Multiply(right, left) else Multiply(left, right)

  def randomT(rightMax: SmallInt): StateT[IO, Rand, Multiply] =
    StateT.fromState(random(rightMax).map(IO.pure))
}

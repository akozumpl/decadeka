import cats.data.State
import cats.data.StateT
import cats.effect.IO

case class Multiply(
    left: Int,
    right: Int
) {
  val ask: String = s"$left × $right = "
  val expectedResult = left * right

  def isCorrect(answer: String): Boolean =
    answer.toIntOption.map(_ == expectedResult).getOrElse(false)
}

object Multiply {
  import Rand._

  val LeftMax = 10

  def random(rightMax: Int): State[Rand, Multiply] = for {
    swapLeftRight <- boolean
    left <- aSmallInt(LeftMax)
    right <- aSmallInt(rightMax)
  } yield if (swapLeftRight) Multiply(right, left) else Multiply(left, right)

  def randomT(rightMax: Int): StateT[IO, Rand, Multiply] =
    StateT.fromState(random(rightMax).map(IO.pure))
}

//> using dep com.monovore::decline::2.4.1
//> using dep org.typelevel::cats-core::2.10.0
//> using dep org.typelevel::cats-effect::3.5.1

import cats.Show
import cats.data.State
import cats.data.StateT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.show._

import java.time.Duration
import java.time.Instant

object Deca extends IOApp {
  val con = Console.apply[IO]
  val LeftMax = 10

  given Show[Duration] = Show.show { duration =>
    val minutes = duration.toMinutes
    val seconds = duration.toSeconds
    String.format("%02d:%02d", minutes, seconds)
  }

  case class Multiply(
      left: Int,
      right: Int
  ) {
    val ask: String = s"$left * $right = "
    val expectedResult = left * right

    def isCorrect(answer: String): Boolean =
      answer.toIntOption.map(_ == expectedResult).getOrElse(false)
  }

  object Multiply {
    import Rand._
    def random(rightMax: Int): State[Rand, Multiply] = for {
      swapLeftRight <- boolean
      left <- maxInt(LeftMax + 1)
      right <- maxInt(rightMax + 1)
    } yield if (swapLeftRight) Multiply(right, left) else Multiply(left, right)

    def randomT(rightMax: Int): StateT[IO, Rand, Multiply] =
      StateT.fromState(random(rightMax).map(IO.pure))
  }

  case class Scorecard(
      start: Instant,
      completed: Int,
      correct: Int,
      correctCount: Int
  ) {
    val isDone: Boolean = correct >= correctCount
    val asEither: Either[Scorecard, Scorecard] = Either.cond(isDone, this, this)

    def addResult(isCorrect: Boolean): Scorecard = {
      val bumped = copy(completed = completed + 1)
      if (isCorrect) bumped.copy(correct = correct + 1) else bumped
    }
  }

  def ask(level: Int, score: Scorecard): StateT[IO, Rand, Scorecard] = {
    Multiply.randomT(level).flatMapF { multiply =>
      for {
        _ <- con.print(multiply.ask)
        answer <- con.readLine
        isCorrect = multiply.isCorrect(answer)
        _ <- con.println("Not quite 😞.").unlessA(isCorrect)
      } yield score.addResult(isCorrect)
    }
  }

  def exercise(cmdOptions: Cmdline): IO[Scorecard] = for {
    now <- IO.realTimeInstant
    seed = now.toEpochMilli
    score = Scorecard(now, 0, 0, cmdOptions.exerciseCount)
    score <- score
      .tailRecM(s => ask(cmdOptions.level, s).map(_.asEither))
      .runA(Rand(seed))
  } yield score

  def run(args: List[String]): IO[ExitCode] =
    Cmdline.parse(args) match {
      case Left(helpMessage) =>
        con.println(helpMessage).as(ExitCode.Error)
      case Right(cmdline) =>
        for {
          score <- exercise(cmdline)
          finish <- IO.realTimeInstant
          took = Duration.between(score.start, finish)
          _ <- con.println(
            show"Finiisht!\nTime it took: ${took}\nDeka strong 💪."
          )
        } yield ExitCode.Success
    }

}

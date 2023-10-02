//> using dep com.monovore::decline::2.4.1
//> using dep org.typelevel::cats-core::2.10.0
//> using dep org.typelevel::cats-effect::3.5.2

import cats.data.StateT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.monad._
import cats.syntax.show._

import java.time.Duration
import java.time.Instant
import scala.concurrent.duration

object Deca extends IOApp {
  val con = Console.apply[IO]

  // https://typelevel.org/cats-effect/docs/core/starvation-and-tuning
  // Disable starvation watchdog or else every laptop suspend triggers it.
  override def runtimeConfig =
    super.runtimeConfig
      .copy(cpuStarvationCheckInitialDelay = duration.Duration.Inf)

  case class Scorecard(
      start: Instant,
      completed: Int,
      correct: Int,
      correctCount: Int
  ) {
    val isDone: Boolean = correct >= correctCount
    val asEither: Either[Scorecard, Scorecard] = Either.cond(isDone, this, this)

    def addSuccess(totalAttempts: Int): Scorecard =
      copy(completed = completed + totalAttempts, correct + 1)
  }

  /** Generates ans asks about the multiplication until the answer is correct. */
  def ask(level: Int, score: Scorecard): StateT[IO, Rand, Scorecard] = {
    Multiply.randomT(level).flatMapF { multiply =>
      List
        .empty[Boolean]
        .iterateUntilM { answers =>
          for {
            _ <- con.print(multiply.ask)
            answer <- con.readLine
            isCorrect = multiply.isCorrect(answer)
            _ <- con.println("Not quite 😞.").unlessA(isCorrect)
          } yield isCorrect :: answers
        }(_.headOption.contains(true))
        .map(answers => score.addSuccess(answers.length))
    }
  }

  def exercise(cmdOptions: Cmdline): IO[Scorecard] = for {
    now <- IO.realTimeInstant
    seed = now.toEpochMilli
    score = Scorecard(now, 0, 0, cmdOptions.exerciseCount)
    score <- score
      .tailRecM(s => ask(cmdOptions.level, s).map(_.asEither))
      .runA(Rand.build(seed))
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

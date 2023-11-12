package decadeka

import cats.data.StateT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.syntax.applicative._
import cats.syntax.monad._
import cats.syntax.show._

import java.time.Duration
import scala.concurrent.duration

object Deca extends IOApp {
  val con = Console.apply[IO]

  // https://typelevel.org/cats-effect/docs/core/starvation-and-tuning
  // Disables the starvation watchdog or else a laptop suspend triggers it.
  override def runtimeConfig =
    super.runtimeConfig
      .copy(cpuStarvationCheckInitialDelay = duration.Duration.Inf)

  /** Generates and asks about a multiplication until the answer is correct. */
  def ask(level: SmallInt, score: Scorecard): StateT[IO, Rand, Scorecard] = {
    Multiply.randomT(level).flatMapF { multiply =>
      false
        .iterateUntilM { answers =>
          for {
            _ <- con.print(multiply.ask)
            answer <- con.readLine
            isCorrect = multiply.isCorrect(answer)
            _ <- con.println("Not quite 😞.").unlessA(isCorrect)
          } yield isCorrect
        }(identity)
        .flatMap(_ =>
          IO.realTimeInstant.map(ts => score.addSuccess(multiply, ts))
        )
    }
  }

  def exercise(cmdOptions: Cmdline): IO[Scorecard] = for {
    now <- IO.realTimeInstant
    seed = now.toEpochMilli
    initScore = Scorecard.empty(now, cmdOptions.exerciseCount)
    score <- initScore
      .recWhileNotDone(s => ask(cmdOptions.level, s))
      .runA(Rand.build(seed))
  } yield score

  def run(args: List[String]): IO[ExitCode] =
    Cmdline.parse(args) match {
      case Left(helpMessage) =>
        con.println(helpMessage).as(ExitCode.Error)
      case Right(cmdline) =>
        for {
          score <- exercise(cmdline)
          _ <- con.println(
            "Finiisht!\n" +
              show"Slowest multiplications:\n${score.byTimeTaken.take(3)}\n" +
              show"In total it took: ${score.totalTime} in total, ${score.timePerAnswer} on average.\n" +
              "Deka strong 💪."
          )
        } yield ExitCode.Success
    }

}

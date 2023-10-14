package decadeka

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
import scala.concurrent.duration

object Deca extends IOApp {
  val con = Console.apply[IO]

  // https://typelevel.org/cats-effect/docs/core/starvation-and-tuning
  // Disable starvation watchdog or else a laptop suspend triggers it.
  override def runtimeConfig =
    super.runtimeConfig
      .copy(cpuStarvationCheckInitialDelay = duration.Duration.Inf)

  /** Generates ans asks about the multiplication until the answer is correct.
    */
  def ask(level: SmallInt, score: Scorecard): StateT[IO, Rand, Scorecard] = {
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
    initScore = Scorecard(now, 0, 0, cmdOptions.exerciseCount)
    score <- initScore
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
          tookPerExercise = took.dividedBy(score.correctCount)
          _ <- con.println(
            "Finiisht!\n" +
            show"It took: ${took} in total, ${tookPerExercise} each.\n" +
            "Deka strong 💪."
          )
        } yield ExitCode.Success
    }

}

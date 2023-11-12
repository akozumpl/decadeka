package decadeka

import cats.Monad
import cats.Show
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._

import java.time.Duration
import java.time.Instant
import scala.annotation.tailrec

case class Scorecard(
    start: Instant,
    /** The correct answers and when they happened. */
    correct: Vector[(Multiply, Instant)],
    correctCount: Int
) {
  val isDone: Boolean = correct.length >= correctCount
  val asEither: Either[Scorecard, Scorecard] = Either.cond(isDone, this, this)

  def addSuccess(multiply: Multiply, ts: Instant): Scorecard =
    copy(correct = correct :+ (multiply, ts))

  def byTimeTaken: Vector[(Multiply, Duration)] = {
    correct
      .zip(start +: correct.map(_._2)) // with start times
      .map { case ((multiply, finish), start) =>
        (multiply, Duration.between(start, finish))
      }
      .sortBy(_._2)
      .reverse
  }

  def totalTime: Option[Duration] =
    correct.lastOption.map { case (_, finishInstant) =>
      Duration.between(start, finishInstant)
    }

  def timePerAnswer: Option[Duration] =
    for {
      total <- totalTime
      count <- Option.when(correct.length > 0)(correct.length)
    } yield total.dividedBy(count)

  def recWhileNotDone[F[_]](f: Scorecard => F[Scorecard])(using
      M: Monad[F]
  ): F[Scorecard] =
    if (isDone) M.pure(this)
    else this.tailRecM(f andThen (fs => fs.map(_.asEither)))

}

object Scorecard {
  def empty(start: Instant, correctCount: Int) =
    Scorecard(start, Vector.empty, correctCount)
}

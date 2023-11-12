package decadeka

import cats.Show
import cats.syntax.show._

import java.time.Duration
import java.time.Instant

case class Scorecard(
    start: Instant,
    /** When the correct answer happened. */
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
}

object Scorecard {
  def empty(start: Instant, correctCount: Int) =
    Scorecard(start, Vector.empty, correctCount)
}

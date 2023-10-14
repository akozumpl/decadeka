package decadeka

import java.time.Instant

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

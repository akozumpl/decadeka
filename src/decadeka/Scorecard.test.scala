package decadeka

import weaver.SimpleIOSuite

import java.time.Duration
import java.time.Instant

object ScorecardTest extends SimpleIOSuite {

  val now = Instant.now()

  val score = Scorecard
    .empty(now, 10)
    .addSuccess(Multiply(8, 4), now.plusSeconds(15))
    .addSuccess(Multiply(9, 9), now.plusSeconds(35))
    .addSuccess(Multiply(3, 9), now.plusSeconds(36))

  pureTest("Sorts results by the time they took.") {
    expect(
      score.byTimeTaken ==
        Vector(
          (Multiply(9, 9), Duration.ofSeconds(20)),
          (Multiply(8, 4), Duration.ofSeconds(15)),
          (Multiply(3, 9), Duration.ofSeconds(1))
        )
    )
  }

  pureTest("Calculates the total exercise time.") {
    expect(score.totalTime == Some(Duration.ofSeconds(36)))
  }

  pureTest("Calculates the correct time per answer.") {
    expect(score.timePerAnswer == Some(Duration.ofSeconds(12)))
  }
}

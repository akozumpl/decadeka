package decadeka

import weaver.SimpleIOSuite

import java.time.Instant
import java.time.Duration

object ScorecardTest extends SimpleIOSuite {

  val now = Instant.now()

  val score = Scorecard
    .empty(now, 10)
    .addSuccess(Multiply(8, 4), now.plusSeconds(15))
    .addSuccess(Multiply(9, 9), now.plusSeconds(35))
    .addSuccess(Multiply(3, 9), now.plusSeconds(37))

  pureTest("sorts results by the time they took") {
    expect(
      score.byTimeTaken ==
        Vector(
          (Multiply(9, 9), Duration.ofSeconds(20)),
          (Multiply(8, 4), Duration.ofSeconds(15)),
          (Multiply(3, 9), Duration.ofSeconds(2))
        )
    )
  }
}

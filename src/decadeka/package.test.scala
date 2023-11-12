package decadeka

import cats.syntax.show._
import weaver.SimpleIOSuite

import java.time.Duration

object PackageTest extends SimpleIOSuite {
  pureTest("Displays a long duration human friendly.") {
    val duration = Duration.ofSeconds(192)
    expect(show"$duration" == "03:12")
  }

  pureTest("Displays a short duration F1-style.") {
    val duration = Duration.ofMillis(8120)
    expect(show"$duration" == "8.120 s")
  }

  pureTest("Displays an empty duration correctly.") {
    val noDuration: Option[Duration] = None
    expect(noDuration.show == "n/a")
  }

}

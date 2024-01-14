package decadeka

import cats.syntax.show.*
import weaver.SimpleIOSuite

import java.time.Duration
import java.time.Instant
import java.time.ZoneId

object PackageTest extends SimpleIOSuite {
  pureTest("Displays an instant human friendly.") {
    val instant = Instant.ofEpochSecond(1705221793)
    given timezone: ZoneId = ZoneId.of("Europe/Dublin")
    println(instant.show)
    expect(show"$instant" == "08:43 on Sunday")
  }

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

package decadeka

import cats.syntax.show._
import weaver.SimpleIOSuite

import java.time.Duration

object PackageTest extends SimpleIOSuite {
  pureTest("display duration human friendly") {
    val duration = Duration.ofSeconds(192)
    expect(show"$duration" == "03:12")
  }
}

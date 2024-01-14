package decadeka

import cats.syntax.traverse.*
import weaver.SimpleIOSuite

import scala.util.Try
import scala.util.Failure

object SmallIntTest extends SimpleIOSuite {
  pureTest("Constructor works as expected") {
    expect(SmallInt.valid(3).map(_.value).toOption == Some(3)) and expect(
      SmallInt.valid(200).isInvalid
    )
  }

  pureTest("Static constructor fails when expected") {
    matches(Try(SmallInt.static(200))) {
      case Failure(e) if e.isInstanceOf[IllegalArgumentException] =>
        expect(e.getMessage == "200 is too large.")
    }
  }

}

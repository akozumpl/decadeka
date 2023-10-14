package decadeka

import cats.syntax.traverse._
import weaver.SimpleIOSuite

object SmallIntTest extends SimpleIOSuite {
  pureTest("Constructor works as expected") {
    expect(SmallInt.valid(3).map(_.value).toOption == Some(3)) and expect(
      SmallInt.valid(200).isInvalid
    )
  }
}

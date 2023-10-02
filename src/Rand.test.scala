import weaver.SimpleIOSuite

import cats.syntax.traverse._

object RandTest extends SimpleIOSuite {
  val Seed = 2016

  pureTest("Building a Rand immediatelly churns the seed value.") {
    expect(Rand.long.runA(Rand.build(Seed)).value != Seed)
  }

  pureTest("Generating a 100 small integers has expected properties.") {
    val randomList = 0
      .until(100)
      .toList
      .traverse(_ => Rand.aSmallInt(6))
      .runA(Rand.build(Seed))
      .value
    val zeroCount = randomList.count(_ == 0)
    expect(randomList.max == 6) and
      expect(randomList.min == 0) and
      expect(zeroCount >= 0) and
      expect(zeroCount < 7)
  }
}

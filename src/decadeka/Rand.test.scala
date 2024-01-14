package decadeka

import cats.syntax.traverse.*
import weaver.SimpleIOSuite

object RandTest extends SimpleIOSuite {
  val Seed = 2016
  val Six = SmallInt.static(6)

  pureTest("Generates the expected distribution") {
    val dist = Rand.buildDistribution(Six)
    expect(dist.count(_ == 0) == 2) and
      expect(dist.count(_ == 2) == 4) and
      expect(dist.count(_ == 6) == 10) and
      expect(dist.count(_ == 7) == 0)
  }

  pureTest("Building a Rand immediatelly churns the seed value.") {
    expect(Rand.long.runA(Rand.build(Seed)).value != Seed)
  }

  pureTest("Generating a 100 small integers has expected properties.") {
    val randomList = 0
      .until(100)
      .toList
      .traverse(_ => Rand.aSmallInt(Six))
      .runA(Rand.build(Seed))
      .value
    val zeroCount = randomList.count(_ == 0)
    val oneCount = randomList.count(_ == 1)
    expect(randomList.max == 6) and
      expect(randomList.min == 0) and
      expect(zeroCount >= 0) and
      expect(zeroCount <= 7) and
      expect(oneCount >= 0) and
      expect(oneCount <= 7)
  }
}

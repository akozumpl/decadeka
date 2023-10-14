package decadeka

import weaver.SimpleIOSuite

object MultiplyTest extends SimpleIOSuite {

  val m = Multiply(3, 5)

  pureTest("Tests for correctness.") {
    expect(m.isCorrect("15")) and expect(!m.isCorrect("-21"))
  }

  pureTest("Silentnly rejects garbage") {
    expect(!m.isCorrect("poppyrooster"))
  }

  pureTest("Whitespace gets strpped.") {
    expect(Multiply(5, 2).isCorrect(" 10 "))
  }

}

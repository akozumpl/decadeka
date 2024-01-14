package decadeka

import cats.data.Validated

case class SmallInt private (int: Int) {
  val value = int
}

object SmallInt {
  val Limit = 20

  /** Builds a valid SmallInt or throws.
    *
    * Use for static definitions only.
    */
  def static(i: Int) =
    valid(i).toEither.left.map(s => new IllegalArgumentException(s)).toTry.get

  def valid(i: Int): Validated[String, SmallInt] =
    if (i <= Limit)
      Validated.valid(SmallInt(i))
    else
      Validated.invalid(s"$i is too large.")

}

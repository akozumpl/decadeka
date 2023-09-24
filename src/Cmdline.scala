import cats.syntax.apply._
import com.monovore.decline._

import java.nio.file.Path

case class Cmdline(
    level: Int,
    exerciseCount: Int
)

object Cmdline {
  val level = Opts.option[Int](
    "level",
    "highest right-hand factor to exercise (left-hand is always max 10)"
  )
  val correctRun = Opts.option[Int]("count", "number of correct answers until pass")

  val decline = Command(
    "decadeka",
    "Deka trains her multiplications table before she can play Minecraft."
  )((level, correctRun).tupled).map(Cmdline.apply)

  def parse(args: List[String]): Either[String, Cmdline] =
    decline.parse(args).left.map(_.toString)

}

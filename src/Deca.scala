//> using dep com.monovore::decline::2.4.1
//> using dep org.typelevel::cats-core::2.10.0
//> using dep org.typelevel::cats-effect::3.5.1

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console

object Deca extends IOApp {
  val con = Console.apply[IO]

  def run(args: List[String]): IO[ExitCode] =
    Cmdline.parse(args) match {
      case Left(helpMessage) =>
        con.println(helpMessage).as(ExitCode.Error)
      case Right(cmdline) =>
        IO(ExitCode.Success)
    }

}

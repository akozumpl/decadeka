package decadeka

import cats.Show
import cats.syntax.contravariant.*
import cats.syntax.show.*

import java.time.Duration
import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField.*

private val DurationResolutionCutoff = Duration.ofSeconds(10)
private val InstantFormatter = new DateTimeFormatterBuilder()
  .appendValue(HOUR_OF_DAY, 2)
  .appendLiteral(":")
  .appendValue(MINUTE_OF_HOUR, 2)
  .appendLiteral(" on ")
  .appendText(DAY_OF_WEEK)
  .toFormatter()

given Show[Duration] = Show[Option[Duration]].contramap(Some(_))

given showInstant(using timezone: ZoneId): Show[Instant] =
  Show.show(_.atZone(timezone).format(InstantFormatter))

given Show[Option[Duration]] = Show.show {
  case Some(duration) =>
    if (duration.compareTo(DurationResolutionCutoff) >= 0) {
      val minutes = duration.toMinutes
      val seconds = duration.toSeconds
      String.format("%02d:%02d", minutes, seconds % 60)
    } else {
      val millis = duration.toMillis
      String.format("%.3f s", millis / 1000d)
    }
  case None => "n/a"
}

given Show[Vector[(Multiply, Duration)]] = Show.show(
  _.map { case (multiply, duration) =>
    show"$multiply: $duration"
  }.mkString("\n")
)

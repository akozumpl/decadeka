package decadeka

import cats.Show

import java.time.Duration

private val DurationResolutionCutoff = Duration.ofSeconds(10)

given Show[Duration] = Show.show { duration =>
  if (duration.compareTo(DurationResolutionCutoff) >= 0) {
    val minutes = duration.toMinutes
    val seconds = duration.toSeconds
    String.format("%02d:%02d", minutes, seconds % 60)
  } else {
    val millis = duration.toMillis
    String.format("%.3f s", millis / 1000d)
  }
}

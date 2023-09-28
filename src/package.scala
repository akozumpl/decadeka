import cats.Show

import java.time.Duration

given Show[Duration] = Show.show { duration =>
  val minutes = duration.toMinutes
  val seconds = duration.toSeconds
  String.format("%02d:%02d", minutes, seconds % 60)
}

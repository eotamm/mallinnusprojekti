# Muuttujat sekunneista minuuteiksi
sekunnit_minuteiksi <- function(x, digits = NULL) {
  stopifnot(is.numeric(x))
  m <- x / 60
  if (!is.null(digits)) m <- round(m, digits)
  m
}

sleep_pregnancy$duration_min <- sekunnit_minuteiksi(sleep_pregnancy$duration)
sleep_pregnancy$awake_min <- sekunnit_minuteiksi(sleep_pregnancy$awake)
sleep_pregnancy$onset_latency_min <- sekunnit_minuteiksi(sleep_pregnancy$onset_latency)

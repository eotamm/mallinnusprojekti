# Muuttujat sekunneista minuuteiksi
sekunnit_minuteiksi <- function(x, digits = NULL) {
  stopifnot(is.numeric(x))
  m <- x / 60
  if (!is.null(digits)) m <- round(m, digits)
  m
}
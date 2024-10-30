#' Get Duration in Note Values
#'
#' Converts a duration given in ticks to a standard musical note value 
#' (e.g., whole, half, quarter, eighth, sixteenth).
#'
#' @param ticks Numeric. The number of ticks representing the duration.
#' @param ticks_per_beat Numeric. The number of ticks in a single beat.
#' @return Character. The note value as a string, such as `"whole"`, `"half"`, `"quarter"`, 
#' `"eighth"`, `"sixteenth"`, or `"unknown"` if the note value does not match a standard duration.
#' @export
#'
#' @examples
#' get_duration(480, 120) # Returns "whole" (4 beats)
#' get_duration(240, 120) # Returns "half" (2 beats)
#' get_duration(120, 120) # Returns "quarter" (1 beat)
#' get_duration(60, 120)  # Returns "eighth" (0.5 beat)
#' get_duration(30, 120)  # Returns "sixteenth" (0.25 beat)
get_duration <- function(ticks, ticks_per_beat) {
  # Convert ticks into beats
  beats <- ticks / ticks_per_beat
  # Map beats to note values
  if (beats == 4) return("whole")
  if (beats == 2) return("half")
  if (beats == 1) return("quarter")
  if (beats == 0.5) return("eighth")
  if (beats == 0.25) return("sixteenth")
  return("unknown")
}

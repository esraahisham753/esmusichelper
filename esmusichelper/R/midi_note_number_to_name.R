#' Convert MIDI Note Numbers to Note Names
#'
#' This function converts a vector of MIDI note numbers into human-readable note names 
#' with octave information. For example, MIDI note 60 corresponds to "C4".
#'
#' @param note_numbers Integer vector. A vector of MIDI note numbers (0-127) 
#'     where each note number represents a specific musical pitch.
#' @return Character vector. A vector of note names corresponding to the given 
#'     MIDI note numbers, formatted as "<Note><Octave>" (e.g., "C4", "A#3").
#' @export
#'
#' @examples
#' midi_note_number_to_name(c(60, 61, 62)) # Returns c("C4", "C#4", "D4")
#' midi_note_number_to_name(69) # Returns "A4"
midi_note_number_to_name <- function(note_numbers) {
  note_numbers <- unlist(note_numbers)
  
  if (any(! is.numeric(note_numbers))) {
    stop("must be integer")
  }
  
  if (any(note_numbers > 127 | note_numbers < 0)) {
    stop("out of range")
  }
  
  note_names <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  note_list <- lapply(note_numbers, function(note_num) {
    octave <- (note_num %/% 12) - 1
    note <- note_names[(note_num %% 12) + 1]
    paste0(note, octave)
  })
  return(unlist(note_list))
}

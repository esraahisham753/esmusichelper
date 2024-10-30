library(tuneR)

#' Process MIDI Information
#'
#' This function returns a list containing MIDI event information with `note_on` 
#' and `note_off` events extracted from a MIDI file.
#'
#' @param midi_file Character. Path to the MIDI file.
#' @return A list with the following components:
#' \describe{
#'   \item{note_on_events}{Data frame or list. Contains information about note-on events, 
#'                         with each row representing an event (e.g., note, velocity, time).}
#'   \item{note_off_events}{Data frame or list. Contains information about note-off events,
#'                          with each row representing an event (e.g., note, time).}
#' }
#' @export
#'
#' @examples
#' midi_info <- process_midi("example.mid")
#' midi_info$note_on_events   # View note-on events
#' midi_info$note_off_events  # View note-off events
read_midi_file <- function(midi_file_path) {
  # Load the MIDI file
  midi_data <- readMidi(midi_file_path)
  
  note_on_events <- midi_data[midi_data$event == "Note On" & midi_data$parameter2 > 0, ]
  
  note_off_events <- midi_data[midi_data$event == "Note Off" | (midi_data$event == "Note On" & midi_data$parameter2 == 0), ]
  
  midi_info <- list(note_on_events = note_on_events, note_off_events = note_off_events)
  
  return(midi_info)
}

#' Detect Chords, Notes, and Rests from MIDI Information
#'
#' This function processes MIDI information to detect chords, individual notes,
#' and rests based on the timing of note-on and note-off events. It groups notes
#' that are played close together in time into chords and identifies rests based
#' on gaps between notes.
#'
#' @param midi_info A list containing two data frames: 
#'   - `note_on_events`: Data frame with note-on events, where each row contains 
#'     the `time` (timestamp) and `parameter1` (MIDI note number).
#'   - `note_off_events`: Data frame with note-off events, where each row contains 
#'     the `time` (timestamp) and `parameter1` (MIDI note number).
#' @param max_time_diff Numeric. The maximum time difference (in ticks) allowed 
#'   to consider notes as part of the same chord. Default is 10.
#' @return A data frame with two columns:
#'   - `timestamp`: The timestamp (in ticks) of each note/chord/rest.
#'   - `note`: The corresponding note name or "r" for rests. Chords are represented 
#'     as a list of note names.
#' @export
#'
#' @examples
#' midi_info <- list(
#'   note_on_events = data.frame(time = c(0, 5, 15, 20), parameter1 = c(60, 64, 62, 60)),
#'   note_off_events = data.frame(time = c(10, 15, 20, 25), parameter1 = c(60, 64, 62, 60))
#' )
#' returns the notes and rests for each timestamp
detect_chords_notes_and_rests <- function(midi_info, max_time_diff = 10) {
  note_on_events <- midi_info$note_on_events
  note_off_events <- midi_info$note_off_events
  ticks_per_beat <- 480
  notes_df <- data.frame(timestamp = numeric(), note = character(), stringsAsFactors = FALSE)
  
  i <- 1
  last_off_time <- 0  # Tracks the end time of the last note/chord
  
  while (i <= nrow(note_on_events)) {
    note_on_time <- note_on_events[i, "time"]
    chord_notes <- list(note_on_events[i, "parameter1"])  # Start with the first note in the chord
    
    # Detect other notes played close in time (forming a chord)
    j <- i + 1
    while (j <= nrow(note_on_events) && (note_on_events[j, "time"] - note_on_time) <= max_time_diff) {
      chord_notes <- c(chord_notes, note_on_events[j, "parameter1"])
      j <- j + 1
    }
    
    # Detect note-off times for all notes in the chord
    max_note_off_time <- -Inf
    for (note in chord_notes) {
      note_off_time <- note_off_events[note_off_events$parameter1 == note & note_off_events$time >= note_on_time, "time"]
      if (length(note_off_time) > 0) {
        max_note_off_time <- max(max_note_off_time, note_off_time[1])  # Get the latest release time for the chord
      }
    }
    
    # Calculate duration and timestamp
    duration_in_ticks <- max_note_off_time - note_on_time
    duration <- get_duration(duration_in_ticks, ticks_per_beat)
    
    # Detect rest if there is a gap between the previous chord/note and the current one
    if (note_on_time - last_off_time > max_time_diff) {
      rest_duration_in_ticks <- note_on_time - last_off_time
      rest_duration <- get_duration(rest_duration_in_ticks, ticks_per_beat)
      notes_df <- rbind(notes_df, data.frame(timestamp = last_off_time, note = "r", stringsAsFactors = FALSE))
    }
    
    # Add chord or note to the dataframe
    if (length(chord_notes) > 1) {
      notes_df <- rbind(notes_df, data.frame(timestamp = note_on_time, note = I(list(midi_note_number_to_name(chord_notes))), stringsAsFactors = FALSE))
    } else {
      notes_df <- rbind(notes_df, data.frame(timestamp = note_on_time, note = midi_note_number_to_name(chord_notes[[1]]), stringsAsFactors = FALSE))
    }
    
    last_off_time <- max_note_off_time  # Update the last note/chord release time
    i <- j  # Move to the next note/chord
  }
  
  return(notes_df)
}

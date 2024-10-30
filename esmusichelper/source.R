library(tuneR)

# Read MIDI file and extract events
read_midi_file <- function(midi_file_path) {
  midi_data <- readMidi(midi_file_path)
  
  events <- midi_data$tracks[[1]]  # Assuming a single track for simplicity
  
  # Extracting Note On/Off events
  note_on_events <- events[events$type == "Note On" & events$parameter2 > 0, ]
  note_off_events <- events[events$type == "Note Off" | (events$type == "Note On" & events$parameter2 == 0), ]
  
  list(note_on_events = note_on_events, note_off_events = note_off_events, events = events)
}

# Convert ticks into musical duration
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

# Detect chords, notes, and rests and create a data frame with timestamps
detect_chords_notes_and_rests <- function(midi_info, max_time_diff = 10) {
  note_on_events <- midi_info$note_on_events
  note_off_events <- midi_info$note_off_events
  ticks_per_beat <- midi_info$ticks_per_beat
  notes_df <- data.frame(timestamp = numeric(), note = character(), duration = character(), stringsAsFactors = FALSE)
  
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
      notes_df <- rbind(notes_df, data.frame(timestamp = last_off_time, note = "r", duration = rest_duration, stringsAsFactors = FALSE))
    }
    
    # Add chord or note to the dataframe
    if (length(chord_notes) > 1) {
      notes_df <- rbind(notes_df, data.frame(timestamp = note_on_time, note = I(list(midi_note_number_to_name(chord_notes))), duration = duration, stringsAsFactors = FALSE))
    } else {
      notes_df <- rbind(notes_df, data.frame(timestamp = note_on_time, note = midi_note_number_to_name(chord_notes[[1]]), duration = duration, stringsAsFactors = FALSE))
    }
    
    last_off_time <- max_note_off_time  # Update the last note/chord release time
    i <- j  # Move to the next note/chord
  }
  
  return(notes_df)
}

# MIDI note number to note name (like C4, D#5, etc.)
midi_note_number_to_name <- function(note_numbers) {
  note_names <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  note_list <- lapply(note_numbers, function(note_num) {
    octave <- (note_num %/% 12) - 1
    note <- note_names[(note_num %% 12) + 1]
    paste0(note, octave)
  })
  return(unlist(note_list))
}

# Example: Read a MIDI file and detect chords, notes, and rests
midi_info <- read_midi_file("path/to/your/midi_file.mid")
midi_info$ticks_per_beat <- midi_info$header$division  # Add ticks per beat to midi_info
notes_df <- detect_chords_notes_and_rests(midi_info)

print(notes_df)

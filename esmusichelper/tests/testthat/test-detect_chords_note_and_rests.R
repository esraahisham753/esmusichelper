test_that("detect_chords_notes_and_rests works correctly", {

  midi_info <- list(
    note_on_events = data.frame(time = c(0, 3, 5), parameter1 = c(60, 64, 67)),  # C, E, G (C major chord)
    note_off_events = data.frame(time = c(10, 15, 20), parameter1 = c(60, 64, 67))
  )
  
  result <- detect_chords_notes_and_rests(midi_info)
  expect_equal(nrow(result), 1)  # Should detect one chord
  expect_equal(unlist(result$note),c("C4", "E4", "G4")) # Expect C major chord
  
})


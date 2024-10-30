# Define the test context
test_that("process_midi correctly processes a MIDI file", {
  
  # Call the function with a sample MIDI file
  midi_info <- read_midi_file("canon.midi")
  
  # Test 1: Check that midi_info is a list
  expect_type(midi_info, "list")
  
  # Test 2: Check that midi_info contains two elements: note_on_events and note_off_events
  expect_named(midi_info, c("note_on_events", "note_off_events"))
  
  # Test 3: Check that note_on_events is a data frame 
  expect_true(is.data.frame(midi_info$note_on_events))
  
  # Test 4: Check that note_off_events is a data frame
  expect_true(is.data.frame(midi_info$note_off_events))
  
  # Test 5: Check that note_on_events has specific columns 
  expect_true(all(c("time", "parameter1", "parameter2") %in% names(midi_info$note_on_events)))

  
  # Test 6: Check that note_off_events has specific columns
  expect_true(all(c("time", "parameter1", "parameter2") %in% names(midi_info$note_off_events)))

})

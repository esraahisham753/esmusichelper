# Define the test context
test_that("midi_note_number_to_name converts MIDI note numbers correctly", {
  
  # Test 1: Basic Note Conversion
  expect_equal(midi_note_number_to_name(60), "C4")      # Middle C
  expect_equal(midi_note_number_to_name(61), "C#4")     # C# in octave 4
  expect_equal(midi_note_number_to_name(69), "A4")      # A4 (A above middle C)
  
  # Test 2: Multiple Notes
  expect_equal(midi_note_number_to_name(c(60, 61, 62)), c("C4", "C#4", "D4"))
  
  # Test 3: Boundary Cases (Lowest and Highest MIDI Notes)
  expect_equal(midi_note_number_to_name(0), "C-1")      # Lowest note, C in octave -1
  expect_equal(midi_note_number_to_name(127), "G9")     # Highest note, G in octave 9
  
  # Test 4: Notes across different octaves
  expect_equal(midi_note_number_to_name(48), "C3")      # C in octave 3
  expect_equal(midi_note_number_to_name(72), "C5")      # C in octave 5
  
  # Test 5: Edge Case - Non-standard input
  expect_error(midi_note_number_to_name("A"), "must be integer")  # Should error if input is not integer
  expect_error(midi_note_number_to_name(128), "out of range")     # Out-of-range error if >127
  expect_error(midi_note_number_to_name(-1), "out of range")      # Out-of-range error if <0
  
})

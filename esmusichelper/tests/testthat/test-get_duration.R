# Load testthat
library(testthat)

# Define the test context
test_that("get_duration correctly maps ticks to note values", {
  
  # Test 1: Whole note (4 beats)
  expect_equal(get_duration(480, 120), "whole")
  
  # Test 2: Half note (2 beats)
  expect_equal(get_duration(240, 120), "half")
  
  # Test 3: Quarter note (1 beat)
  expect_equal(get_duration(120, 120), "quarter")
  
  # Test 4: Eighth note (0.5 beat)
  expect_equal(get_duration(60, 120), "eighth")
  
  # Test 5: Sixteenth note (0.25 beat)
  expect_equal(get_duration(30, 120), "sixteenth")
  
  # Test 6: Unknown duration (not a standard note value)
  expect_equal(get_duration(100, 120), "unknown")
  
  # Additional Tests
  # Testing edge cases with non-standard tick values
  expect_equal(get_duration(0, 120), "unknown")   # Edge case with zero ticks
  expect_equal(get_duration(240, 60), "whole")    # Adjusted ticks_per_beat
  expect_equal(get_duration(15, 120), "unknown")  # Very small tick value
  
})

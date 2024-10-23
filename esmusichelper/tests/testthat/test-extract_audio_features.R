testthat::test_that("extract_audio_features works and returns correct values", {
  audio <- read_audio("canon.mp3")
  features <- extract_audio_features(audio)
  
  testthat::expect_type(features$duration, "double")
  testthat::expect_type(features$sample_rate, "integer")
  testthat::expect_type(features$channels, "integer")
  
  testthat::expect_equal(features$duration, 355.7181)
  testthat::expect_equal(features$sample_rate, 44100)
  testthat::expect_equal(features$channels, 2)
})
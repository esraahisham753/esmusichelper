testthat::test_that("read audio reads both .wav and ,mp3 files", {
  wav_audio <- read_audio("./canon.wav")
  mp3_audio <- read_audio("./canon.mp3")
  testthat::expect_s4_class(wav_audio, "Wave")
  testthat::expect_s4_class(mp3_audio, "Wave")
})
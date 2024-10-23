#'
#' Extract duration, sample rate and channels from audio Wave object and return them as a list
#' @param audio The audio Wave object
#' @return features, a list containing (duration, sample rate and channels)
#' @export
extract_audio_features <- function(audio) {
  if (! inherits(audio, "Wave")) {
    stop("This function accepts only Wave objects.")
  }
  
  duration <- length(audio@left) / audio@samp.rate
  sample_rate <- as.integer(audio@samp.rate)
  channels <- as.integer(audio@stereo + 1)
  
  features <- list(
    duration = duration,
    sample_rate = sample_rate,
    channels = channels
  )
  
  return(features)
}
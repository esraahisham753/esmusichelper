#' Read mp3 and wave files and return it as a variable
#' 
#' @param file_path path to the audio file
#' @return audio file variable
#' @export
read_audio <- function(file_path) {
  if (! file.exists(file_path)) {
    stop(paste("The file with file path =", file_path, "doesn't exist"))
  }
  
  ext <- tools::file_ext(file_path)
  
  audio <- NULL
  
  if (ext == "mp3") {
    audio <- tuneR::readMP3(file_path)    
  } else if (ext == "wav") {
    audio <- tuneR::readWave(file_path)
  } else {
    stop(paste(ext, "file extension isn't supported please use mp3 or wav files"))
  }
  
  return(audio)
}
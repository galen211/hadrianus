load_xes <- function(file_name) {
  require(xesreadR)
  message("Loading XES Log File...")
  ev <- read_xes(xesfile = file_name)
  message("XES Log File Loaded!")
  ev
}
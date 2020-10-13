#' Quicky and Easy Session Info .txt Files
#'
#' This function provides a simple way to export session information to disk.
#'
#' @param path The file path to where you would like to save your session information.
#' @examples
#' save_info("/Users/jackbailey/Desktop/session_info.txt")
#' @export

save_info <- function(path = "session_info.txt"){

  sink(file = path)
  utils::sessionInfo()
  sink()

}

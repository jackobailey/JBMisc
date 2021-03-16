#' Create List of All Objects in the Global Environment
#'
#' This function returns a list of all objects currently stored in the global environemnt. Useful for saving the output of an analysis in one go.
#'
#' @return A list object.
#' @examples
#' list_objects()
#' @export

list_objects <- function(){
  mget(
    ls(envir = .GlobalEnv),
    envir = .GlobalEnv
  )
}

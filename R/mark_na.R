#' Mark Values as NA
#'
#' This function makes it easier to convert values to missing (NA).
#'
#' @param x A vector of factor data to convert to a dummy.
#' @param term A vector of terms to recode as 1.
#' @param drop Drop missing levels? Defaults to TRUE.
#' @return A vector of data, with missing data marked as NA.
#' @examples
#' x <- sample(c("Coffee", "Tea", "Hot Chocolate"), replace = T, size = 100)
#' mark_na(x, term = "Coffee")
#' @export
mark_na <- function(x, term, drop = T){
  if(missing(term)){
    warning("You have not given values for mark_na to remove.")
  } else if(is.ordered(x) == T | is.factor(x) == T & drop == T){
    x[x %in% term] <- NA
    x <- droplevels(x)
    x
  } else {
    x[x %in% term] <- NA
    x
  }
}

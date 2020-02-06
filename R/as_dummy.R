#' Convert Factor Variables to Dummies
#'
#' This function takes away the need to rely on ifelse() to create dummy variables.
#'
#' @param x A vector of factor data to convert to a dummy.
#' @param term A vector of terms to recode as 1.
#' @param factor Convert the dummy to a factor variable? Defaults to FALSE.
#' @param labels If factor = T, a character vector to specify the names of the resulting levels (e.g. c("Tails", "Heads")). Defaults to c("Off", "On").
#' @return A vector of dummy data.
#' @examples
#' x <- sample(c("Coffee", "Tea", "Hot Chocolate"), replace = T, size = 100)
#' as_dummy(x, term = "Coffee")
#' @export

as_dummy <- function(x, term, factor = F, labels = c("Off", "On")){

  x <- ifelse(is.na(x) == T,
              yes = NA,
              no = ifelse(x %in% term, 1, 0))

  if(factor == T){
    x <-
      x %>%
      factor(
        labels = labels
      )
  }

  x
}

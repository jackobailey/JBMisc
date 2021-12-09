#' Convert Factor Variables to Dummies
#'
#' This function takes away the need to rely on ifelse() to create dummy variables.
#'
#' @param x A vector of factor data to convert to a dummy.
#' @param ... Terms to recode as 1.
#' @param factor Convert the dummy to a factor variable? Defaults to FALSE.
#' @param labels If factor = T, a character vector to specify the names of the resulting levels (e.g. c("Tails", "Heads")). Defaults to c("Off", "On").
#' @return A vector of dummy data.
#' @examples
#' x <- sample(c("Coffee", "Tea", "Hot Chocolate"), replace = TRUE, size = 10)
#' as_dummy(x, "Coffee")
#' @export

as_dummy <- function(x, ..., factor = F, labels = c("Off", "On")){


  # Create dummy variable

  x <-
    ifelse(
      is.na(x) == T,
      NA,
      ifelse(
        x %in% c(...),
        1,
        0
      )
    )


  # Convert to factor if desired

  if(factor == T){
    x <-
      factor(
        x,
        levels = 0:1,
        labels = labels
      )
  }


  # Return x

  return(x)

}

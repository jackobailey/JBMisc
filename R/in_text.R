#' In-Text Summary Statistics
#'
#' Generate in-text summary statistics for posterior distributions from Bayesian models. Most useful when used in Rmarkdown documents.
#'
#' @param x The distribution to summarise
#' @param inside Should the median be inside or outside the brackets? Defaults to T.
#' @param probs The level at which to compute the lower and upper bounds. Defaults to 0.95.
#' @param digits How many digits to round the summary statistics to. Defaults to 2.
#' @return A text string.
#' @examples
#' rnorm(1e3, 0.5, .1) %>% in_text()
#' @export

in_text <- function(x, inside = T, probs = .95, digits = 2){

  # Get median of the distribution and round to
  # desired number of digits

  m <-
    x %>%
    median() %>%
    round(digits = digits) %>%
    format(nsmall = digits)


  # Get lower bound and round to desired digits

  l <-
    x %>%
    quantile(probs = (1 - probs)/2) %>%
    round(digits = digits) %>%
    format(nsmall = digits)


  # Get upper bound and round to desired digits

  u <-
    x %>%
    quantile(probs = probs + ((1 - probs)/2)) %>%
    round(digits = digits) %>%
    format(nsmall = digits)


  # Return in-text version

  if(inside == T){

    paste0("(", m, ", ", probs*100, "% ", "CI: ", l, " to ", u, ")")

  } else {

    paste0(m, " (", probs*100, "% ", "CI: ", l, " to ", u, ")")

  }

}

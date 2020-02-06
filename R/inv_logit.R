#' Inverse Logit Function
#'
#' Convert variables from the logit to probability scale. A port of the same function from the rethinking package.
#'
#' @param x A vector of numeric data.
#' @examples
#' x <- rnorm(100)
#' inv_logit(x)
#' @export
inv_logit <- function(x){
    p <- 1/(1 + exp(-x))
    p <- ifelse(x == Inf, 1, p)
    p
}

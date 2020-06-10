#' Compute Combinations
#'
#' Compute arbitrary combinations (order doesn't matter).
#'
#' @param n Number of sample points in set n.
#' @param r Number of sample points in each combination.
#' @return A numbers.
#' @examples
#' ncr(n = 5, r = 2)
#' @export

ncr <- function(n = 1, r = 1){
  factorial(n)/(factorial(r)*factorial(n-r))
}

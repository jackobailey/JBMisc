#' Compute Permutations
#'
#' Compute arbitrary permutations (order matters).
#'
#' @param n Number of sample points in set n.
#' @param r Number of sample points in each permutation.
#' @return A numbers.
#' @examples
#' npr(n = 5, r = 2)
#' @export

npr <- function(n = 1, r = 1){
  factorial(n)/factorial(n-r)
}

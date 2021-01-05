#' Compute Two-Party Swing
#'
#' This convenience function takes two numeric vectors of vote share data and returns the estimated two-party swing.
#'
#' @param party_a A numeric vector of length 2. The first value corresponds to the most recent election.
#' @param party_b A numeric vector of length 2. The first value corresponds to the most recent election.
#' @examples
#' \dontrun{
#' compute_swing(party_a = c(0.5, 0.4), party_b = c(0.5, 0.6))
#' }
#' @export

compute_swing <- function(party_a = NULL, party_b = NULL){

  # Check party_a =! NULL, party_b =! NULL, and both != NULL

  if(is.null(party_a) == T & is.null(party_b) == T){
    warning("Both party_a and party_b are NULL. Please input values. E.g. 'party_a = c(0.5, 0.4), party_b = c(0.5, 0.6)'")
  } else if(is.null(party_a) == T){
    warning("party_a is NULL. Please input values. E.g. 'party_a = c(0.5, 0.4)'")
  } else if(is.null(party_b) == T){
    warning("party_b is NULL. Please input values. E.g. 'party_a = c(0.5, 0.4)'")
  }


  # Check that party_a and party_b are of type vector

  if(is.vector(party_a) == F & is.vector(party_b) == F){
    warning("Both party_a and party_b are not numeric. Please input numeric values. E.g. 'party_a = c(0.5, 0.4), party_b = c(0.5, 0.6)'")
  } else if(is.vector(party_a) == F){
    warning("party_a is not numeric. Please input numeric values. E.g. 'party_a = c(0.5, 0.4)'")
  } else if(is.vector(party_b) == F){
    warning("party_b is not numeric. Please input numeric values. E.g. 'party_a = c(0.5, 0.4)'")
  }


  # Check that party_a and party_b are of length 2

  if(length(party_a) != 2 & length(party_b) != 2){
    warning("Both party_a and party_b are not of length 2. Please input numeric vectors of length 2. E.g. 'party_a = c(0.5, 0.4), party_b = c(0.5, 0.6)'")
  } else if(length(party_a) != 2){
    warning("party_a is not of length 2. Please input numeric vectors of length 2. E.g. 'party_a = c(0.5, 0.4)'")
  } else if(length(party_b) != 2){
    warning("party_b is not of length 2. Please input numeric vectors of length 2. E.g. 'party_a = c(0.5, 0.4)'")
  }


  # Compute within-party change

  swing_a <- party_a[1] - party_a[2]
  swing_b <- party_b[1] - party_b[2]


  # Compute two-party swing

  swing <- (swing_a + swing_b)/2


  # Return two-party swing to user

  return(swing)


}

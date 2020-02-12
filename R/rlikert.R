#' Simulate Likert-Type Data
#'
#' Simulate ordinal variables of arbitrary length.
#'
#' @param n The number of random outcomes to generate.
#' @param length The number of response options.
#' @param mean The mean of the latent continuous variable that underpins the ordinal outcome. Defaults to 0.
#' @param disc The discrimination of the latent continuous variable that underpins the ordinal outcome. Defaults to 0.
#' @param sd The standard deviation of the latent continuous variable that underpins the ordinal outcome. Defaults to NULL.
#' @return A vector of likert type data.
#' @examples
#' rlikert(n = 10)
#' @export

rlikert <- function(n = 100,
                    length = 5,
                    mean = 0,
                    disc = 0,
                    sd = NULL){

  # Warnings in case things go wrong
  if(length < 2){
    stop("length must be greater than or equal to 2")
  } else if(n <= 0){
    stop("n must be greater than 0")
  } else if(is.null(sd) == F) {
    if(sd <= 0){
      stop("sd must be greater than 0")
    }
  }

  # Deal with sd and disc
  if(is.null(sd) == T){
    sd <- 1/exp(disc)
  }

  # Simulate probabilities for each response
  resp <-
    gtools::rdirichlet(n = n, alpha = rep(1, length)) %>%
    data.frame()

  # Drop last column
  resp[, length] <- NULL

  # Convert to thresholds
  for(i in (length-1):2){
    resp[, i] <-
      rowSums(resp[, 1:i])
  }

  # Convert thresholds to probit scale and incorporate
  # information from mean and sd parameters
  resp <-
    resp %>%
    sapply(qnorm) %>%
    data.frame()

  # Create empty data frame for probabilities
  dta <-
    data.frame(matrix(ncol = length, nrow = n)) %>%
    `names<-`(paste0("p", 1:length))

  # Calculate probabilities of each response for each individual
  dta[["p1"]] <- pnorm(resp[["X1"]], mean, sd)
  dta[[paste0("p", length)]] <- 1 - pnorm(resp[[paste0("X", length-1)]], mean, sd)
  for(i in 2:(length-1)){
    dta[[paste0("p", i)]] <-
      pnorm(resp[[paste0("X", i)]], mean, sd) -
      pnorm(resp[[paste0("X", i-1)]], mean, sd)
  }

  # For each individual, sample a single response category
  outcome <- rep(NA, n)
  for (i in 1:nrow(dta)) {
    outcome[i] <-
      sample(
        x = 1:length,
        size = 1,
        prob = dta[i,]
      )
  }

  # Return
  outcome %>% ordered(levels = 1:length)
}

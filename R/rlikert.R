#' Simulate Likert-Type Data
#'
#' Simulate ordinal variables of arbitrary length.
#'
#' @param n The number of random outcomes to generate.
#' @param length The number of response options.
#' @param mean The mean of the latent continuous variable that underpins the ordinal outcome. Defaults to 0.
#' @param sd The standard deviation of the latent continuous variable that underpins the ordinal outcome. Defaults to 1.
#' @return A vector of likert type data.
#' @examples
#' rlikert(n = 10)
#' @export

rlikert <- function(n = 100,
                    length = 5,
                    mean = 0,
                    sd = 1){

  if(length < 2){
    stop("length must be greater than or equal to 2")
  } else if(sd <= 0){
    stop("sd must be greater than 0")
  } else if(n <= 0){
    stop("n must be greater than 0")
  } else {

    # Simulate probabilities for each response
    probs <-
      gtools::rdirichlet(n = n, alpha = rep(1, length)) %>%
      data.frame()

    # Create empty dataframe
    dta <- tibble(n = 1:n)

    # Calculate probabilities of each response for each individual
    dta <-
      dta %>%
      mutate(
        p1 = pnorm(t1, mean, sd),
        p2 = pnorm(t2, mean, sd) - pnorm(t1, mean, sd),
        p3 = pnorm(t3, mean, sd) - pnorm(t2, mean, sd),
        p4 = pnorm(t4, mean, sd) - pnorm(t3, mean, sd),
        p5 = 1 - pnorm(t4, mean, sd)
      )

    # For each individual, sample a single response category
    dta <- dta %>% mutate(resp = NA)
    for (i in 1:nrow(dta)) {
      dta$resp[i] <-
        sample(
          x = c(1:5),
          size = 1,
          prob = c(dta$p1[i], dta$p2[i], dta$p3[i], dta$p4[i], dta$p5[i])
        )
    }

    # Return
    dta$resp %>%
      ordered()
  }
}

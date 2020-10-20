#' Simulate Likert-Type Data
#'
#' Simulate ordinal variables of arbitrary length.
#'
#' @param n The number of random outcomes to generate.
#' @param mean The mean of the latent continuous variable that underpins the ordinal outcome. Defaults to 0.
#' @param disc The discrimination of the latent continuous variable that underpins the ordinal outcome. Defaults to 0.
#' @param t1 The first threshold parameter. Defaults to -0.84.
#' @param t2 The second threshold parameter. Defaults to -0.25.
#' @param t3 The third threshold parameter. Defaults to 0.25.
#' @param t4 The fourth threshold parameter. Defaults to 0.84.
#' @return A vector of likert type data.
#' @examples
#' rlikert(n = 10)
#' @export

rlikert <- function(n = 1e3,
                    t1 = -0.84,
                    t2 = -0.25,
                    t3 = 0.25,
                    t4 = 0.84,
                    mean = 0,
                    disc = 0){

  # Calculate standard deviation from discrimination
  sd <- 1/exp(disc)

  # Create empty dataframe
  dta <- tibble::tibble(n = 1:n)

  # Calculate probabilities of each response for each individual
  dta <-
    dplyr::mutate(
      .data = dta,
      p1 = stats::pnorm(t1, mean, sd),
      p2 = stats::pnorm(t2, mean, sd) - stats::pnorm(t1, mean, sd),
      p3 = stats::pnorm(t3, mean, sd) - stats::pnorm(t2, mean, sd),
      p4 = stats::pnorm(t4, mean, sd) - stats::pnorm(t3, mean, sd),
      p5 = 1 - stats::pnorm(t4, mean, sd)
    )

  # Calculate outcome
  dta <-
    dplyr::mutate(
      .data = dta,
      resp =
        purrr::pmap_dbl(
          list(a = dta$p1, b = dta$p2, c = dta$p3, d = dta$p4, e = dta$p5),
          .f = function(a, b, c, d, e) sample(x = 1:5, size = 1, prob = c(a, b, c, d, e))
        )
    )

  # Return
  dta$resp

}


# rlikert <- function(n = 100,
#                     length = 5,
#                     mean = 0,
#                     disc = 0,
#                     sd = NULL){
#
#   # Warnings in case things go wrong
#   if(length <= 2){
#     stop("length must be greater than 2")
#   } else if(n <= 1){
#     stop("n must be greater than 1")
#   } else if(is.null(sd) == F) {
#     if(sd <= 0){
#       stop("sd must be greater than 0")
#     }
#   }
#
#   # Deal with sd and disc
#   if(is.null(sd) == T){
#     sd <- 1/exp(disc)
#   }
#
#   # Simulate probabilities for each response
#   resp <-
#     gtools::rdirichlet(n = n, alpha = rep(1, length)) %>%
#     data.frame()
#
#   # Drop last column
#   resp[, length] <- NULL
#
#   # Convert to thresholds
#   for(i in (length-1):2){
#     resp[, i] <-
#       rowSums(resp[, 1:i])
#   }
#
#   # Convert thresholds to probit scale and incorporate
#   # information from mean and sd parameters
#   resp <-
#     resp %>%
#     sapply(qnorm) %>%
#     data.frame()
#
#   # Create empty data frame for probabilities
#   dta <-
#     data.frame(matrix(ncol = length, nrow = n)) %>%
#     `names<-`(paste0("p", 1:length))
#
#   # Calculate probabilities of each response for each individual
#   dta[["p1"]] <- pnorm(resp[["X1"]], mean, sd)
#   dta[[paste0("p", length)]] <- 1 - pnorm(resp[[paste0("X", length-1)]], mean, sd)
#   for(i in 2:(length-1)){
#     dta[[paste0("p", i)]] <-
#       pnorm(resp[[paste0("X", i)]], mean, sd) -
#       pnorm(resp[[paste0("X", i-1)]], mean, sd)
#   }
#
#   # For each individual, sample a single response category
#   outcome <- rep(NA, n)
#   for (i in 1:nrow(dta)) {
#     outcome[i] <-
#       sample(
#         x = 1:length,
#         size = 1,
#         prob = dta[i,]
#       )
#   }
#
#   # Return
#   outcome %>% ordered(levels = 1:length)
# }

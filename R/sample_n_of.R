#' Sample Groups at Random
#'
#' Sample groups from a data set just as one would do using sample_n(). Based on a similar function from TJ Mahr's package tjmisc.
#'
#' @param data A tibble or data frame.
#' @param size The number of groups you want to sample.
#' @param replace Whether to sample with replacement or not. Defaults to TRUE.
#' @param ... The group you want to sample.
#' @examples
#' sample_n_of(iris, 2, replace = FALSE, Species)
#' @export

sample_n_of <- function(data, size, replace = TRUE, ...) {

  dots <- ggplot2::quos(...)

  group_ids <-  dplyr::group_by(.data = data,!!! dots)
  group_ids <- dplyr::group_indices(.data = group_ids)

  sampled_groups <- sample(unique(group_ids), size, replace = replace)

  dplyr::filter(.data = data, group_ids %in% sampled_groups)
}

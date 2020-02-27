#' Sample Groups at Random
#'
#' Convert variables from the logit to probability scale. A port of the same function from the rethinking package.
#'
#' @param data A tibble or data frame.
#' @param size The number of groups you want to sample.
#' @param replace Whether to sample with replacement or not. Defaults to TRUE.
#' @param ... The group you want to sample.
#' @examples
#' iris %>%
#' sample_n_of(1000, )
#' @export

sample_n_of <- function(data, size, replace = TRUE, ...) {
  dots <- quos(...)

  group_ids <- data %>%
    group_by(!!! dots) %>%
    group_indices()

  sampled_groups <- sample(unique(group_ids), size, replace = replace)

  data %>%
    filter(group_ids %in% sampled_groups)
}

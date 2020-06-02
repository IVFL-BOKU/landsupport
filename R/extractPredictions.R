#' Extracts mlr benchmark grid predictions as a data frame
#' @param res benchmark_grid() results
#' @import dplyr
#' @export
extractPredictions = function(res) {
  mlr3::as.data.table(res) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      task = purrr::map_chr(.data$task, function(x){x$id}),
      learner = purrr::map_chr(.data$learner, function(x){x$id}),
      resampling = purrr::map_chr(.data$resampling, function(x){x$id}),
      prediction = purrr::map(.data$prediction, function(x){x[[1]]$data$tab})
    ) %>%
    tidyr::unnest(.data$prediction) %>%
    return()
}

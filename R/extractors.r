#' @importFrom magrittr "%>%"
#' @importFrom rlang are_na
#' @importFrom purrr map map_df invoke_map keep discard
#' @importFrom stats var

submodels <- function(x) {
  stopifnot(class(x) == "MxModel")
  methods::slot(x, "submodels")
}

# Exract submodel data
smData <- function(x) {
  purrr::map(submodels(x), ~.x@data@observed, .id = ".submodel")
}

smSummaries <- function(x) {
  data <- smData(x)

  summaries <- list(mean = mean,
        variance = var,
        minimum = min,
        maximum = max,
        missing = function(x, na.rm = FALSE) sum(rlang::are_na(x))
    )

  calc_summaries <- function(x) {
    x %>%
      purrr::keep(is.numeric) %>%
      purrr::discard(is_invariant) %>%
      purrr::map_df(
        ~ purrr::invoke_map(summaries, x = .x, na.rm = TRUE),
        .id = ".variable"
      )
  }

  purrr::map_df(data, calc_summaries, .id = ".submodel")
}


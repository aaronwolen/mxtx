#' @importFrom rlang are_na
#' @importFrom purrr map map_df invoke_map keep discard
#' @importFrom stats var

submodels <- function(x) {
  stopifnot(class(x) == "MxModel")
  methods::slot(x, "submodels")
}

#' Extract submodel data
#'
#' @param x MxModel object
#' @param rm.invar remove variables with invariant values
#' @param num.only remove non-numeric variables
smData <- function(x, rm.invar = TRUE, num.only = TRUE) {
  out <- purrr::map(submodels(x), ~.x@data@observed, .id = ".submodel")
  if (rm.invar) out <- purrr::map(out, purrr::discard, .p = is_invariant)
  if (num.only) out <- purrr::map(out, purrr::keep, .p = is.numeric)
  out
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
    purrr::map_df(x,
      ~ purrr::invoke_map(summaries, x = .x, na.rm = TRUE),
      .id = ".variable"
    )
  }

  purrr::map_df(data, calc_summaries, .id = ".submodel")
}

smN <- function(x) {
  length(submodels(x))
}

smNames <- function(x) {
  names(submodels(x))
}


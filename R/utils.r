#' @importFrom stats na.omit
is_invariant <- function(x) {
  length(unique(stats::na.omit(x))) == 1
}

#' @importFrom stats na.omit
is_invariant <- function(x) {
  length(unique(stats::na.omit(x))) == 1
}


# Splice hetcor results
spliceHetCor <- function(x, values = c("tests", "correlations"), diag = FALSE) {
  stopifnot(class(x) == "hetcor")
  hetcor.values <- c("correlations", "type", "std.errors", "n", "tests")
  values[1] <- match.arg(values[1], hetcor.values)
  values[2] <- match.arg(values[2], hetcor.values)

  # tests is the only non-symmetrical matrix with an empty upper triangle
  if ("tests" %in% values) {
    values <- c("tests", setdiff(values, "tests"))
  }

  lmat <- x[[values[1]]]
  umat <- x[[values[2]]]
  out <- lmat
  out[upper.tri(lmat)] <- umat[upper.tri(umat)]

  if(!diag) diag(out) <- NA
  out
}

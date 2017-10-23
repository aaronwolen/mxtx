#' Generate appropriate report module
#'
#' @name reportModule
#' @export
reportModule <- function(x) UseMethod("reportModule")

#' @rdname reportModule
#' @export
reportModule.MxModel <- function(x) {
  file <- system.file("modules", "model-summary.Rmd", package = "mxtx")
  src <- knitr::knit_expand(file, .label = x$name)
  knitr::knit(text = src, envir = env(.model = x), quiet = TRUE)
}

#' @rdname reportModule
#' @export
reportModule.list <- function(x) {
  file <- system.file("modules", "model-comparison.Rmd", package = "mxtx")

  .name <- if (length(x) == 2) {
    paste(map_chr(x, "name"), collapse = " vs ")
  } else {
    paste(c(x[[1]]$name, paste0(length(x) - 1, " models")), collapse = " vs ")
  }

  src <- knitr::knit_expand(file, .name = .name, .label = make.names(.name))
  knitr::knit(text = src, envir = env(.models = x), quiet = TRUE)
}

#' @rdname reportModule
#' @export
reportModule.default <- function(x) {
  stop("No report module available for unrecognized object.", call. = FALSE)
}

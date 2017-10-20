#' Generate an OpenMx Report
#'
#' @param models a list of `MxModel` objects
#' @examples
#' data(AdeFit)
#' mxReport(.models = list(AdeFit, AeFit))
#' @export

mxReport <- function(.models, report_file = "openmx_report.html", report_dir = ".", params = report_params(name = "Aaron")) {

  if (class(.models) == "MxModel") .models <- list(.models)
  rmd_file <- system.file("openmx_report.Rmd", package = "mxtx")
  report_dir <- normalizePath(report_dir, mustWork = TRUE)

  output_file <- rmarkdown::render(
    rmd_file,
    output_file = report_file,
    output_dir = report_dir,
    clean = F,
    quiet = TRUE
  )
}

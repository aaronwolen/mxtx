table_gof <- function(x) {
  stopifnot(class(x) == "summary.mxmodel")
  data.frame(
     os    = x$observedStatistics,
     ep    = x$estimatedParameters,
    `-2LL` = x$Minus2LogLikelihood,
     AIC   = x$AIC.Mx,
    check.names = FALSE
  )
}

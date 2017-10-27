# mxtx: OpenMx Transcriber

Automated reporting for OpenMx analyses.

**NOTE:** This is a *very* half-baked package that was built for demonstration purposes at the [Advanced Genetic Epidemiology Statistical Workshop](http://www.vipbg.vcu.edu/events/workshops/2017/ages/).

## Installation

```r
# install.packages("devtools")
devtools::install_github("aaronwolen/mxtx")
```

## Example

```r
library(mxtx)

# summarize the underlying data and AdeFit
mxReport(AdeFit)

# ...include a summary of AeFit
mxReport(list(AdeFit, AeFit))

# ...include a comparison of AdeFit and AeFit
mxReport(list(AdeFit, AeFit, c(AdeFit, AeFit)))
```

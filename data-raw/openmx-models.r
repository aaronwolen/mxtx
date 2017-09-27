# Generate example MxModel objects
#
# Outputs:
#  - data/AdeFit

library(OpenMx)
library(devtools)

data("twinData", package = "OpenMx")
twinData <- transform(twinData, age = age / 100)
twinData <- subset(twinData, !is.na(age) & zyg %in% c(1, 3))

# SET MODEL PARAMETER VALUES ----------------------------------------------

Vars      <- 'bmi'                     # outcome variable
covVars   <- 'age'                     # covariates
selVars   <- paste0(Vars,c(rep(1, nv),rep(2, nv)))
nv        <- 1                         # number of variables
ntv       <- nv*2                      # number of total variables
svMe      <- 20                        # start value for means
svVa      <- .8                        # start value for variance
lbVa      <- .0001                     # start value for lower bounds
svVas     <- diag(svVa,ntv,ntv)        # start values on diagonal of covariance matrix
lbVas     <- diag(lbVa,ntv,ntv)        # lower bound for variances
laMeMZ    <- c("m1MZ","m2MZ")          # labels for means for MZ twins
laMeDZ    <- c("m1DZ","m2DZ")          # labels for means for DZ twins
laVaMZ    <- c("v1MZ","c21MZ","v2MZ")  # labels for (co)variances for MZ twins
laVaDZ    <- c("v1DZ","c21DZ","v2DZ")  # labels for (co)variances for DZ twins


# PREPARE ADE MODEL -------------------------------------------------------

# Set Starting Values
svMe      <- 20                    # start value for means
svPa      <- .6                    # start value for path coefficients (sqrt(variance/#ofpaths))


# ADE Model
# Matrices declared to store a, d, and e Path Coefficients
pathA     <- mxMatrix( type="Full", nrow=nv, ncol=nv, free=TRUE, values=svPa, label="a11", name="a" )
pathD     <- mxMatrix( type="Full", nrow=nv, ncol=nv, free=TRUE, values=svPa, label="d11", name="d" )
pathE     <- mxMatrix( type="Full", nrow=nv, ncol=nv, free=TRUE, values=svPa, label="e11", name="e" )


# Matrices generated to hold A, D, and E computed Variance Components
covA      <- mxAlgebra( expression=a %*% t(a), name="A" )
covD      <- mxAlgebra( expression=d %*% t(d), name="D" )
covE      <- mxAlgebra( expression=e %*% t(e), name="E" )


# Matrices for covariates and linear regression coefficients
defAge    <- mxMatrix( type="Full", nrow=1, ncol=1, free=FALSE, labels=c("data.age"), name="Age" )
pathB     <- mxMatrix( type="Full", nrow=1, ncol=1, free=TRUE, values= .01, label="b11", name="b" )


# Algebra for expected Mean Matrices in MZ & DZ twins
meanG     <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels="xbmi", name="mean" )
expMean   <- mxAlgebra( expression= mean + cbind(b%*%Age,b%*%Age), name="expMean" )


# Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
covP      <- mxAlgebra( expression= A+D+E, name="V" )
expCovMZ  <- mxAlgebra( expression= rbind( cbind(V, A+D), cbind(A+D, V)), name="expCovMZ" )
expCovDZ  <- mxAlgebra( expression= rbind( cbind(V, 0.5%x%A+ 0.25%x%D),
                                           cbind(0.5%x%A+ 0.25%x%D , V)), name="expCovDZ" )

# Data objects for Multiple Groups
dataMZ <- mxData(observed = subset(twinData, zyg == 1), type = "raw")
dataDZ <- mxData(observed = subset(twinData, zyg == 3), type = "raw")


# Objective objects for Multiple Groups
expMZ <-
  mxExpectationNormal(covariance = "expCovMZ",
                      means = "expMean",
                      dimnames = selVars)
expDZ <-
  mxExpectationNormal(covariance = "expCovDZ",
                      means = "expMean",
                      dimnames = selVars)
funML <- mxFitFunctionML()


# Combine Groups
pars      <- list(pathA, pathD, pathE, covA, covD, covE, covP, pathB)
modelMZ   <- mxModel(pars, defAge, meanG, expMean, expCovMZ, dataMZ, expMZ, funML, name="MZ" )
modelDZ   <- mxModel( pars, defAge, meanG, expMean, expCovDZ, dataDZ, expDZ, funML, name="DZ" )
multi     <- mxFitFunctionMultigroup( c("MZ","DZ") )
AdeModel  <- mxModel( "ADE", pars, modelMZ, modelDZ, multi )

AdeFit    <- mxRun(AdeModel)

devtools::use_data(AdeFit)

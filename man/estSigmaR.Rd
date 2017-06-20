\name{estSigmaR}
\alias{estSigmaR}
\title{Estimate Recruitment Sigma}
\description{
  Estimate sigma R (recruitment variability), based on the empirical
  standard deviation of recruitment deviates in log space.
}
\usage{
estSigmaR(model, digits=2)
}
\arguments{
  \item{model}{fitted \code{scape} model containing element \code{Dev}.}
  \item{digits}{number of decimal places to use when rounding, or
    \code{NULL} to suppress rounding.}
}
\value{
  Vector of two numbers, estimating recruitment variability based on (1)
  the estimated age composition in the first year, and (2) subsequent
  annual recruitment.
}
\note{
  This function uses the empirical standard deviation to estimate sigma
  R, which may be appropriate as likelihood penalty (or Bayesian prior
  distribution) for recruitment deviates from the stock-recruitment
  curve. The smaller the estimated recruitment deviates, the smaller the
  estimated sigma R.

  \code{estSigmaR} can be used iteratively, along with
  \code{\link{estN}} and \code{\link{estSigmaI}} to assign likelihood
  weights that are indicated by the model fit to the data. Sigmas and
  sample sizes are then adjusted between model runs, until they
  converge. The \code{iterate} function facilitates this procedure.

  If \eqn{ss} is the sum of squared recruitment deviates in log space
  and \eqn{n} is the number of estimated recruitment deviates, then the
  estimated sigma R is:

  \deqn{\sigma_R=\sqrt{\frac{ss}{n}}}{sigmaR = sqrt(ss/n)}

  The denominator is neither \eqn{n-}1 nor \eqn{n-p}, since \eqn{ss} is
  based on deviates from zero and not the mean, and the deviates do not
  converge to zero as the number of model parameters increases.
}
\seealso{
  \code{\link{getN}}, \code{\link{getSigmaI}}, \code{\link{getSigmaR}},
  \code{\link{estN}}, \code{\link{estSigmaI}}, and \code{estSigmaR}
  extract and estimate sample sizes and sigmas.

  \code{\link{iterate}} combines all the \code{get*} and \code{est*}
  functions in one call.

  \code{\link{plotN}} and \code{\link{plotB}(..., what="s")} show what
  is behind the sigma R estimation.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
getSigmaR(x.cod)  # sigmaR used in assessment 0.5 and 1.0
estSigmaR(x.cod)  # model estimates imply 0.20 and 0.52

getSigmaR(x.ling)  # 0.6, deterministic age distribution in first year
estSigmaR(x.ling)  # model estimates imply 0.36

getSigmaR(x.sbw)
estSigmaR(x.sbw)  # large deviates in first year
plotN(x.sbw)      # enormous plus group and 1991 cohort

# x.oreo assessment had deterministic recruitment, so no deviates
}

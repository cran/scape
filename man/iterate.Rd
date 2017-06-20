\name{iterate}
\alias{iterate}
\title{Get Candidate Sigmas and Sample Sizes}
\description{
  Compare current sigmas and sample sizes with candidate values, by
  running variations of \code{estSigmaR}, \code{estN}, and
  \code{estSigmaI} on all model components.
}
\usage{
iterate(model, ceiling=Inf, p=1, digits.n=0, digits.sigma=2)
}
\arguments{
  \item{model}{fitted \code{scape} model.}
  \item{ceiling}{largest possible sample size in one year, passed to
    \code{estN}.}
  \item{p}{effective number of parameters estimated in the model, passed
    to \code{estSigmaI}.}
  \item{digits.n}{number of decimal places to use when rounding sample
    sizes, or \code{NULL} to suppress rounding.}
  \item{digits.sigma}{number of decimal places to use when rounding
    sigmas, or \code{NULL} to suppress rounding.}
}
\value{
  List containing data frames summarizing current sigmas and sample
  sizes, as well as candidate values. The following abbreviations are
  used in column names:

  \describe{
    \item{sigmahat}{candidate sigma, the empirical standard deviation.}
    \item{nhat}{candidate sample sizes, the empirical multinomial sample
      sizes.}
    \item{candbar}{vector of candidate values, whose mean equals
      \code{sigmahat} or \code{nhat}.}
    \item{candmed}{vector of candidate values, whose median equals
      \code{sigmahat} or \code{nhat}.}
    \item{candbar1}{vector of identical candidate values, the mean of
      \code{nhat}.}
    \item{candmed1}{vector of identical candidate values, the median of
      \code{nhat}.}
  }
}
\seealso{
  \code{\link{getN}}, \code{\link{getSigmaI}}, \code{\link{getSigmaR}},
  \code{\link{estN}}, \code{\link{estSigmaI}}, and
  \code{\link{estSigmaR}} extract and estimate sample sizes and sigmas.

  \code{iterate} combines all the \code{get*} and \code{est*}
  functions in one call.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
iterate(x.cod)
iterate(x.ling)
iterate(x.oreo)
iterate(x.sbw)
}

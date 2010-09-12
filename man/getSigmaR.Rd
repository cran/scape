\name{getSigmaR}
\alias{getSigmaR}
\title{Extract Recruitment sigma}
\description{
  Extract sigma R (recruitment variability) that was used in a model, as
  indicated in the \code{Dev$sigmaR} model component.
}
\usage{
getSigmaR(model, digits=NULL)
}
\arguments{
  \item{model}{fitted \code{scape} model containing element \code{Dev}.}
  \item{digits}{digits}{number of decimal places to use when rounding,
    or \code{NULL} to suppress rounding.}
}
\value{
  Vector of two numbers, representing recruitment variability in (1) the
  estimated age composition in the first year, and (2) subsequent annual
  recruitment.
}
\note{
  Thin wrapper to access \code{model$Dev$sigmaR}, providing a uniform
  interface with other \code{get*} and \code{est*} functions.

  See discussion in the \code{\link{estSigmaR}} documentation.
}
\seealso{
  \code{\link{getN}}, \code{\link{getSigmaI}}, \code{getSigmaR},
  \code{\link{estN}}, \code{\link{estSigmaI}}, and
  \code{\link{estSigmaR}} extract and estimate sample sizes and sigmas.
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
% Statistics
\keyword{distribution}

\name{getN}
\alias{getN}
\title{Extract Sample Size}
\description{
  Extract the sample size that was used in a model, from catch-at-age or
  catch-at-length data.
}
\usage{
getN(model, what="CAc", series=NULL, digits=NULL)
}
\arguments{
  \item{model}{fitted \code{scape} model containing catch-at-age and/or
    catch-at-length data.}
  \item{what}{name of model element: \code{"CAc"}, \code{"CAs"},
    \code{"CLc"}, or \code{"CLs"}.}
  \item{series}{vector of strings indicating which gears or surveys to
    analyze (all by default).}
  \item{digits}{number of decimal places to use when rounding, or
    \code{NULL} to suppress rounding.}
}
\value{
  Numeric vector of year-specific sample sizes, or a list of such
  vectors when analyzing multiple series.
}
\note{
  Thin wrapper to access \code{model$element$SS}, providing a uniform
  interface with other \code{get*} and \code{est*} functions.

  See discussion in the \code{\link{estN}} documentation.
}
\seealso{
  \code{getN}, \code{\link{getSigmaI}}, \code{\link{getSigmaR}},
  \code{\link{estN}}, \code{\link{estSigmaI}}, and
  \code{\link{estSigmaR}} extract and estimate sample sizes and sigmas.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
## Exploring candidate sample sizes:

getN(x.sbw)  # sample sizes used in assessment: number of tows
estN(x.sbw)  # effective sample size, given data (tows) and model fit

## Multiple series:

getN(x.ling, "CLc")              # sample size used in assessment
getN(x.ling, "CLc", digits=0)    # rounded
estN(x.ling, "CLc")              # model fit implies larger sample sizes

getN(x.ling, "CLc", series="1", digits=0)  # get one series
estN(x.ling, "CLc", series="1")            # estimate one series
}
% Statistics
\keyword{distribution}

\name{getSigmaI}
\alias{getSigmaI}
\title{Extract Abundance Index Sigma}
\description{
  Extract the sigma (magnitude of observation noise) that was used in a
  model, from survey or commercial abundance index data.
}
\usage{
getSigmaI(model, what="s", series=NULL, digits=NULL)
}
\arguments{
  \item{model}{fitted \code{scape} model containing element \code{CPUE}
    and/or \code{Survey}.}
  \item{what}{which sigma to extract: \code{"c"}[ommercial] or
    \code{"s"}[urvey] abundance index.}
  \item{series}{vector of strings indicating which gears or surveys to
    analyze (all by default).}
  \item{digits}{number of decimal places to use when rounding, or
    \code{NULL} to suppress rounding.}
}
\value{
  Numeric vector of year-specific sigmas, or a list of such vectors when
  analyzing multiple series.
}
\note{
  Thin wrapper to access \code{model$element$CV}, providing a uniform
  interface with other \code{get*} and \code{est*} functions.

  See discussion in the \code{\link{estSigmaI}} documentation.
}
\seealso{
  \code{getN}, \code{\link{getSigmaI}}, \code{\link{getSigmaR}},
  \code{\link{estN}}, \code{\link{estSigmaI}}, and
  \code{\link{estSigmaR}} extract and estimate sample sizes and sigmas.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
## Exploring candidate sigmas:

getSigmaI(x.cod)  # sigma used in assessment 0.20
estSigmaI(x.cod)  # model fit implies 0.17

## Multiple series:

getSigmaI(x.oreo, "c")                 # sigma used in assessment
getSigmaI(x.oreo, "c", digits=2)       # rounded
estSigmaI(x.oreo, "c")                 # model fit implies smaller sigma

getSigmaI(x.oreo, "c", series="Series 2-1")  # get one series
estSigmaI(x.oreo, "c", series="Series 2-1")  # estimate one series
}
% Statistics
\keyword{distribution}

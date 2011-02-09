\name{scape-package}
\alias{scape-package}
\alias{scape}
\docType{package}
\title{Statistical Catch-at-Age Plotting Environment}
\description{
  Import and plot results from statistical catch-at-age models, used in
  fisheries stock assessments.
}
\details{
  \emph{Import model results:}
  \tabular{ll}{
    \code{\link{importCol}} \tab Coleraine model output
  }
  \emph{Plot model fit to data:}
  \tabular{ll}{
    \code{\link{plotCA}}    \tab catch at age\cr
    \code{\link{plotCL}}    \tab catch at length\cr
    \code{\link{plotIndex}} \tab abundance index\cr
    \code{\link{plotLA}}    \tab length at age
  }
  \emph{Plot derived quantities:}
  \tabular{ll}{
    \code{\link{plotB}}   \tab biomass, recruitment, and landings\cr
    \code{\link{plotN}}   \tab numbers at age\cr
    \code{\link{plotSel}} \tab selectivity and maturity
  }
  \emph{Sigmas and sample sizes:}
  \tabular{ll}{
    \code{\link{getN}}, \code{\link{getSigmaI}}, \code{\link{getSigmaR}}
    \tab extract\cr
    \code{\link{estN}}, \code{\link{estSigmaI}}, \code{\link{estSigmaR}}
    \tab estimate\cr
    \code{\link{iterate}} \tab combine all \code{get*} and
    \code{est*}\cr
  }
  \emph{Examples:}
  \tabular{ll}{
    \code{\link{x.cod}}, \code{\link{x.ling}}, \code{\link{x.oreo}},
    \code{\link{x.sbw}} \tab cod, ling, oreo, and whiting assessments
  }
}
\note{
  \code{browseVignettes()} shows the vignettes, found in the
  \file{scape/doc} directory.

  The functions \code{Args} and \code{ll} (package \pkg{gdata}) can be
  useful for browsing unwieldy functions and objects.
}
\author{Arni Magnusson.}
\references{
  Magnusson, A. (2005) \cite{R goes fishing: Analyzing fisheries data
    using AD Model Builder and R.} Proceedings of the 5th International
  Workshop on Distributed Statistical Computing. Available at
  \url{http://www.hafro.is/~arnima/uw/s/pdf/dsc.pdf}.
}
\seealso{
  All \pkg{scape} graphics are \code{trellis} plots, rendered with the
  \pkg{lattice} package.

  Once the best model(s) have been chosen, the \pkg{scapeMCMC} package
  can be used to diagnose MCMC runs.
}
% Graphics
\keyword{hplot}

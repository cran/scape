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
    \code{\link{importADCAM}} \tab ADCAM model results\cr
    \code{\link{importCol}}   \tab Coleraine model results
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
  \emph{Import MCMC results:}
  \tabular{ll}{
    \code{\link{importMCMC}} \tab traces of likelihoods, parameters,
    biomass and recruitment\cr
    \code{\link{importProj}} \tab medium-term projections of biomass and
    catch
  }
  \emph{Examples:}
  \tabular{ll}{
    \code{\link{x.cod}}, \code{\link{x.ling}}, \code{\link{x.oreo}},
    \code{\link{x.saithe}}, \code{\link{x.sbw}} \tab cod, ling, oreo,
    and whiting assessments\cr
    \code{\link{xmcmc}}, \code{\link{xproj}} \tab MCMC results
  }
}
\note{
  \code{browseVignettes()} shows the vignettes, found in the
  \file{scape/doc} directory.
}
\author{Arni Magnusson.}
\references{
  Magnusson, A. (2005).
  \emph{R goes fishing: Analyzing fisheries data using AD Model Builder and R.}
  Proceedings of the 4th International Workshop on Distributed Statistical
  Computing.

  Magnusson, A. and Hilborn, R. (2007).
  What makes fisheries data informative?
  \emph{Fish and Fisheries}, \bold{8}, 337--358.
  \doi{10.1111/j.1467-2979.2007.00258.x}

  Magnusson, A., Punt, A.E. and Hilborn, R. (2013).
  Measuring uncertainty in fisheries stock assessment: the delta method,
  bootstrap, and MCMC.
  \emph{Fish and Fisheries}, \bold{14}, 325--342.
  \doi{10.1111/j.1467-2979.2012.00473.x}
}
\seealso{
  Most \pkg{scape} graphics are \code{trellis} plots, rendered with the
  \pkg{lattice} package.

  The functions \code{Args} and \code{ll} (package \pkg{gdata}) can be
  useful for browsing unwieldy functions and objects.

  The \pkg{plotMCMC} package can be used to diagnose the results of MCMC
  analysis.
}

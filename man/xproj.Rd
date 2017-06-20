\name{xproj}
\alias{xproj}
\docType{data}
\title{MCMC Projections from Cod Assessment}
\description{
  Markov chain Monte Carlo projections from stock assessment of cod
  (\emph{Gadus morhua}) in Icelandic waters.
}
\usage{xproj}
\format{
  List containing two lists:
  \tabular{ll}{
    \code{B}\tab projected biomass by year, given a constant harvest
    rate policy: \code{"0"}, \code{"0.05"}, \ldots, \code{"0.50"}.\cr
    \code{Y}\tab projected catch by year, given a constant harvest rate
    policy: \code{"0"}, \code{"0.05"}, \ldots, \code{"0.50"}.\cr
  }
}
\details{
  MCMC projections can be used to evaluate the medium-term outcome of
  harvest policies (in this case, a certain fixed harvest rate), given
  the uncertainty about parameter values and random future recruitment.

  See the \code{\link{x.cod}} help page for details about the data and
  model.
}
\note{
  The list was imported from the files \file{strategy.out},
  \file{projspbm.out} and \file{procatch.out}, using the
  \code{importProj()} function. These files can be found in the
  \file{scape/example/mcmc} directory.

  The functions \code{ll} (package \pkg{gdata}) and \code{head}
  are recommended for browsing nested objects, e.g. \code{ll(xproj)},
  \code{ll(xproj$B)}, \code{ll(xproj$B$"0.25")}, and
  \code{head(xproj$B$"0.25")}.

  The \pkg{plotMCMC} package is recommended for plotting MCMC
  diagnostics and posteriors.
}
\references{
  Magnusson, A., Punt, A. E., and Hilborn, R. (2013) Measuring
  uncertainty in fisheries stock assessment: the delta method,
  bootstrap, and MCMC. \emph{Fish and Fisheries} \bold{14}, 325--342.
}
\seealso{
  \code{\link{importProj}} was used to import the MCMC projections.

  \code{\link{xmcmc}} and \code{xproj} are MCMC results and projections
  for the \code{\link{x.cod}} model run.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
# See examples in package 'plotMCMC'
}

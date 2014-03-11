\name{xmcmc}
\alias{xmcmc}
\docType{data}
\title{MCMC Results from Cod Assessment}
\description{
  Markov chain Monte Carlo results from stock assessment of cod
  (\emph{Gadus morhua}) in Icelandic waters.
}
\usage{xmcmc}
\format{
  List containing four data frames:
  \tabular{ll}{
    \code{L}\tab likelihood components: \code{CAc} (commercial catch at
    age), \code{CAs} (survey catch at age), \code{Survey} (survey
    abundance index), \code{Prior} (penalty for priors), \code{Total}
    (sum of all likelihood components; the objective function).\cr
    \code{P}\tab estimated parameters: \code{R0} (average virgin
    recruitment), \code{Rinit} (initial recruitment scaler),
    \code{uinit} (initial harvest rate), \code{cSleft} (left-side slope
    of commercial selectivity curve), \code{cSfull} (age at full
    commercial selectivity), \code{sSleft} (left-side slope of survey
    selectivity curve), \code{sSfull} (age at full survey selectivity),
    \code{logq} (log-transformed survey catchability).\cr
    \code{B}\tab predicted biomass (age 4+) by year.\cr
    \code{R}\tab predicted recruitment by year.\cr
  }
}
\details{
  See the \code{\link{x.cod}} help page for details about the data and
  model.
}
\note{
  The list was imported from the files \file{mcmclike.out},
  \file{params.pst}, \file{spawbiom.pst} and \file{recruits.pst}, using
  the \code{importMCMC()} function. These files can be found in the
  \file{scape/example/mcmc} directory.

  The functions \code{ll} (package \pkg{gdata}) and \code{head}
  are recommended for browsing nested objects, e.g. \code{ll(xmcmc)},
  \code{ll(xmcmc$P)}, and \code{head(xmcmc$P)}.

  The \pkg{plotMCMC} package is recommended for plotting MCMC
  diagnostics and posteriors.
}
\references{
  Magnusson, A., Punt, A. E., and Hilborn, R. (2013) Measuring
  uncertainty in fisheries stock assessment: the delta method,
  bootstrap, and MCMC. \emph{Fish and Fisheries} \bold{14}, 325--342.
}
\seealso{
  \code{\link{importMCMC}} was used to import the MCMC results.

  \code{xmcmc} and \code{\link{xproj}} are MCMC results and projections
  for the \code{\link{x.cod}} model run.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
# See examples in package 'plotMCMC'
}
\keyword{datasets}

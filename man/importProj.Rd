\name{importProj}
\alias{importProj}
\title{Import Coleraine MCMC Projections}
\description{
  Import Coleraine MCMC traces for spawning biomass and catch, projected
  into the near future.
}
\usage{
importProj(dir, coda=FALSE, quiet=TRUE)
}
\arguments{
  \item{dir}{directory containing the files \file{strategy.out},
    \file{projspbm.pst} and \file{procatch.pst}.}
  \item{coda}{whether data frames should be coerced to class \code{mcmc}
    using the \pkg{coda} package.}
  \item{quiet}{whether to report progress while parsing files in
    directory.}
}
\value{
  A list containing:
  \item{B}{biomass by catch policy and year}
  \item{Y}{catch by catch policy and year}
  as lists of data frames, or \code{mcmc} objects if \code{coda=TRUE}.
}
\note{
  MCMC projections can be used to evaluate the short-term outcome of
  harvest policies (constant catch or constant harvest rate), given the
  uncertainty about parameter values and random future recruitment.

  The functions \code{ll} (package \pkg{gdata}) and \code{head} are
  recommended for browsing nested objects, e.g. \code{ll(xproj)},
  \code{ll(xproj$B)}, \code{ll(xproj$B$"0.2")}, and
  \code{head(xproj$B$"0.2")}.

  The \pkg{plotMCMC} package is recommended for plotting MCMC
  diagnostics and posteriors.
}
\references{
  Hilborn, R., Maunder, M., Parma, A., Ernst, B., Payne, J., and Starr,
  P. (2003) \emph{Coleraine: A generalized age-structured stock
    assessment model.} User's manual version 2.0. University of
  Washington Report SAFS-UW-0116. Available at
  \url{http://fish.washington.edu/research/coleraine/pdf/coleraine.pdf}.
}
\seealso{
  \code{\link{read.table}}, \code{\link{readLines}}, and
  \code{\link{scan}} import any data.

  \code{\link{importMCMC}} and \code{importProj} import Coleraine MCMC
  results.

  \code{\link{xproj}} was created using \code{importProj}.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
\dontrun{
dir <- system.file("example/mcmc", package="scape")
xproj <- importProj(dir)
}
}
% Programming
\keyword{file}
\keyword{interface}

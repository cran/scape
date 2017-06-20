\name{importADCAM}
\alias{importADCAM}
\title{Import ADCAM Model Results}
\description{
  Import ADCAM model results into a standard format suitable for
  plotting.
}
\usage{
importADCAM(dir)
}
\arguments{
  \item{dir}{directory containing the results files
    (\file{resultsbyyearandage}, \file{resultsbyyear},
    \file{resultsbyage}), as well as the input file
    \file{catchparameters} specifying selectivity periods.}
}
\value{
  A list of class \code{scape} containing the following elements:
  \item{N}{predicted numbers at age}
  \item{B}{predicted biomass, recruitment, and observed landings (year
    things)}
  \item{Sel}{predicted selectivity and observed maturity (age things)}
  \item{Dev}{predicted recruitment deviates from the stock-recruitment
    curve}
  \item{Survey}{survey abundance index and fit}
  \item{CAc}{commercial C@A (catch at age) and fit}
  \item{CAs}{survey C@A (catch at age) and fit}
}
\note{
  \code{B$VB} is reference biomass.

  \code{Survey$CV} is \code{0} to give \code{plotIndex} sensible y-axis
  limits.

  \code{CAc$SS} and \code{CAs$SS} are the
  \code{log(obs+const) - log(fit+const)} residuals used in the ADCAM
  likelihood.
}
\references{
  Bjornsson, H. and A. Magnusson. (2009) \emph{ADCAM user manual (draft
    version).} ICES CM 2009/ACOM:56 Annex 6.
}
\seealso{
  \code{\link{read.table}}, \code{\link{readLines}}, and
  \code{\link{scan}} import any data.

  \code{\link{x.saithe}} was created using \code{importADCAM}.

  \code{\link{importCol}} imports Coleraine model results.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
path <- system.file("example/saithe", package="scape")
saithe <- importADCAM(path)
}

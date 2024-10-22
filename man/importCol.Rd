\name{importCol}
\alias{importCol}
\title{Import Coleraine Model Results}
\description{
  Import Coleraine model results from \file{.res} file, and rearrange
  into a standard format suitable for plotting.
}
\usage{
importCol(res.file, Dev=FALSE, CPUE=FALSE, Survey=FALSE, CAc=FALSE,
          CAs=FALSE, CLc=FALSE, CLs=FALSE, LA=FALSE, quiet=TRUE)
}
\arguments{
  \item{res.file}{name of Coleraine model results file to import.}
  \item{Dev}{whether recruitment deviates were estimated in model.}
  \item{CPUE}{whether model was fitted to catch-per-unit-effort data.}
  \item{Survey}{whether model was fitted to survey abundance index
    data.}
  \item{CAc}{whether model was fitted to commercial catch-at-age data.}
  \item{CAs}{whether model was fitted to survey catch-at-age data.}
  \item{CLc}{whether model was fitted to commercial catch-at-length
    data.}
  \item{CLs}{whether model was fitted to survey catch-at-length data.}
  \item{LA}{whether model was fitted to length-at-age data.}
  \item{quiet}{whether to report progress while parsing file.}
}
\value{
  A list of class \code{scape} containing at least \code{N}, \code{B},
  and \code{Sel}. The other elements may or may not be included in the
  list, depending on how \code{importCol} was called:
  \item{N}{predicted numbers at age}
  \item{B}{predicted biomass, recruitment, and observed landings (year
    things)}
  \item{Sel}{predicted selectivity and observed maturity (age things)}
  \item{Dev}{predicted recruitment deviates from the stock-recruitment
    curve}
  \item{CPUE, Survey}{commercial and survey abundance index and fit}
  \item{CAc, CAs}{commercial and survey C@A (catch at age) and fit}
  \item{CLc, CLs}{commercial and survey C@L (catch at length) and fit}
  \item{LA}{observed L@A and fit}
}
\note{
  This import function is implemented for the Coleraine statistical
  catch-at-age software, and can serve as a template for \pkg{scape}
  users who would like to implement import functions for specific stock
  assessment software.

  The functions \code{ll} (package \pkg{gdata}) and \code{head} are
  recommended for browsing nested objects, e.g. \code{ll(x.cod)},
  \code{ll(x.cod$N)}, and \code{head(x.cod$N)}.
}
\references{
  Hilborn, R., Maunder, M., Parma, A., Ernst, B., Payne, J., and Starr, P.
  (2003).
  \emph{Coleraine: A generalized age-structured stock assessment model.}
  User's manual version 2.0.
  University of Washington Report SAFS-UW-0116.
}
\seealso{
  \code{\link{read.table}}, \code{\link{readLines}}, and
  \code{\link{scan}} import any data.

  \code{\link{x.cod}}, \code{\link{x.ling}}, \code{\link{x.oreo}}, and
  \code{\link{x.sbw}} were created using \code{importCol}.

  \code{\link{importADCAM}} imports ADCAM model results.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
path <- system.file("example/cod.res", package="scape")
cod <- importCol(path, Dev=TRUE, Survey=TRUE, CAc=TRUE, CAs=TRUE)
}

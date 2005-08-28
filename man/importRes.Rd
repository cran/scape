\name{importRes}
\alias{importRes}
\title{
  Import Coleraine Model Results
}
\description{
  Import Coleraine model results from .res file, and rearrange into a
  standard format suitable for plotting.
}
\usage{
importRes(res.file, info="", Dev=FALSE, CPUE=FALSE, Survey=FALSE,
          CAc=FALSE, CAs=FALSE, CLc=FALSE, CLs=FALSE, LA=FALSE,
          verbose=FALSE)
}
\arguments{
  \item{res.file}{name of Coleraine model results file to import.}
  \item{info}{optional string containing information to store with model
    results.}
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
  \item{verbose}{whether to report progress while parsing file.}
}
\value{
  A list of class \code{scape} containing at least \code{N}, \code{B},
  and \code{Sel}. The other elements may or may not be included in the
  list, depending on how \code{importRes} was called:
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
\references{
  \cite{Hilborn, R., M. Maunder, A. Parma, B. Ernst, J. Payne, and P.
    Starr. 2003. Coleraine: A generalized age-structured stock
    assessment model. User's manual version 2.0. University of
    Washington Report SAFS--UW--0116.} Available at
  http://fish.washington.edu/research/coleraine/coleraine.pdf.
}
\note{
  This import function is implemented for the Coleraine statistical
  catch-at-age software, and can serve as a template for \pkg{scape}
  users who would like to implement import functions for specific stock
  assessment software.

  The example datasets \code{x.cod}, \code{x.ling}, \code{x.oreo}, and
  \code{x.sbw} were generated using \code{importRes}.

  The functions \code{ll} (package \pkg{gdata}) and \code{head} are
  recommended for browsing \code{scape} objects and their elements, e.g.
  \code{ll(x.cod); ll(x.cod$N); head(x.cod$N)}.
}
\author{Arni Magnusson \email{arnima@u.washington.edu}}
\seealso{
  The imported model results can be plotted using \code{\link{plotB}},
  \code{\link{plotCA}}, \code{\link{plotCL}}, \code{\link{plotIndex}},
  \code{\link{plotLA}}, \code{\link{plotN}}, and \code{\link{plotSel}}.

  \code{\link{read.table}}, \code{\link{readLines}}, and
  \code{\link{scan}} are commonly used to import data from text files.
}
\examples{
\dontrun{
path <- paste(.find.package("scape"), "/example/cod.res", sep="")
x.cod <- importRes(path, dev=T, Survey=T, CAc=T, CAs=T)
}
}
% Programming, Input/Ouput, and Miscellaneous
\keyword{file}

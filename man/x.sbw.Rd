\name{x.sbw}
\docType{data}
\alias{x.sbw}
\title{Whiting Assessment}
\description{
  Stock assessment data and model fit for southern blue whiting
  (\emph{Micromesistius australis}) in New Zealand waters, using a
  Coleraine statistical catch-at-age model.

  This is a single-sex model with 11 age classes, the catch data
  starting in 1979 and ending in 2002. The model was fitted to two data
  components: survey abundance index and commercial catch at age.
}
\usage{x.sbw}
\format{
  List of class \code{scape} containing:
  \tabular{ll}{
    \code{N}\tab predicted numbers at age\cr
    \code{B}\tab predicted biomass, recruitment, and observed landings
    (year things)\cr
    \code{Sel}\tab predicted selectivity and observed maturity (age
    things)\cr
    \code{Dev}\tab predicted recruitment deviates from the
    stock-recruitment curve\cr
    \code{Survey}\tab survey abundance index and fit\cr
    \code{CAc}\tab commercial C@A (catch at age) and fit\cr
  }
}
\details{
  Hilborn et al. (2003) give a general description of the Coleraine
  generalized model.

  The survey abundance index was preprocessed so that it contains only
  age 4 and older.

  Estimated parameters: R0, Rinit, Rplus, Sleft[commercial], Sfull[c],
  q, and 33 recruitment deviates.
}
\note{
  The list was imported from the file \file{scape/example/sbw.res} using
  \code{importCol}.

  The functions \code{ll} (package \pkg{gdata}) and \code{head} are
  recommended for browsing nested objects, e.g. \code{ll(x.sbw)},
  \code{ll(x.sbw$N)}, and \code{head(x.sbw$N)}.
}
\source{
  Annala, J. H., Sullivan, K. J., O'Brien, C. J., Smith, N. W. M. and
  Grayling S. M., eds. (2003) \emph{Report from the Fishery Assessment
    Plenary: Stock assessments and yield estimates.} Ministry of
  Fisheries, Wellington.
}
\references{
  Branch, T. A., Magnusson, A., Hilborn, R. and Starr, P. J. (2002)
  \emph{Stock assessment of the Campbell Island Rise population of
    southern blue whiting (Micromesistius australis) for the 2000--01
  fishing season}. University of Washington Report SAFS-UW-0107.

  Hilborn, R., Maunder, M., Parma, A., Ernst, B., Payne, J. and Starr,
  P. (2003) \emph{Coleraine: A generalized age-structured stock
    assessment model.} User's manual version 2.0. University of
  Washington Report SAFS-UW-0116.

  Magnusson, A. and Hilborn, R. 2004. \emph{What is it in fisheries data
    that tells us about population abundance?} Poster presented at the
  4th World Fisheries Congress, Vancouver, BC.
}
\seealso{
  \code{\link{importCol}} was used to import the fitted model.

  \code{\link{x.cod}}, \code{\link{x.ling}}, \code{\link{x.oreo}},
  \code{\link{x.saithe}}, and \code{x.sbw} are fitted \code{scape}
  models to explore.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
plotB(x.sbw)
plotCA(x.sbw, "c")
plotIndex(x.sbw, "s")
plotN(x.sbw)
plotSel(x.sbw)
}

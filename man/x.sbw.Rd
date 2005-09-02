\name{x.sbw}
\docType{data}
\alias{x.sbw}
\title{Whiting assessment}
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
  The survey abundance index was preprocessed so that it contains only
  age 4 and older.

  Estimated parameters: R0, Rinit, Rplus, Sleft[commercial], Sfull[c],
  q, and 33 recruitment deviates.
}
\note{
  The list was imported from the file \code{scape/example/sbw.res} using
  \code{importRes}.
}
\source{
  \cite{Annala, J.H., K.J. Sullivan, C.J. O'Brien, N.W.M. Smith, and
    S.M. Grayling (eds.) 2003. Report from the Fishery Assessment
    Plenary: Stock assessments and yield estimates. Wellington: Ministry
    of Fisheries.} Available from NIWA library, Wellington.}
\references{
  \cite{Hilborn, R., M. Maunder, A. Parma, B. Ernst, J. Payne, and P.
    Starr. 2003. Coleraine: A generalized age-structured stock
    assessment model. User's manual version 2.0. University of
    Washington Report SAFS--UW--0116.} Available at
  http://fish.washington.edu/research/coleraine/coleraine.pdf.

  \cite{Magnusson, A. and R. Hilborn. 2004. What is it in fisheries data
    that tells us about population abundance? Poster presented at the
    4th World Fisheries Congress, Vancouver, BC.} Available from the
  author.
}
\seealso{
  \code{\link{importRes}}, \code{\link{x.cod}}, \code{\link{x.ling}},
  \code{\link{x.oreo}}
}
\examples{
plotB(x.sbw)
plotCA(x.sbw, "c")
plotIndex(x.sbw, "s")
plotN(x.sbw)
plotSel(x.sbw)
}
\keyword{datasets}

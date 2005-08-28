\name{x.ling}
\docType{data}
\alias{x.ling}
\title{Ling assessment}
\description{
  Stock assessment data and model fit for ling (\emph{Genypterus
    blacodes}) in New Zealand waters, using a Coleraine statistical
  catch-at-age model.

  This is a two-sex model with 30 age classes and 29 length classes, the
  catch data starting in 1973 and ending in 2000. The model was fitted
  to five data components: longline abundance index, survey abundance
  index, survey catch at age, longline catch at length, and trawl catch
  at length.
}
\usage{x.ling}
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
    \code{CPUE}\tab commercial abundance index and fit\cr
    \code{Survey}\tab survey abundance index and fit\cr
    \code{CAs}\tab survey C@A (catch at age) and fit\cr
    \code{CLc}\tab commercial C@L (catch at length) and fit\cr
  }
}
\details{
  Estimated parameters: R0, Rinit, Sleft[trawl], Sfemale[t], Smale[t],
  Sright[t], Sleft[longline], Sfemale[l], Smale[l], Sright[l],
  Sleft[survey], Sfemale[s], Smale[s], Sright[s], q[l], q[s], and 29
  recruitment deviates.
}
\note{
  The list was imported from the file \code{scape/example/ling.res}
  using \code{importRes}.
}
\source{
  \cite{Annala, J.H., K.J. Sullivan, C.J. O'Brien, and N.W.M. Smith.
    (eds.) 2001. Report from the Fishery Assessment Plenary: Stock
    assessments and yield estimates. Wellington: NIWA.} Available from
  NIWA library, Wellington.}
\references{
  \cite{Hilborn, R., M. Maunder, A. Parma, B. Ernst, J. Payne, and P.
    Starr. 2003. Coleraine: A generalized age-structured stock
    assessment model. User's manual version 2.0. University of
    Washington Report SAFS--UW--0116.} Available at
  http://fish.washington.edu/research/coleraine/coleraine.pdf.

  \cite{Magnusson, A. 2001. SeaFIC assessment of Chatham Rise ling (LIN
    3 and 4). Middle Depths Working Group Doc. 11. Report for the New
    Zealand Ministry of Fisheries.} Available from the author.
}
\seealso{
  \code{\link{importRes}}, \code{\link{x.cod}}, \code{\link{x.oreo}},
  \code{\link{x.sbw}}
}
\examples{
plotB(x.ling)
plotCA(x.ling, "s")
plotCL(x.ling, "c", series="1")
plotCL(x.ling, "c", series="2")
plotIndex(x.ling, "c")
plotIndex(x.ling, "s")
plotN(x.ling)
plotSel(x.ling)
}
\keyword{datasets}

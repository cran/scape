\name{x.ling}
\docType{data}
\alias{x.ling}
\title{Ling Assessment}
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
  Hilborn et al. (2003) give a general description of the Coleraine
  generalized model.

  Estimated parameters: R0, Rinit, Sleft[trawl], Sfemale[t], Smale[t],
  Sright[t], Sleft[longline], Sfemale[l], Smale[l], Sright[l],
  Sleft[survey], Sfemale[s], Smale[s], Sright[s], q[l], q[s], and 29
  recruitment deviates.
}
\note{
  The list was imported from the file \file{scape/example/ling.res}
  using \code{importCol}.

  The functions \code{ll} (package \pkg{gdata}) and \code{head} are
  recommended for browsing nested objects, e.g. \code{ll(x.ling)},
  \code{ll(x.ling$N)}, and \code{head(x.ling$N)}.
}
\source{
  Annala, J. H., Sullivan, K. J., O'Brien, C. J., and Smith, N. W. M.,
  eds. (2001) \emph{Report from the Fishery Assessment Plenary: Stock
    assessments and yield estimates.} Wellington: NIWA. Available from
  NIWA library, Wellington.

  Magnusson, A. (2001) \emph{SeaFIC assessment of Chatham Rise ling (LIN
    3 and 4).} Middle Depths Working Group Doc. 11. Report for the New
  Zealand Ministry of Fisheries. Available at
  \url{http://www.hafro.is/~arnima/pdf/2001-ling.pdf}.
}
\references{
  Hilborn, R., Maunder, M., Parma, A., Ernst, B., Payne, J., and Starr,
  P. (2003) \emph{Coleraine: A generalized age-structured stock
    assessment model.} User's manual version 2.0. University of
  Washington Report SAFS-UW-0116. Available at
  \url{http://fish.washington.edu/research/coleraine/pdf/coleraine.pdf}.
}
\seealso{
  \code{\link{importCol}} was used to import the fitted model.

  \code{\link{x.cod}}, \code{x.ling}, \code{\link{x.oreo}}, and
  \code{\link{x.sbw}} are fitted \code{scape} models to explore.

  \code{\link{scape-package}} gives an overview of the package.
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

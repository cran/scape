\name{x.oreo}
\docType{data}
\alias{x.oreo}
\title{Oreo Assessment}
\description{
  Stock assessment data and model fit for smooth oreo
  (\emph{Pseudocyttus maculatus}) in New Zealand waters, using a
  Coleraine statistical catch-at-age model.

  This is a two-sex model with 80 age classes and 44 length classes, the
  catch data starting in 1979 and ending in 2001. The model was fitted
  to seven data components: pre-GPS commercial abundance index, post-GPS
  commercial abundance index, survey abundance index, commercial catch
  at length, survey catch at length, female length at age, and male
  length at age.
}
\usage{x.oreo}
\format{
  List of class \code{scape} containing:
  \tabular{ll}{
    \code{N}\tab predicted numbers at age\cr
    \code{B}\tab predicted biomass, recruitment, and observed landings
    (year things)\cr
    \code{Sel}\tab predicted selectivity and observed maturity (age
    things)\cr
    \code{CPUE}\tab commercial abundance index and fit\cr
    \code{Survey}\tab survey abundance index and fit\cr
    \code{CLc}\tab commercial C@L (catch at length) and fit\cr
    \code{CLs}\tab survey C@L and fit\cr
    \code{LA}\tab observed L@A (length at age)\cr
  }
}
\details{
  Hilborn et al. (2003) give a general description of the Coleraine
  generalized model.

  Since relatively few smooth oreo individuals have been aged, this
  assessment admits uncertainty about the von Bertalanffy growth curve,
  which is estimated for each sex. The acoustic survey abundance
  estimate is considered absolute, so q[survey] is fixed at 1.

  Estimated parameters: R0, Sfull[commercial], Sfull[survey],
  q[pre-GPS], q[post-GPS], L80female, L80male, Kfemale, Kmale, CVfemale,
  and CVmale.
}
\note{
  The list was imported from the files \file{scape/example/oreo.res},
  \file{oreo.txt}, and \file{l\_at\_age.dat} using \code{importCol}.

  The functions \code{ll} (package \pkg{gdata}) and \code{head} are
  recommended for browsing nested objects, e.g. \code{ll(x.oreo)},
  \code{ll(x.oreo$N)}, and \code{head(x.oreo$N)}.
}
\source{
  Annala, J. H., Sullivan, K. J., O'Brien, C. J., Smith, N. W. M., and
  Grayling S. M., eds. (2003) \emph{Report from the Fishery Assessment
    Plenary: Stock assessments and yield estimates.} Wellington:
  Ministry of Fisheries. Available from NIWA library, Wellington.
}
\references{
  Hilborn, R., Maunder, M., Parma, A., Ernst, B., Payne, J., and Starr,
  P. (2003) \emph{Coleraine: A generalized age-structured stock
    assessment model.} User's manual version 2.0. University of
  Washington Report SAFS-UW-0116. Available at
  \url{http://fish.washington.edu/research/coleraine/pdf/coleraine.pdf}.

  Magnusson, A. (2003) \emph{Stock assessment of Chatham Rise smooth
    oreo (SSO4).} Deepwater Working Group Doc. 16. Report for the New
    Zealand Ministry of Fisheries. Available at
    \url{http://www.hafro.is/~arnima/pdf/2003-oreo.pdf}.
}
\seealso{
  \code{\link{importCol}} was used to import the fitted model.

  \code{\link{x.cod}}, \code{\link{x.ling}}, \code{x.oreo}, and
  \code{\link{x.sbw}} are fitted \code{scape} models to explore.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
plotB(x.oreo)
plotCL(x.oreo, "c")
plotCL(x.oreo, "s")
plotIndex(x.oreo, "c")
plotIndex(x.oreo, "s")
plotLA(x.oreo)
plotN(x.oreo)
plotSel(x.oreo)
}
\keyword{datasets}

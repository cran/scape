\name{x.oreo}
\docType{data}
\alias{x.oreo}
\title{Oreo assessment}
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
  Since relatively few smooth oreo individuals have been aged, this
  assessment admits uncertainty about the von Bertalanffy growth curve,
  which is estimated for each sex. The acoustic survey abundance
  estimate is considered absolute, so q[survey] is fixed at 1.

  Estimated parameters: R0, Sfull[commercial], Sfull[survey],
  q[pre-GPS], q[post-GPS], L80female, L80male, Kfemale, Kmale, CVfemale,
  and CVmale.
}
\note{
  The list was imported from the files \code{scape/example/oreo.res},
  \code{oreo.txt}, and \code{l_at_age.dat} using \code{importRes}.
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

  \cite{Magnusson, A. 2003. Stock assessment of Chatham Rise smooth oreo
    (SSO4). Deepwater Working Group Doc. 16. Report for the New Zealand
    Ministry of Fisheries.} Available from the author.
}
\seealso{
  \code{\link{importRes}}, \code{\link{x.cod}}, \code{\link{x.ling}},
  \code{\link{x.sbw}}
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

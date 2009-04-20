\name{x.cod}
\docType{data}
\alias{x.cod}
\title{Cod Assessment}
\description{
  Stock assessment data and model fit for cod (\emph{Gadus morhua}) in
  Icelandic waters, using a Coleraine statistical catch-at-age model.

  This is a single-sex model with 10 age classes, the catch data
  starting in 1971 and ending in 2003. The model was fitted to three
  data components: survey abundance index, commercial catch at age, and
  survey catch at age.
}
\usage{x.cod}
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
    \code{CAs}\tab survey C@A and fit\cr
  }
}
\details{
  A maturity vector of zeros and ones was used to predict the biomass of
  age 4 and older, the quantity of main interest for the management of
  this stock.

  Estimated parameters: R0, Rinit, uinit, Sleft[commercial], Sfull[c],
  Sleft[survey], Sfull[s], q, and 41 recruitment deviates.
}
\note{
  The list was imported from the file \file{scape/example/cod.res} using
  \code{importCol}.

  The functions \code{ll} (package \pkg{gdata}) and \code{head} are
  recommended for browsing model results, e.g. \code{ll(x.cod);
    ll(x.cod$N); head(x.cod$N)}.
}
\source{
  Marine Research Institute. 2003. \cite{State of marine stocks in
    Icelandic waters 2002/2003.} Available at
  http://www.hafro.is/Astand/2003/astand-allt-03.pdf.
}
\references{
  Hilborn, R., M. Maunder, A. Parma, B. Ernst, J. Payne, and P. Starr.
  2003. \cite{Coleraine: A generalized age-structured stock assessment
  model.} User's manual version 2.0. University of Washington Report
  SAFS-UW-0116. Available at
  http://fish.washington.edu/research/coleraine/coleraine.pdf.

  Magnusson, A. 2003. \cite{Coleraine assessment of the Icelandic cod
  stock.} Report for the Icelandic Marine Research Institute. Available
  from the author.
}
\seealso{
  \code{\link{importCol}}, \code{\link{x.ling}}, \code{\link{x.oreo}},
  \code{\link{x.sbw}}.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
plotB(x.cod)
plotCA(x.cod, "c")
plotCA(x.cod, "s")
plotIndex(x.cod, "s")
plotN(x.cod)
plotSel(x.cod)
}
\keyword{datasets}

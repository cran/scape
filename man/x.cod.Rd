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
  Hilborn et al. (2003) give a general description of the Coleraine
  generalized model. Magnusson and Hilborn (2007) describe the model
  used in this assessment.

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
  recommended for browsing nested objects, e.g. \code{ll(x.cod)},
  \code{ll(x.cod$N)}, and \code{head(x.cod$N)}.
}
\source{
  Magnusson, A. (2003) \emph{Coleraine assessment of the Icelandic cod
    stock.} ICES North Western Working Group (NWWG) WD 31.

  Marine Research Institute (2003) \emph{State of marine stocks in
    Icelandic waters 2002/2003.} Marine Research in Iceland 97.
}
\references{
  Hilborn, R., Maunder, M., Parma, A., Ernst, B., Payne, J., and Starr, P.
  (2003).
  \emph{Coleraine: A generalized age-structured stock assessment model.}
  User's manual version 2.0.
  University of Washington Report SAFS-UW-0116.

  Magnusson, A. and Hilborn, R. (2007).
  What makes fisheries data informative?
  \emph{Fish and Fisheries}, \bold{8}, 337--358.
  \doi{10.1111/j.1467-2979.2007.00258.x}
}
\seealso{
  \code{\link{importCol}} was used to import the fitted model.

  \code{x.cod}, \code{\link{x.ling}}, \code{\link{x.oreo}},
  \code{\link{x.saithe}}, and \code{\link{x.sbw}} are fitted
  \code{scape} models to explore.

  \code{\link{xmcmc}} and \code{\link{xproj}} are MCMC results and
  projections for the \code{x.cod} model run.

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

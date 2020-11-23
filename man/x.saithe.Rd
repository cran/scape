\name{x.saithe}
\docType{data}
\alias{x.saithe}
\title{Saithe Assessment}
\description{
  Stock assessment data and model fit for saithe
  (\emph{Pollachius virens}) in Icelandic waters, using an ADCAM-type
  (separable) catch-at-age model.

  This is a single-sex model with 14 age classes, the catch data
  starting in 1980 and ending in 2014. The model was fitted to two data
  components: survey abundance index and commercial catch at age.
}
\usage{x.saithe}
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
  Bjornsson and Magnusson (2009) give a general description of the ADCAM
  modelling platform. The specific model used in this assessment is
  described in the ICES (2015) assessment report.

  Fleet selectivity is modelled with three periods: 1980-1996,
  1997-2003, and 2004 onwards.
}
\note{
  The list was imported from results files in the directory
  \file{scape/example/saithe} using \code{importADCAM}.

  The functions \code{ll} (package \pkg{gdata}) and \code{head} are
  recommended for browsing nested objects, e.g. \code{ll(x.saithe)},
  \code{ll(x.saithe$N)}, and \code{head(x.saithe$N)}.
}
\source{
  ICES (2015) \emph{Report of the North-Western Working Group (NWWG).}
  ICES CM 2015/ACOM:07.
}
\references{
  Bjornsson, H. and A. Magnusson. (2009) \emph{ADCAM user manual (draft
    version).} ICES CM 2009/ACOM:56, Annex 6.
}
\seealso{
  \code{\link{importADCAM}} was used to import the fitted model.

  \code{\link{x.cod}}, \code{\link{x.ling}}, \code{\link{x.oreo}},
  \code{x.saithe}, and \code{\link{x.sbw}} are fitted \code{scape}
  models to explore.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
plotB(x.saithe)
plotCA(x.saithe, "c")
plotCA(x.saithe, "s")
plotIndex(x.saithe, "s")
plotN(x.saithe)
plotSel(x.saithe)
}

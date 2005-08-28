\name{plotB}
\alias{plotB}
\concept{Fisheries stock assessment}
\title{Plot Biomass, Recruitment, and Landings}
\description{
  Plot \code{scape} model predicted biomass, stock recruitment, and
  landings.
}
\usage{
plotB(model, what="d", series=NULL, years=NULL, axes=TRUE, div=1,
      legend="bottom", main="", xlab="", ylab="", cex.main=1.2,
      cex.legend=1, cex.lab=1, cex.axis=0.8, las=1,
      tck=c(1,what=="d")/2, tick.number=5, lty.grid=3, col.grid="white",
      pch=16, cex.points=0.8, col.points="black", lty.lines=1:3,
      lwd.lines=2, col.lines="black", ratio.bars=3, col.bars="grey",
      plot.it=TRUE, ...)
}
\arguments{
  \item{model}{fitted \code{scape} model.}
  \item{what}{what should be plotted: \code{"d"}[efault],
    \code{"s"}[tock recruitment], or \code{"l"}[andings].}
  \item{series}{vector of strings indicating which column names in
    \code{model$B} data frame should be plotted (all by default).}
  \item{years}{vector of numbers indicating which years should be
    included (all by default).}
  \item{axes}{whether axis values should be plotted.}
  \item{div}{denominator to shorten values on y axis, or a vector with
    two elements referring to x  and y axis.}
  \item{legend}{legend location: \code{"bottom"}, \code{"left"},
    \code{"top"}, \code{"right"}, or \code{""} to suppress legend.}
  \item{main}{main title.}
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}
  \item{cex.main}{size of main title.}
  \item{cex.legend}{size of legend text.}
  \item{cex.lab}{size of axis labels.}
  \item{cex.axis}{size of tick labels.}
  \item{las}{orientation of tick labels: 0=parallel, 1=horizontal,
    2=perpendicular, 3=vertical.}
  \item{tck}{tick mark length.}
  \item{tick.number}{number of tick marks.}
  \item{lty.grid}{line type of gridlines.}
  \item{col.grid}{colour of gridlines.}
  \item{pch}{symbol for points.}
  \item{cex.points}{size of points.}
  \item{col.points}{colour of points.}
  \item{lty.lines}{line type of main lines, possibly a vector.}
  \item{lwd.lines}{line width of main lines, possibly a vector.}
  \item{col.lines}{colour of main lines, possibly a vector.}
  \item{ratio.bars}{width of bars.}
  \item{col.bars}{colour of bars.}
  \item{plot.it}{whether to draw plot.}
  \item{\dots}{passed to \code{xyplot}.}
}
\details{
  The \code{"d"}[efault] plot shows spawning biomass and vulnerable
  biomass as lines, and landings as bars, on the same scale.
}
\value{
  When \code{plot.it=TRUE}, a trellis plot is drawn and a data frame is
  returned, containing the data used for plotting. When
  \code{plot.it=FALSE}, a trellis object is returned.
}
\author{Arni Magnusson \email{arnima@u.washington.edu}}
\note{
  This function tries to draw the plot on a trellis device with a white
  background.

  The \code{Args} function from the \pkg{gdata} package is recommended
  for reviewing the arguments, instead of \code{args}.
}
\seealso{
  Fitted Coleraine models can be imported using \code{\link{importRes}}.

  Trellis plot details can be studied from the \code{\link{Lattice}}
  help page.
}
\examples{
plotB(x.ling, series=c("VB.1","VB.2","Y"), div=1000, xlab="Year\n",
      ylab="Biomass and landings (1000 t)")

plotB(x.ling, "s", div=1000, xlab="Biomass age 4+ (1000 t)",
      ylab="Recruitment (million one-year-olds)")
}
% Graphics
\keyword{hplot}

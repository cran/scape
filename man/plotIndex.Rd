\name{plotIndex}
\alias{plotIndex}
\concept{Fisheries stock assessment}
\title{Plot Abundance Index}
\description{Plot \code{scape} model fit to abundance index data}
\usage{
plotIndex(model, what="c", series=NULL, axes=TRUE, same.limits=FALSE,
          between=list(x=axes,y=axes), ylim=NULL, q=1, bar=1,
          log.transform=FALSE, base.log=10, main="", xlab="", ylab="",
          cex.main=1.2, cex.lab=1, cex.strip=0.8, cex.axis=0.8, las=1,
          tck=c(1,0)/2, tick.number=5, lty.grid=3, col.grid="white",
          pch=16, cex.points=1.2, col.points="black", lty.lines=1,
          lwd.lines=4, col.lines="dimgrey", lty.bar=1, plot.it=TRUE,
          ...)
}
\arguments{
  \item{model}{fitted \code{scape} model containing element \code{CPUE}
    and/or \code{Survey}.}
  \item{what}{what should be plotted: \code{"c"}[ommercial] or
    \code{"s"}[urvey] abundance index.}
  \item{series}{vector of strings indicating which gears or surveys
    should be plotted (first by default).}
  \item{axes}{whether axis values should be plotted.}
  \item{same.limits}{whether panels should have same y-axis limits.}
  \item{between}{list with \code{x} and \code{y} indicating panel
    spacing.}
  \item{ylim}{vector with lower and upper y-axis limits.}
  \item{q}{denominator to scale the y-axis, e.g. to vulnerable biomass.}
  \item{bar}{extent of error bars relative to standard error.}
  \item{log.transform}{whether values should be log-transformed.}
  \item{base.log}{logarithm base.}
  \item{main}{main title.}
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}
  \item{cex.main}{size of main title.}
  \item{cex.lab}{size of axis labels.}
  \item{cex.strip}{size of strip labels.}
  \item{cex.axis}{size of tick labels.}
  \item{las}{orientation of tick labels: 0=parallel, 1=horizontal,
    2=perpendicular, 3=vertical.}
  \item{tck}{tick mark length.}
  \item{tick.number}{number of tick marks.}
  \item{lty.grid}{line type of gridlines.}
  \item{col.grid}{colour of gridlines.}
  \item{pch}{symbol for points.}
  \item{cex.points}{size of points.}
  \item{col.points}{colour of points, possibly a vector.}
  \item{lty.lines}{line type of main lines.}
  \item{lwd.lines}{line width of main lines.}
  \item{col.lines}{colour of main lines, possibly a vector.}
  \item{lty.bar}{line type of error bars.}
  \item{plot.it}{whether to draw plot.}
  \item{\dots}{passed to \code{xyplot}.}
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

  Trellis plot details can be studied from the
  \code{\link[lattice]{Lattice}} help page.
}
\examples{
plotIndex(x.cod, "s", xlab="Year", ylab="Survey abundance index",
          strip=FALSE)

plotIndex(x.oreo, xlim=list(c(1981,1990),c(1992,2002)), xlab="Year",
          ylab="Observed CPUE", col.points=c("peru","red"), lty.lines=0)
}
\keyword{hplot}

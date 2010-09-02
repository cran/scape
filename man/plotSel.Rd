\name{plotSel}
\alias{plotSel}
\title{Plot Selectivity and Maturity}
\description{
  Plot \code{scape} model predicted selectivity and observed maturity.
}
\usage{
plotSel(model, together=FALSE, series=NULL, sex=NULL, axes=TRUE,
        legend="bottom", main="", xlab="", ylab="", cex.main=1.2,
        cex.legend=1, cex.lab=1, cex.axis=0.8, cex.strip=0.8,
        col.strip="gray95", las=1, tck=0, tick.number=5, lty.grid=3,
        col.grid="gray", pch="m", cex.points=1, col.points="black",
        lty.lines=1, lwd.lines=4, col.lines=c("red","blue"), plot=TRUE,
        \dots)
}
\arguments{
  \item{model}{fitted \code{scape} model.}
  \item{together}{whether gears should be plotted in one panel.}
  \item{series}{vector of strings indicating which gears or surveys
    should be plotted (all by default).}
  \item{sex}{string indicating which sex should be plotted (both by
    default).}
  \item{axes}{whether axis values should be plotted.}
  \item{legend}{legend location: \code{"bottom"}, \code{"left"},
    \code{"top"}, \code{"right"}, or \code{""} to suppress legend.}
  \item{main}{main title.}
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}
  \item{cex.main}{size of main title.}
  \item{cex.legend}{size of legend text.}
  \item{cex.lab}{size of axis labels.}
  \item{cex.axis}{size of tick labels.}
  \item{cex.strip}{size of strip labels.}
  \item{col.strip}{color of strip labels.}
  \item{las}{orientation of tick labels: 0=parallel, 1=horizontal,
    2=perpendicular, 3=vertical.}
  \item{tck}{tick mark length.}
  \item{tick.number}{number of tick marks.}
  \item{lty.grid}{line type of gridlines.}
  \item{col.grid}{color of gridlines.}
  \item{pch}{symbol for points.}
  \item{cex.points}{size of points.}
  \item{col.points}{color of points.}
  \item{lty.lines}{line type of main lines, possibly a vector.}
  \item{lwd.lines}{line width of main lines, possibly a vector.}
  \item{col.lines}{color of main lines, possibly a vector.}
  \item{plot}{whether to draw plot.}
  \item{\dots}{passed to \code{xyplot}, \code{panel.points},
    \code{panel.lines}, and \code{panel.superpose}.}
}
\value{
  When \code{plot=TRUE}, a trellis plot is drawn and a data frame is
  returned, containing the data used for plotting. When
  \code{plot=FALSE}, a trellis object is returned.
}
\note{
  The \code{Args} function from the \pkg{gdata} package is recommended
  for reviewing the arguments, instead of \code{args}.
}
\seealso{
  \code{\link[lattice]{xyplot}}, \code{\link[lattice]{panel.points}},
  \code{\link[lattice]{panel.lines}}, and
  \code{\link[lattice]{panel.superpose}} are the underlying drawing
  functions.

  \code{\link{plotCA}}, \code{\link{plotCL}}, \code{\link{plotIndex}},
  and \code{\link{plotLA}} plot model fit and data.

  \code{\link{plotB}}, \code{\link{plotN}}, and \code{plotSel} plot
  derived quantities.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
plotSel(x.ling, xlab="Age", ylab="Selectivity and maturity")

plotSel(x.cod, together=TRUE, xlab="Age\n", ylab="Selectivity",
        pch=NA, col.lines=c("coral","navyblue"), strip=FALSE)
}
% Graphics
\keyword{hplot}

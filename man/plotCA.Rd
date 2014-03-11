\name{plotCA}
\alias{plotCA}
\title{Plot Catch at Age}
\description{Plot \code{scape} model fit to catch-at-age data.}
\usage{
plotCA(model, what="c", fit=TRUE, swap=FALSE, series=NULL, sex=NULL,
       years=NULL, ages=NULL, axes=TRUE, same.limits=TRUE, log=FALSE,
       base=10, eps.log=1e-5, main="", xlab="", ylab="", cex.main=1.2,
       cex.lab=1, cex.axis=0.8, cex.strip=0.8, col.strip="gray95",
       strip=strip.custom(bg=col.strip), las=!fit, tck=c(1,fit)/2,
       tick.number=5, lty.grid=3, col.grid="gray", pch=16,
       cex.points=0.5, col.points="black", lty.lines=1, lwd.lines=2,
       col.lines=c("red","blue"), plot=TRUE, \dots)
}
\arguments{
  \item{model}{fitted \code{scape} model containing element \code{CAc}
    and/or \code{CAs}.}
  \item{what}{what to plot: \code{"c"}[ommercial] or \code{"s"}[urvey]
    catch at age.}
  \item{fit}{whether to overlay fitted values on observed data.}
  \item{swap}{whether to swap ages and years between axes or panels.}
  \item{series}{string indicating which gear or survey to plot (first by
    default).}
  \item{sex}{string indicating which sex to plot (both by default).}
  \item{years}{vector of numbers indicating which years to plot (all by
    default).}
  \item{ages}{vector of numbers indicating which ages to plot (all by
    default).}
  \item{axes}{whether to plot axis values.}
  \item{same.limits}{whether panels should have same y-axis limits.}
  \item{log}{whether to log-transform values.}
  \item{base}{logarithm base.}
  \item{eps.log}{small number to add before log-transforming to avoid
    log 0.}
  \item{main}{main title.}
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}
  \item{cex.main}{size of main title.}
  \item{cex.lab}{size of axis labels.}
  \item{cex.axis}{size of tick labels.}
  \item{cex.strip}{size of strip labels.}
  \item{col.strip}{color of strip labels.}
  \item{strip}{logical flag (whether to plot strip labels), or a
    function passed to \code{xyplot}.}
  \item{las}{orientation of tick labels: 0=parallel, 1=horizontal,
    2=perpendicular, 3=vertical.}
  \item{tck}{tick mark length.}
  \item{tick.number}{number of tick marks.}
  \item{lty.grid}{line type of gridlines.}
  \item{col.grid}{color of gridlines.}
  \item{pch}{symbol for points.}
  \item{cex.points}{size of points.}
  \item{col.points}{color of points.}
  \item{lty.lines}{line type of main lines, possibly a vector where
    element 2 refers to males.}
  \item{lwd.lines}{line width of main lines, possibly a vector where
    element 2 refers to males.}
  \item{col.lines}{color of main lines, possibly a vector where element
    2 refers to males.}
  \item{plot}{whether to draw plot.}
  \item{\dots}{passed to \code{xyplot}, \code{panel.xyplot},
    \code{panel.superpose}, and \code{panel.superpose.2}.}
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
  \code{\link[lattice]{xyplot}}, \code{\link[lattice]{panel.xyplot}},
  and \code{\link[lattice]{panel.superpose}} are the underlying drawing
  functions.

  \code{plotCA}, \code{\link{plotCL}}, \code{\link{plotIndex}}, and
  \code{\link{plotLA}} plot model fit and data.

  \code{\link{plotB}}, \code{\link{plotN}}, and \code{\link{plotSel}}
  plot derived quantities.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
plotCA(x.sbw, fit=FALSE, strip=FALSE, xlab="Age", ylab="Year",
       tick.number=10)

plotCA(x.cod, xlab="Age", ylab="Proportion in catch", cex.strip=0.7,
       cex.axis=0.7, col.lines="brown", layout=c(8,4))

plotCA(x.cod, xlab="Age", ylab="Proportion in catch", cex.strip=0.7,
       cex.axis=0.7, col.lines="brown", layout=c(2,4), swap=TRUE,
       ages=3:10, same.limits=FALSE)

plotCA(x.ling, "s", col.points=c("red","blue"), lty.lines=0, xlab="Age",
       ylab="Observed proportion in survey", tck=0.5, cex.strip=0.7,
       cex.axis=0.7)

plotCA(x.ling, "s", xlab="Age", ylab="Observed proportion in survey",
       fit=FALSE, cex.strip=0.7, cex.axis=0.7, tck=0.5, layout=c(5,2))

plotCA(x.ling, "s", xlab="Age", ylab="Observed proportion in survey",
       fit=FALSE, cex.strip=0.7, cex.axis=0.7, tck=0.5, layout=c(5,6),
       swap=TRUE)
}
% Graphics
\keyword{hplot}

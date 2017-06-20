\name{plotN}
\alias{plotN}
\title{Plot Numbers at Age}
\description{Plot \code{scape} model predicted numbers at age.}
\usage{
plotN(model, what="d", swap=FALSE, years=NULL, ages=NULL, axes=TRUE,
      same.limits=TRUE, div=1, log=FALSE, base=10, main="", xlab="",
      ylab="", cex.main=1.2, cex.lab=1, cex.axis=0.8, cex.strip=0.8,
      col.strip="gray95", strip=strip.custom(bg=col.strip),
      las=(what=="b"), tck=c(1,what=="b")/2, tick.number=10, lty.grid=3,
      col.grid="white", pch=16, cex.points=1, col.points="black",
      ratio.bars=3, col.bars="gray", plot=TRUE, \dots)
}
\arguments{
  \item{model}{fitted \code{scape} model.}
  \item{what}{what to plot: \code{"d"}[efault],
    \code{"i"}[nitial year], \code{"l"}[ast year],
    \code{"r"}[ecruitment], \code{"p"}[panels], \code{"b"}[ubble plot].}
  \item{swap}{whether to swap ages and years between axes or panels.}
  \item{years}{vector of numbers indicating which years to plot (all by
    default).}
  \item{ages}{vector of numbers indicating which ages to plot (all by
    default).}
  \item{axes}{whether to plot axis values.}
  \item{same.limits}{whether panels should have same y-axis limits.}
  \item{div}{denominator to shorten values on the y axis.}
  \item{log}{whether to log-transform values.}
  \item{base}{logarithm base.}
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
  \item{ratio.bars}{width of bars.}
  \item{col.bars}{color of bars.}
  \item{plot}{whether to draw plot.}
  \item{\dots}{passed to \code{xyplot}, \code{panel.barchart}, and
    \code{panel.xyplot}.}
}
\details{
  The \code{"d"}[efault] plot is a combination of
  \code{"i"}[nitial year] and \code{"r"}[ecruitment].
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
  and \code{\link[lattice]{panel.barchart}} are the underlying drawing
  functions.

  \code{\link{plotCA}}, \code{\link{plotCL}}, \code{\link{plotIndex}},
  and \code{\link{plotLA}} plot model fit and data.

  \code{\link{plotB}}, \code{plotN}, and \code{\link{plotSel}} plot
  derived quantities.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
plotN(x.cod, div=1000, xlab=c("Age (years)","Year"),
      ylab="Individuals (million)")

plotN(x.cod, "l", div=1000, xlab="Age", ylab="Individuals (million)")

plotN(x.cod, "r", age=3, div=1000, xlim=c(1967,2002))

plotN(x.cod, "p", div=1000, ages=3:10, xlim=c(2,11), xlab="Age",
      ylab="Individuals (million)", cex.strip=0.7, cex.axis=0.7,
      tck=0.5)

plotN(x.cod, "b", xlab="Age (years)", ylab="Year", cex.points=0.7)
}

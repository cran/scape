\name{plotLA}
\alias{plotLA}
\title{Plot Length at Age}
\description{Plot \code{scape} model fit to length-at-age data.}
\usage{
plotLA(model, together=FALSE, sex=NULL, axes=TRUE, same.limits=TRUE,
       between=list(x=axes,y=axes), ylim=NULL, bands=1, main="",
       xlab="", ylab="", cex.main=1.2, cex.lab=1, cex.axis=0.8,
       cex.strip=0.8, col.strip="gray95",
       strip=strip.custom(bg=col.strip), las=1, tck=0, tick.number=5,
       lty.grid=3, col.grid="gray", pch=16, cex.points=0.5,
       col.points="black", lty.lines=1, lwd.lines=4,
       col.lines=c("red","blue"), lty.bands=2*(!together), lwd.bands=1,
       col.bands="black", plot=TRUE, \dots)
}
\arguments{
  \item{model}{fitted \code{scape} model containing element \code{LA}.}
  \item{together}{whether to plot both sexes in one panel.}
  \item{sex}{string indicating which sex to plot (both by default).}
  \item{axes}{whether to plot axis values.}
  \item{same.limits}{whether panels should have same y-axis limits.}
  \item{between}{list with \code{x} and \code{y} indicating panel
    spacing.}
  \item{ylim}{vector with lower and upper y-axis limits.}
  \item{bands}{extent of error bands relative to standard error.}
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
  \item{pch}{symbol for points, possibly a vector where element 2 refers
    to males.}
  \item{cex.points}{size of points, possibly a vector where element 2
    refers to males.}
  \item{col.points}{color of points, possibly a vector where element 2
    refers to males.}
  \item{lty.lines}{line type of main lines, possibly a vector where
    element 2 refers to males.}
  \item{lwd.lines}{line width of main lines, possibly a vector where
    element 2 refers to males.}
  \item{col.lines}{color of main lines, possibly a vector where element
    2 refers to males.}
  \item{lty.bands}{line type of error bands.}
  \item{lwd.bands}{line width of error bands.}
  \item{col.bands}{color of error bands, possibly a vector where element
    2 refers to males.}
  \item{plot}{whether to draw plot.}
  \item{\dots}{passed to \code{xyplot} and \code{panel.superpose.2}.}
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

  \code{\link{plotCA}}, \code{\link{plotCL}}, \code{\link{plotIndex}},
  and \code{plotLA} plot model fit and data.

  \code{\link{plotB}}, \code{\link{plotN}}, and \code{\link{plotSel}}
  plot derived quantities.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
plotLA(x.oreo, xlab="Age", ylab="Length (cm)")

mykey <- list(text=list(lab=c("Female","Male")), space="right",
              lines=list(lwd=4,col=c("red","blue")))
plotLA(x.oreo, together=TRUE, xlab="Age", ylab="Length (cm)", pch=NA,
       key=mykey)

mykey <- list(text=list(lab=c("Female","Male")), space="right",
              points=list(pch=16,cex=0.5,col=c("red","blue")))
plotLA(x.oreo, together=TRUE, xlab="Age", ylab="Length (cm)",
       col.points=c("red","blue"), lty.lines=0, key=mykey)
}

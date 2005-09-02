\name{plotN}
\alias{plotN}
\concept{Fisheries stock assessment}
\title{Plot Numbers at Age}
\description{Plot \code{scape} model predicted numbers at age.}
\usage{
plotN(model, what="d", years=NULL, ages=NULL, axes=TRUE,
      same.limits=TRUE, div=1, log.transform=FALSE, base.log=10,
      main="", xlab="", ylab="", cex.main=1.2, cex.lab=1, cex.strip=0.8,
      cex.axis=0.8, las=what == "b", tck=c(1,what=="b")/2,
      tick.number=10, lty.grid=3, col.grid="white", pch=16,
      cex.points=1, col.points="black", ratio.bars=3, col.bars="grey",
      plot.it=TRUE, ...)
}
\arguments{
  \item{model}{fitted \code{scape} model.}
  \item{what}{what should be plotted: \code{"d"}[efault],
    \code{"i"}[nitial year], \code{"r"}[ecruitment],
    \code{"y"}[ear panels], \code{"b"}[ubble plot].}
  \item{years}{vector of numbers indicating which years should be
    plotted (all by default).}
  \item{ages}{vector of numbers indicating which ages should be plotted
    (all by default).}
  \item{axes}{whether axis values should be plotted.}
  \item{same.limits}{whether panels should have same y-axis limits.}
  \item{div}{denominator to shorten values on y axis.}
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
  \item{col.points}{colour of points.}
  \item{ratio.bars}{width of bars.}
  \item{col.bars}{colour of bars.}
  \item{plot.it}{whether to draw plot.}
  \item{\dots}{passed to \code{xyplot}.}
}
\details{
  The \code{"d"}[efault] plot is a combination of
  \code{"i"}[nitial year] and \code{"r"}[ecruitment].
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
plotN(x.cod, div=1000, xlab=c("Age (years)","Year"),
      ylab=c("Individuals (million)","One-year-olds (million)"))

plotN(x.cod, "b", xlab="Age (years)", ylab="Year", cex.points=0.7)

plotN(x.cod, "y", div=1000, ages=3:10, xlim=c(2,11), xlab="Age",
      ylab="Individuals (million)", cex.strip=0.7, cex.axis=0.7,
      tck=0.5)
}
\keyword{hplot}

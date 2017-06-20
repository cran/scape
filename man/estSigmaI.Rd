\name{estSigmaI}
\alias{estSigmaI}
\title{Estimate Abundance Index Sigma}
\description{
  Estimate the effective sigma (magnitude of observation noise) for a
  survey or commercial abundance index, based on the empirical standard
  deviation.
}
\usage{
estSigmaI(model, what="s", series=NULL, init=NULL, FUN=mean, p=1,
          digits=2)
}
\arguments{
  \item{model}{fitted \code{scape} model containing element \code{CPUE}
    and/or \code{Survey}.}
  \item{what}{which effective sigma to estimate: \code{"c"}[ommercial]
    or \code{"s"}[urvey] abundance index.}
  \item{series}{vector of strings indicating which gears or surveys to
    analyze (all by default).}
  \item{init}{initial sigma, determining the relative pattern of the
    effective sigmas between years.}
  \item{FUN}{function to use when scaling a vector of sigmas.}
  \item{p}{effective number of parameters estimated in the model.}
  \item{digits}{number of decimal places to use when rounding, or
    \code{NULL} to suppress rounding.}
}
\details{
  The \code{init} sigmas set a fixed pattern for the relative sigmas
  between years. For example, if there are two years of abundance index
  data and the initial sigmas are 0.1 in year 1 and 0.2 in year 2, the
  effective sigma will be two times greater in year 2 than in year 1,
  although both will be scaled up or down depending on how closely the
  model fits the abundance index. The value of \code{init} can be one of
  the following:

  \describe{
    \item{\code{NULL}}{means read the initial sigmas from the existing
      \code{CV} column (default).}
    \item{model}{means read the initial sigmas from the \code{CV} column
      in that model (object of class \code{scape}).}
    \item{numeric vector}{means those are the initial sigmas (same
      length as the number of years).}
    \item{\code{FALSE} or \code{1}}{means use one effective sigma
    (\eqn{\hat sigma}{sigmahat}) across all years.}
  }

  The idea behind \code{FUN=mean} is to guarantee that regardless of the
  value of \code{init}, the mean effective sigma will always be the
  same. Other functions can be used to a similar effect, such as
  \code{FUN=median}.
}
\value{
  Numeric vector of effective sigmas (one value if \code{init=1}), or a
  list of such vectors when analyzing multiple series.
}
\note{
  This function uses the empirical standard deviation to estimate an
  effective sigma, which may be appropriate as likelihood weights for
  abundance index data. The better the model fits the data, the smaller
  the effective sigma.

  \code{estSigmaI} can be used iteratively, along with
  \code{\link{estN}} and \code{\link{estSigmaR}} to assign likelihood
  weights that are indicated by the model fit to the data. Sigmas and
  sample sizes are then adjusted between model runs, until they
  converge. The \code{iterate} function facilitates this procedure.

  If \eqn{rss} is the residual sum of squares in log space, \eqn{n} is
  the number of abundance index data points, and \eqn{p} is the
  effective number of parameters estimated in the model, then the
  estimated effective sigma is:

  \deqn{\hat\sigma=\sqrt{\frac{rss}{n-p}}}{sigmahat = sqrt(rss/(n-p))}

  There is no simple way to calculate \eqn{p} for statistical
  catch-at-age models. The default value of 1 is likely to underestimate
  the true magnitude of observation noise.
}
\seealso{
  \code{\link{getN}}, \code{\link{getSigmaI}}, \code{\link{getSigmaR}},
  \code{\link{estN}}, \code{estSigmaI}, and \code{\link{estSigmaR}}
  extract and estimate sample sizes and sigmas.

  \code{\link{iterate}} combines all the \code{get*} and \code{est*}
  functions in one call.

  \code{\link{plotIndex}} shows what is behind the sigma estimation.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
## Exploring candidate sigmas:

getSigmaI(x.cod)       # sigma used in assessment 0.20
estSigmaI(x.cod)       # model fit implies 0.17
plotIndex(x.cod)       # model fit
estSigmaI(x.cod, p=8)  # eight estimated parameters implies 0.22

getSigmaI(x.sbw)          # sigma used in assessment
estSigmaI(x.sbw)          # model fit implies smaller sigma
estSigmaI(x.sbw, init=1)  # could use 0.17 in all years

## Same mean, regardless of init:

mean(estSigmaI(x.sbw, digits=NULL))
mean(estSigmaI(x.sbw, digits=NULL, init=1))

## Same median, regardless of init:

median(estSigmaI(x.sbw, FUN=median, digits=NULL))
median(estSigmaI(x.sbw, FUN=median, digits=NULL, init=1))

## Multiple series:

getSigmaI(x.oreo, "c")                 # sigma used in assessment
getSigmaI(x.oreo, "c", digits=2)       # rounded
estSigmaI(x.oreo, "c")                 # model fit implies smaller sigma
estSigmaI(x.oreo, "c", init=1)         # could use 0.19 in all years
estSigmaI(x.oreo, "c", init=1, digits=3)  # series 2 slightly worse fit
# estSigmaI(x.oreo, "c", init=1, p=11) # more parameters than datapoints

getSigmaI(x.oreo, "c", series="Series 2-1")  # get one series
estSigmaI(x.oreo, "c", series="Series 2-1")  # estimate one series
}

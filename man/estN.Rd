\name{estN}
\alias{estN}
\alias{estN.int}
\title{Estimate Effective Sample Size}
\description{
  Estimate the effective sample size for catch-at-age or catch-at-length
  data, based on the multinomial distribution.
}
\usage{
estN(model, what="CAc", series=NULL, init=NULL, FUN=mean, ceiling=Inf,
     digits=0)

estN.int(P, Phat)  # internal function
}
\arguments{
  \item{model}{fitted \code{scape} model containing catch-at-age and/or
    catch-at-length data.}
  \item{what}{name of model element: \code{"CAc"}, \code{"CAs"},
    \code{"CLc"}, or \code{"CLs"}.}
  \item{series}{vector of strings indicating which gears or surveys to
    analyze (all by default).}
  \item{init}{initial sample size, determining the relative pattern of
    the effective sample size between years.}
  \item{FUN}{function to standardize the effective sample size.}
  \item{ceiling}{largest possible sample size in one year.}
  \item{digits}{number of decimal places to use when rounding, or
    \code{NULL} to suppress rounding.}
  \item{P}{observed catch-at-age or catch-at-length matrix.}
  \item{Phat}{fitted catch-at-age or catch-at-length matrix.}
}
\details{
  The \code{init} sample sizes set a fixed pattern for the relative
  sample sizes between years. For example, if there are two years of
  catch-at-age data and the initial sample sizes are 100 in year 1 and
  200 in year 2, the effective sample size will be two times greater in
  year 2 than in year 1, although both will be scaled up or down
  depending on how closely the model fits the catch-at-age data. The
  value of \code{init} can be one of the following:

  \describe{
    \item{\code{NULL}}{means read the initial sample sizes from the
      existing \code{SS} column (default).}
    \item{model}{means read the initial sample sizes from the \code{SS}
      column in that model (object of class \code{scape}).}
    \item{numeric vector}{means those are the initial sample sizes (same
      length as the number of years).}
    \item{\code{FALSE}}{means ignore the initial sample sizes and use
      the empirical multinomial sample size (\eqn{\hat n}{nhat}) in each
      year.}
    \item{\code{1}}{means calculate one effective sample size to use
      across all years, e.g. the mean or median of \eqn{\hat n}{nhat}.}
  }

  The idea behind \code{FUN=mean} is to guarantee that regardless of the
  value of \code{init}, the mean effective sample size will always be
  the same. Other functions can be used to a similar effect, such as
  \code{FUN=median}.

  The \code{estN} function is implemented for basic single-sex datasets.
  If the data are sex-specific, \code{estN} pools (averages) the sexes
  before estimating effective sample sizes. The general function
  \code{estN.int}, on the other hand, is suitable for analyzing any
  datasets in \code{matrix} format. The \samp{int} in \code{estN.int}
  stands for internal (not integer), analogous to \code{rep.int},
  \code{seq.int}, \code{sort.int}, and similar functions.
}
\value{
  Numeric vector of effective sample sizes (one value if \code{init=1}),
  or a list of such vectors when analyzing multiple series.
}
\note{
  This function uses the empirical multinomial sample size to estimate
  an effective sample size, which may be appropriate as likelihood
  weights for catch-at-age and catch-at-length data. The better the
  model fits the data, the larger the effective sample size. See
  McAllister and Ianelli (1997), Gavaris and Ianelli (2002), and
  Magnusson et al. (2013).

  \code{estN} can be used iteratively, along with
  \code{\link{estSigmaI}} and \code{\link{estSigmaR}} to assign
  likelihood weights that are indicated by the model fit to the data.
  Sigmas and sample sizes are then adjusted between model runs, until
  they converge. The \code{iterate} function facilitates this procedure.

  If \eqn{P_{t,a}}{P[t,a]} is the observed proportion of fish at age (or
  length bin) \eqn{a} in year \eqn{t}, and \eqn{\hat P_{t,a}}{Phat[t,a]}
  is the fitted proportion, then the estimated sample size in that year
  is:

  \deqn{\hat n_t=\left.\sum_a{\hat P_{t,a}(1-\hat
      P_{t,a})}\right/\sum_a{(P_{t,a}-\hat P_{t,a})^2}}{nhat[t] =
    sum_a(Phat[t,a]*(1-Phat[t,a])) / sum_a((P[t,a]-Phat[t,a])^2)}

  Due to the non-random and non-independent nature of sampling fish, the
  effective sample size, for statistical purposes, is much less than the
  number of fish sampled. Common starting points include using the
  number of tows as the sample size in each year, or using the empirical
  multinomial sample sizes. Those \dQuote{initial} sample sizes can then
  be scaled up or down. Sample sizes between 20 and 200 are common in
  the stock assessment literature.
}
\references{
  Gavaris, S. and Ianelli, J. N. (2002) Statistical issues in fisheries'
  stock assessments. \emph{Scandinavian Journal of Statistics}
  \bold{29}, 245--271.

  Magnusson, A., Punt, A. E., and Hilborn, R. (2013) Measuring
  uncertainty in fisheries stock assessment: the delta method,
  bootstrap, and MCMC. \emph{Fish and Fisheries} \bold{14}, 325--342.

  McAllister, M. K. and Ianelli, J. N. (1997) Bayesian stock assessment
  using catch-age data and the sampling-importance resampling algorithm.
  \emph{Canadian Journal of Fisheries and Aquaticic Sciences}
  \bold{54}, 284--300.
}
\seealso{
  \code{\link{getN}}, \code{\link{getSigmaI}}, \code{\link{getSigmaR}},
  \code{estN}, \code{\link{estSigmaI}}, and \code{\link{estSigmaR}}
  extract and estimate sample sizes and sigmas.

  \code{\link{iterate}} combines all the \code{get*} and \code{est*}
  functions in one call.

  \code{\link{plotCA}} and \code{\link{plotCL}} show what is behind the
  sample-size estimation.

  \code{\link{scape-package}} gives an overview of the package.
}
\examples{
## Exploring candidate sample sizes:

getN(x.sbw)     # sample sizes used in assessment: number of tows
estN(x.sbw)     # effective sample size, given data (tows) and model fit
estN(x.sbw, ceiling=200)  # could use this
estN(x.sbw, init=FALSE)   # from model fit, disregarding tows
plotCA(x.sbw)             # years with good fit => large sample size
estN(x.sbw, init=1)       # one sample size across all years
estN(x.sbw, init=c(rep(1,14),rep(2,9)))  # two sampling periods

## Same mean, regardless of init:

mean(estN(x.sbw, digits=NULL))
mean(estN(x.sbw, digits=NULL, init=FALSE))
mean(estN(x.sbw, digits=NULL, init=1))
mean(estN(x.sbw, digits=NULL, init=c(rep(1,14),rep(2,9))))

## Same median, regardless of init:

median(estN(x.sbw, FUN=median, digits=NULL))
median(estN(x.sbw, FUN=median, digits=NULL, init=FALSE))
median(estN(x.sbw, FUN=median, digits=NULL, init=1))
median(estN(x.sbw, FUN=median, digits=NULL, init=c(rep(1,14),rep(2,9))))

## Multiple series:

getN(x.ling, "CLc")              # sample size used in assessment
getN(x.ling, "CLc", digits=0)    # rounded
estN(x.ling, "CLc")              # model fit implies larger sample sizes

getN(x.ling, "CLc", series="1", digits=0)  # get one series
estN(x.ling, "CLc", series="1")            # estimate one series
}

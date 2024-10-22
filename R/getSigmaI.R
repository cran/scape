getSigmaI <- function(model, what="s", series=NULL, digits=NULL)
{
  ## 1  Parse args
  what <- match.arg(what, c("c","s"))
  x <- if(inherits(model,"scape") && what=="c") model$CPUE
       else if(inherits(model,"scape") && what=="s") model$Survey
       else model  # allow data frame
  if(is.null(x))
    stop(paste("element", if(what=="s") "'Survey'" else "'CPUE'", "not found"))
  x <- x[!is.na(x$CV),]

  ## 2  Extract series
  if(is.null(series))
    series <- unique(x$Series)
  if(length(series) > 1)
  {
    output <- lapply(series, function(s)
                     getSigmaI(model=model, what=what, series=s, digits=digits))
    names(output) <- series
  }
  else
  {
    ok.series <- x$Series %in% series
    if(!any(ok.series)) stop("please check if the 'series' argument is correct")
    x <- x[!is.na(x$Obs) & ok.series,]

    ## 3  Create output
    output <- structure(x$CV, names=x$Year)
    if(!is.null(digits))
      output <- round(output, digits=digits)
  }

  output
}

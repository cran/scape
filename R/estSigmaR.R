estSigmaR <- function(model, digits=2)
{
  ## 1  Parse args
  x <- if(inherits(model,"scape")) model$Dev else model  # allow data frame
  if(is.null(x))
    stop("element 'Dev' not found")

  ## 2  Calculate sigmaR
  ss <- sapply(x[c("Initial","Annual")], function(x) sum(x^2))
  n <- sapply(x[c("Initial","Annual")], length)
  output <- sqrt(ss/(n-1))
  if(!is.null(digits))
    output <- round(output, digits=digits)

  output
}

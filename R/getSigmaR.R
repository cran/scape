getSigmaR <- function(model, digits=NULL)
{
  ## 1  Parse args
  x <- if(class(model)=="scape") model$Dev else model  # allow data frame
  if(is.null(x))
    stop("element 'Dev' not found")

  ## 2  Create output
  output <- x$sigmaR
  if(!is.null(digits))
    output <- round(output, digits=digits)

  output
}

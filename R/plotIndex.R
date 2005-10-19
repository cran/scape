"plotIndex" <-
function(model, what="c", series=NULL, axes=TRUE, same.limits=FALSE, between=list(x=axes,y=axes), ylim=NULL, q=1, bar=1,
         log=FALSE, base=10, main="", xlab="", ylab="", cex.main=1.2, cex.lab=1, cex.strip=0.8, cex.axis=0.8, las=1,
         tck=c(1,0)/2, tick.number=5, lty.grid=3, col.grid="white", pch=16, cex.points=1.2, col.points="black",
         lty.lines=1, lwd.lines=4, col.lines="dimgrey", lty.bar=1, plot=TRUE, ...)
{
  ## 1 DEFINE FUNCTIONS
  panel.index <- function(x, y, subscripts, yobs, yfit, col.points, col.lines, ...)  # overlay obs and fit in series panels
  {
    y.range <- range(c(rep(0,!log),attr(yobs,"other"),y), na.rm=TRUE)
    panel.abline(v=pretty(x,tick.number), h=pretty(y.range,tick.number), lty=lty.grid, col=col.grid)
    panel.xyplot(x, y, type="n", ...)
    panel.xyplot(x, yfit[subscripts], type="l", lty=lty.lines, lwd=lwd.lines, col=col.lines[subscripts], ...)
    ok.Y <- !is.na(yobs[subscripts])  # NAs in panel.xYplot are fine on screen, but crash PDF and PS files
    if(lty.bar == 0)
      panel.xyplot(x, yobs[subscripts], col=col.points, ...)
    else
      panel.xYplot(x[ok.Y], yobs[subscripts][ok.Y], subscripts=subscripts[ok.Y], col=col.points[subscripts],
                   lty.bar=lty.bar, ...)
  }

  ## 2 PARSE ARGS
  if(class(model) != "scape")
    stop("The 'model' argument should be a scape object, not ", chartr("."," ",class(model)), ".")
  what <- match.arg(what, c("c","s"))
  relation <- if(same.limits) "same" else "free"

  ## 3 PREPARE DATA (extract, filter, add columns, transform)
  if(what == "c")
  {
    if(any(names(model) == "CPUE"))
      x <- model$CPUE
    else
    {
      what <- "s"
      cat("Commercial CPUE data (", substitute(model), "$CPUE) not found. Assuming user intended what=\"s\".\n", sep="")
    }
  }
  if(what == "s")  # 'what' may have changed since last if statement
  {
    if(any(names(model) == "Survey"))
      x <- model$Survey
    else
      stop("Found neither commercial CPUE data (", substitute(model), "$CPUE) nor survey abundance data (",
           substitute(model), "$Survey).\nPlease verify that ", substitute(model),
           " is a 'scape' model that contains abundance index data.")
  }
  if(is.null(series))
    series <- unique(x$Series)
  ok.series <- x$Series %in% series; if(!any(ok.series)) stop("Please check if the 'series' argument is correct.")
  x <- x[ok.series,]
  if(is.numeric(x$Series))
    x$Series <- factor(paste("Series", x$Series))
  x$Obs <- x$Obs/q
  x$Fit <- x$Fit/q
  x$Hi  <- x$Obs*exp(bar*x$CV)
  x$Lo  <- x$Obs/exp(bar*x$CV)
  if(log)
  {
    x$Obs <- log(x$Obs, base=base)
    x$Fit <- log(x$Fit, base=base)
    x$Hi  <- log(x$Hi,  base=base)
    x$Lo  <- log(x$Lo,  base=base)
  }

  ## 4 PREPARE PLOT (check device, vectorize args, create list args)
  require(grid, quietly=TRUE, warn.conflicts=FALSE)
  require(Hmisc, quietly=TRUE, warn.conflicts=FALSE)
  require(lattice, quietly=TRUE, warn.conflicts=FALSE)
  if(trellis.par.get()$background$col == "#909090")
  {
    for(d in dev.list()) dev.off()
    trellis.device(color=FALSE)
  }
  col.points <- rep(col.points, length.out=nlevels(x$Series))
  col.lines <- rep(col.lines, length.out=nlevels(x$Series))
  mymain <- list(label=main, cex=cex.main)
  myxlab <- list(label=xlab, cex=cex.lab)
  myylab <- list(label=ylab, cex=cex.lab)
  myrot <- switch(as.character(las), "0"=list(x=list(rot=0),y=list(rot=90)), "1"=list(x=list(rot=0),y=list(rot=0)),
                  "2"=list(x=list(rot=90),y=list(rot=0)), "3"=list(x=list(rot=90),y=list(rot=90)))
  myscales <- c(list(draw=axes,relation=relation,cex=cex.axis,tck=tck,tick.number=tick.number), myrot)
  mystrip <- list(cex=cex.strip)

  ## 5 CREATE TRELLIS OBJECT
  graph <- xyplot(Fit~Year|Series, data=x, panel=panel.index, yobs=Cbind(x$Obs,x$Hi,x$Lo), yfit=x$Fit, as.table=TRUE,
                  between=between, main=mymain, xlab=myxlab, ylab=myylab, par.strip.text=mystrip, scales=myscales,
                  pch=pch, cex=cex.points, col.points=col.points[x$Series], col.lines=col.lines[x$Series], ...)
  if(is.list(ylim))
    graph$y.limits <- rep(ylim, length.out=length(series))
  else if(is.numeric(ylim))
    graph$y.limits <- rep(list(ylim), length(series))
  else  # ylim is null so find reasonable limits
  {
    extremes <- as.data.frame(lapply(split(x[,c("Fit","Hi","Lo")],x$Series), range, na.rm=TRUE))
    if(!log)
      graph$y.limits <- lapply(extremes, function(x) c(0,1.04*x[2]))
    else
      graph$y.limits <- lapply(extremes, function(x) range(x)+c(-0.04,0.04)*diff(range(x)))
  }
  if(same.limits)
    graph$y.limits <- range(unlist(graph$y.limits))

  ## 6 FINISH
  if(plot)
  {
    print(graph)
    return(list(extremes=extremes, graph.y.limits=graph$y.limits))
    ## invisible(x)
  }
  else
  {
    return(list(extremes=extremes, graph.y.limits=graph$y.limits))
    ## invisible(graph)
  }
}


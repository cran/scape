"plotN" <-
function(model, what="d", years=NULL, ages=NULL, axes=TRUE, same.limits=TRUE, div=1, log=FALSE, base=10, main="", xlab="",
         ylab="", cex.main=1.2, cex.lab=1, cex.strip=0.8, cex.axis=0.8, las=(what=="b"), tck=c(1,what=="b")/2,
         tick.number=10, lty.grid=3, col.grid="white", pch=16, cex.points=1, col.points="black", ratio.bars=3,
         col.bars="grey", plot=TRUE, ...)
{
  ## 1 DEFINE FUNCTIONS
  panel.bar <- function(x, y, ...)  # barplot of N in one or more panel
  {
    panel.abline(h=pretty(y,tick.number), lty=lty.grid, col=col.grid)
    panel.barchart(x, y, ...)
  }
  panel.bubble <- function(x, y, ...)  # bubble plot N in one panel
  {
    panel.abline(v=pretty(x,tick.number), h=pretty(y,tick.number), lty=lty.grid, col=col.grid)
    panel.xyplot(x, y, ...)
  }

  ## 2 PARSE ARGS
  if(class(model) != "scape")
    stop("The 'model' argument should be a scape object, not ", chartr("."," ",class(model)), ".")
  what <- match.arg(what, c("d","i","r","y","b"))
  relation <- if(same.limits) "same" else "free"
  las <- as.numeric(las)

  ## 3 PREPARE DATA (extract, rearrange, filter, transform)
  x <- model$N
  x <- aggregate(list(N=x$N), list(Year=x$Year,Age=x$Age), sum)
  x$Year <- as.integer(as.character(x$Year))
  x$Age <- as.integer(as.character(x$Age))
  if(is.null(years))
    years <- unique(x$Year)
  if(is.null(ages))
    ages <- unique(x$Age)
  ok.years <- x$Year %in% years; if(!any(ok.years)) stop("Please check if the 'years' argument is correct.")
  ok.ages  <- x$Age  %in% ages;  if(!any(ok.ages))  stop("Please check if the 'ages' argument is correct.")
  x <- x[ok.years & ok.ages,]
  x$N <- x$N / div
  if(log)
    x$N <- log(x$N, base)

  ## 4 PREPARE PLOT (check device, vectorize args, create list args)
  require(grid, quietly=TRUE, warn.conflicts=FALSE)
  require(lattice, quietly=TRUE, warn.conflicts=FALSE)
  if(trellis.par.get()$background$col == "#909090")
  {
    for(d in dev.list()) dev.off()
    trellis.device(color=FALSE)
  }
  main <- rep(main, length.out=2)
  xlab <- rep(xlab, length.out=2)
  ylab <- rep(ylab, length.out=2)
  las  <- rep(las,  length.out=2)
  mymain <- list(label=main[1], cex=cex.main)
  myxlab <- list(label=xlab[1], cex=cex.lab)
  myylab <- list(label=ylab[1], cex=cex.lab)
  myrot <- switch(as.character(las[1]), "0"=list(x=list(rot=0),y=list(rot=90)), "1"=list(x=list(rot=0),y=list(rot=0)),
                  "2"=list(x=list(rot=90),y=list(rot=0)), "3"=list(x=list(rot=90),y=list(rot=90)))
  myscales <- c(list(draw=axes,relation=relation,cex=cex.axis,tck=tck,tick.number=tick.number), myrot)
  mystrip <- list(cex=cex.strip)

  ## 5 CREATE TRELLIS OBJECT
  printed <- FALSE
  fixed.ylim <- FALSE
  if(what == "d")  # recursive flow: plotN("i",plot=F) -> print -> plotN("r",plot=F) -> print
  {
    graph <- plotN(model, what="i", years=years, ages=ages, axes=axes, relation=relation,
                   div=div, log=log, base=base, main=main, xlab=xlab, ylab=ylab,
                   cex.main=cex.main, cex.lab=cex.lab, cex.strip=cex.strip, col.grid=col.grid, cex.axis=cex.axis, las=las,
                   tck=tck, tick.number=tick.number, lty.grid=lty.grid, cex.points=cex.points, col.points=col.points,
                   ratio.bars=ratio.bars, col.bars=col.bars, plot=FALSE, ...)
    print(graph, split=c(1,1,1,2), more=TRUE)
    graph <- plotN(model, what="r", years=years, ages=ages, axes=axes, relation=relation,
                   div=div, log=log, base=base, main=main, xlab=xlab, ylab=ylab,
                   cex.main=cex.main, cex.lab=cex.lab, cex.strip=cex.strip, col.grid=col.grid, cex.axis=cex.axis, las=las,
                   tck=tck, tick.number=tick.number, lty.grid=lty.grid, cex.points=cex.points, col.points=col.points,
                   ratio.bars=ratio.bars, col.bars=col.bars, plot=FALSE, ...)
    print(graph, split=c(1,2,1,2))
    printed <- TRUE
  }
  if(what == "i")
  {
    x <- x[x$Year==min(x$Year),]
    graph <- xyplot(N~Age|"Initial population", data=x, panel=panel.bar, horizontal=FALSE,
                    main=mymain, xlab=myxlab, ylab=myylab, scales=myscales, par.strip.text=mystrip,
                    box.ratio=ratio.bars, col=col.bars, ...)
    graph$x.limits <- c(min(x$Age)-0.5, max(x$Age)+0.5)
  }
  if(what == "r")  # Year-1 aligns cohorts
  {
    x <- x[x$Age==min(x$Age),]
    mymain <- list(label=main[2], cex=cex.main)
    myxlab <- list(label=xlab[2], cex=cex.lab)
    myylab <- list(label=ylab[2], cex=cex.lab)
    myrot <- switch(as.character(las[2]), "0"=list(x=list(rot=0),y=list(rot=90)), "1"=list(x=list(rot=0),y=list(rot=0)),
                    "2"=list(x=list(rot=90),y=list(rot=0)), "3"=list(x=list(rot=90),y=list(rot=90)))
    graph <- xyplot(N~Year-1|"Cohorts", data=x, panel=panel.bar, horizontal=FALSE,
                    main=mymain, xlab=myxlab, ylab=myylab, scales=myscales, par.strip.text=mystrip,
                    box.ratio=ratio.bars, col=col.bars, ...)
    graph$x.limits <- c(min(x$Year-1)-0.5, max(x$Year-1)+0.5)
  }
  if(what == "y")
  {
    graph <- xyplot(N~Age|factor(Year), data=x, panel=panel.bar, horizontal=FALSE, as.table=TRUE,
                    main=mymain, xlab=myxlab, ylab=myylab, par.strip.text=mystrip, scales=myscales,
                    box.ratio=ratio.bars, col=col.bars, ...)
  }
  if(what == "b")
  {
    graph <- xyplot(Year~Age, data=x, panel=panel.bubble,
                    main=mymain, xlab=myxlab, ylab=myylab, scales=myscales,
                    pch=pch, cex=cex.points*sqrt(x$N/mean(x$N)), ...)
    graph$y.limits <- rev(graph$y.limits)
    fixed.ylim <- TRUE
  }
  if(!log && !fixed.ylim)  # leave ylim alone if log-transformed or bubble plot
  {
    if(is.list(graph$y.limits))                                                 # set lower ylim to 0
      graph$y.limits <- lapply(graph$y.limits, function(y){y[1]<-0;return(y)})  # multi-panel plot
    else
      graph$y.limits[1] <- 0                                                    # single-panel plot
  }

  ## 6 FINISH
  if(plot)
  {
    if(!printed)
      print(graph)
    invisible(x)
  }
  else
  {
    invisible(graph)
  }
}


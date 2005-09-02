"plotB" <-
function(model, what="d", series=NULL, years=NULL, axes=TRUE, div=1, legend="bottom", main="", xlab="", ylab="",
         cex.main=1.2, cex.legend=1, cex.lab=1, cex.axis=0.8, las=1, tck=c(1,what=="d")/2, tick.number=5, lty.grid=3,
         col.grid="white", pch=16, cex.points=0.8, col.points="black", lty.lines=1:3, lwd.lines=2, col.lines="black",
         ratio.bars=3, col.bars="grey", plot.it=TRUE, ...)
{
  ## 1 DEFINE FUNCTIONS
  panel.linebar <- function(x, y, bars, ...)  # biomass lines and/or yield bars
  {
    panel.abline(h=pretty(y,tick.number), lty=lty.grid, col=col.grid)
    panel.superpose(x, y, ...)
    panel.barchart(bars$Year, bars$Value, horizontal=FALSE, box.ratio=ratio.bars, col=col.bars)
  }
  panel.bar <- function(x, y, ...)  # only yield bars
  {
    panel.abline(h=pretty(y,tick.number), lty=lty.grid, col=col.grid)
    panel.barchart(x, y, horizontal=FALSE, box.ratio=ratio.bars, col=col.bars)
  }

  ## 2 PARSE ARGS
  if(class(model) != "scape")
    stop("The 'model' argument should be a scape object, not ", chartr("."," ",class(model)), ".")
  what <- match.arg(what, c("d","s","l"))
  las <- as.numeric(las)

  ## 3 PREPARE DATA (extract, filter, transform)
  x <- model$B
  x <- data.frame(Year=rep(x$Year,ncol(x)-1), Series=rep(names(x)[-1], each=nrow(x)), Value=as.vector(as.matrix(x[,-1])))
  x$Value <- x$Value
  if(is.null(series))
    series <- unique(as.character(x$Series))
  if(is.null(years))
    years <- unique(x$Year)
  ok.series <- x$Series %in% series; if(!any(ok.series)) stop("Please check if the 'series' argument is correct.")
  ok.years  <- x$Year   %in% years;  if(!any(ok.years))  stop("Please check if the 'years' argument is correct.")
  x <- x[ok.series & ok.years,]
  Bframe <- x[x$Series %in% grep("B",series,value=TRUE),]
  Bframe$Series <- factor(Bframe$Series)  # update levels
  Rframe <- x[x$Series=="R",]
  Yframe <- x[x$Series=="Y",]
  Bframe$Value <- Bframe$Value / div[1]
  Rframe$Value <- Rframe$Value / rep(div,length.out=2)[2]
  Yframe$Value <- Yframe$Value / div[1]

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
  myscales <- c(list(draw=axes,cex=cex.axis,tck=tck,tick.number=tick.number), myrot)
  lty.lines <- rep(lty.lines, length.out=nlevels(Bframe$Series))
  lwd.lines <- rep(lwd.lines, length.out=nlevels(Bframe$Series))
  col.lines <- rep(col.lines, length.out=nlevels(Bframe$Series))
  mykey <- list(space=legend, text=list(lab=levels(Bframe$Series),cex=cex.legend),
                lines=list(lty=lty.lines,lwd=lwd.lines,col=col.lines))

  ## 5 CREATE TRELLIS OBJECT
  if(what == "s")
  {
    graph <- xyplot(Rframe$Value~Bframe$Value[Bframe$Series=="SB"],
                    main=mymain, xlab=myxlab, ylab=myylab, scales=myscales,
                    pch=pch, cex=cex.points, col=col.points, ...)
    graph$x.limits[1] <- 0
  }
  else if(what=="d" && nrow(Bframe)>0)
  {
    graph <- xyplot(Value~Year, groups=Series, data=Bframe, panel=panel.linebar, type="l", bars=Yframe,
                    main=mymain, xlab=myxlab, ylab=myylab, scales=myscales, key=mykey,
                    lty=lty.lines, lwd=lwd.lines, col=col.lines, ...)
  }
  else  # what=="l" || (what=="d" && nrow(Bframe)>0)
  {
    graph <- xyplot(Value~Year, data=Yframe, panel=panel.bar,
                    main=mymain, xlab=myxlab, ylab=myylab, scales=myscales, ...)
  }
  graph$y.limits[1] <- 0  # single-panel plot

  ## 6 FINISH
  if(plot.it)
  {
    print(graph)
    invisible(x)
  }
  else
  {
    invisible(graph)
  }
}


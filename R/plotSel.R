"plotSel" <-
function(model, together=FALSE, series=NULL, sex=NULL, axes=TRUE, legend="bottom", main="", xlab="", ylab="", cex.main=1.2,
         cex.legend=1, cex.lab=1, cex.strip=0.8, cex.axis=0.8, las=1, tck=0, tick.number=5, lty.grid=3, col.grid="grey",
         pch="m", cex.points=1, col.points="black", lty.lines=1, lwd.lines=4, col.lines=c("red","blue"), plot.it=TRUE, ...)
{
  ## 1 DEFINE FUNCTIONS
  panel.each <- function(x, y, subscripts, maturity, col.lines.vector, ...)
  {
    panel.grid(h=-1, v=-1, lty=lty.grid, col=col.grid)
    panel.points(maturity$Age, maturity$P, col=col.points, ...)
    panel.lines(x, y, col=col.lines.vector[subscripts], ...)
  }
  panel.together <- function(x, y, subscripts, maturity, ...)
  {
    panel.grid(h=-1, v=-1, lty=lty.grid, col=col.grid)
    panel.points(maturity$Age, maturity$P, col=col.points, ...)
    panel.superpose(x, y, type="l", subscripts=subscripts, col=col.lines, ...)
  }

  ## 2 PARSE ARGS
  if(class(model) != "scape")
    stop("The 'model' argument should be a scape object, not ", chartr("."," ",class(model)), ".")

  ## 3 PREPARE DATA (extract, rearrange, filter)
  x <- model$Sel
  mat <- x[x$Series=="Maturity",]
  if(is.null(series))
    series <- unique(x$Series)
  if(is.null(sex))
    sex <- unique(x$Sex)
  ok.series <- x$Series %in% series; if(!any(ok.series)) stop("Please check if the 'series' argument is correct.")
  ok.sex <- x$Sex %in% sex; if(!any(ok.sex)) stop("Please check if the 'sex' argument is correct.")
  x <- x[ok.series & ok.sex,]
  if(!is.factor(x$Series))
    x$Series <- factor(paste("Series", x$Series))
  mat <- x[x$Series=="Maturity",]
  sel <- x[x$Series!="Maturity",]
  sel$Series <- factor(as.character(sel$Series))  # update levels

  ## 4 PREPARE PLOT (check device, vectorize args, create list args)
  require(grid, quietly=TRUE, warn.conflicts=FALSE)
  require(lattice, quietly=TRUE, warn.conflicts=FALSE)
  if(trellis.par.get()$background$col == "#909090")
  {
    for(d in dev.list()) dev.off()
    trellis.device(color=FALSE)
  }
  lty.lines <- rep(lty.lines, length.out=max(2,nlevels(sel$Series)))  # 2 <= length(lty.lines) <- nlevels(sel$Series)
  lwd.lines <- rep(lwd.lines, length.out=max(2,nlevels(sel$Series)))  #             lwd.lines
  col.lines <- rep(col.lines, length.out=max(2,nlevels(sel$Series)))  #             col.lines
  mymain <- list(label=main, cex=cex.main)
  myxlab <- list(label=xlab, cex=cex.lab)
  myylab <- list(label=ylab, cex=cex.lab)
  myrot <- switch(as.character(las), "0"=list(x=list(rot=0),y=list(rot=90)), "1"=list(x=list(rot=0),y=list(rot=0)),
                  "2"=list(x=list(rot=90),y=list(rot=0)), "3"=list(x=list(rot=90),y=list(rot=90)))
  myscales <- c(list(draw=axes,cex=cex.axis,tck=tck,tick.number=tick.number), myrot)
  mystrip <- list(cex=cex.strip)
  mykey <- list(space=legend, text=list(lab=levels(sel$Series),cex=cex.legend),
                lines=list(lty=lty.lines,lwd=lwd.lines,col=col.lines))

  ## 5 CREATE TRELLIS OBJECT
  if(!together)
  {
    graph <- xyplot(P~Age|Series*Sex, maturity=mat, data=sel, panel=panel.each, as.table=TRUE,
                    main=mymain, xlab=myxlab, ylab=myylab, scales=myscales, par.strip.text=mystrip,
                    pch=pch, col.points=col.points, cex=cex.points, lty=lty.lines, lwd=lwd.lines,
                    col.lines.vector=col.lines[x$Sex], ...)
  }
  else
  {
    graph <- xyplot(P~Age|Sex, maturity=mat, groups=Series, data=sel, panel=panel.together,
                    main=mymain, xlab=myxlab, ylab=myylab, scales=myscales, par.strip.text=mystrip, key=mykey,
                    pch=pch, col.points=col.points, cex=cex.points, lty=lty.lines, lwd=lwd.lines, col.line=col.lines, ...)
  }
  ages <- sort(unique(x$Age))
  if(is.list(graph$x.limits))                                                            # set xlim=0,max(ages) & ylim=0,1
  {
    graph$x.limits <- lapply(graph$x.limits, function(x){x<-c(0,max(ages));return(x)})   # multi-panel plot
    graph$y.limits <- lapply(graph$y.limits, function(y){y<-c(-0.005,1.005);return(y)})  # multi-panel plot
  }
  else
  {
    graph$x.limits <- c(0, max(ages))                                                    # single-panel plot
    graph$y.limits <- c(-0.005,1.005)                                                    # single-panel plot
  }
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


"plotCA" <-
function(model, what="c", fit=TRUE, series=NULL, sex=NULL, years=NULL, ages=NULL, axes=TRUE, same.limits=TRUE,
         log.transform=FALSE, base.log=10, eps.log=1e-5, main="", xlab="", ylab="", cex.main=1.2, cex.lab=1, cex.strip=0.8,
         cex.axis=0.8, las=!fit, tck=c(1,fit)/2, tick.number=5, lty.grid=3, col.grid="grey", pch=16, cex.points=0.5,
         col.points="black", lty.lines=1, lwd.lines=2, col.lines=c("red","blue"), plot.it=TRUE, ...)
{
  ## 1 DEFINE FUNCTIONS
  panel.bubble <- function(x, y, ...)  # bubble plot obs in one single-sex panel
  {
    panel.abline(v=pretty(x,tick.number), h=pretty(y,tick.number), lty=lty.grid, col=col.grid)
    panel.xyplot(x, y, ...)
  }
  panel.obs <- function(x, y, ...)  # overlay male and female lines in year panels
  {
    panel.abline(v=pretty(x,tick.number), lty=lty.grid, col=col.grid)
    panel.superpose(x, y, ...)
  }
  panel.fit <- function(x, y, subscripts, col.points, col.lines, ...)  # overlay obs and fit in sex:year panels
  {
    panel.abline(v=pretty(x,tick.number), lty=lty.grid, col=col.grid)
    panel.superpose.2(x, y, subscripts=subscripts, col.symbol=col.points[subscripts], col.line=col.lines[subscripts], ...)
  }

  ## 2 PARSE ARGS
  if(class(model) != "scape")
    stop("The 'model' argument should be a scape object, not ", chartr("."," ",class(model)), ".")
  what <- match.arg(what, c("c","s"))
  relation <- if(same.limits) "same" else "free"
  las <- as.numeric(las)

  ## 3 PREPARE DATA (extract, rearrange, filter, transform)
  if(what == "c")
  {
    if(!any(names(model)=="CAc"))
    {
      cat("Commercial C@A data (", substitute(model), "$CAc) not found. Assuming what=\"s\" was intended.\n", sep="")
      what <- "s"
    }
    else
      x <- model$CAc
  }
  if(what == "s")  # value of 'what' may have changed
  {
    if(!any(names(model)=="CAs"))
      stop("Survey C@A data (", substitute(model), "$CAs) not found, try plotCA(x, what=\"c\") to plot commercial C@A")
    x <- model$CAs
  }
  x <- data.frame(Series=rep(x$Series,2), Year=rep(x$Year,2), SS=rep(x$SS,2), Sex=rep(x$Sex,2), Age=rep(x$Age,2),
                  ObsFit=c(rep("Obs",nrow(x)),rep("Fit",nrow(x))), P=c(x$Obs,x$Fit))
  if(is.null(series))
    series <- unique(x$Series)
  if(is.null(sex))
    sex <- unique(x$Sex)
  if(is.null(years))
    years <- unique(x$Year)
  if(is.null(ages))
    ages <- unique(x$Age)
  if(length(series) > 1)
  {
    series <- sort(series)[1]
    cat("More than one C@A series found. Assuming series=\"", series[1], "\" was intended.\n", sep="")
  }
  ok.series <- x$Series %in% series; if(!any(ok.series)) stop("Please check if the 'series' argument is correct.")
  ok.sex    <- x$Sex    %in% sex;    if(!any(ok.sex))    stop("Please check if the 'sex' argument is correct.")
  ok.years  <- x$Year   %in% years;  if(!any(ok.years))  stop("Please check if the 'years' argument is correct.")
  ok.ages   <- x$Age    %in% ages;   if(!any(ok.ages))   stop("Please check if the 'ages' argument is correct.")
  x <- x[ok.series & ok.sex & ok.years & ok.ages,]
  if(nrow(x) == 0)
    stop("Empty data frame. Please check if subsetting args (series, sex, years, ages) had mistakes.")
  nsexes <- length(unique(x$Sex))
  if(log.transform)
    x$P <- log(x$P+eps.log, base=base.log)

  ## 4 PREPARE PLOT (check device, vectorize args, create list args)
  require(grid, quietly=TRUE, warn.conflicts=FALSE)
  require(lattice, quietly=TRUE, warn.conflicts=FALSE)
  if(trellis.par.get()$background$col == "#909090")
  {
    for(d in dev.list()) dev.off()
    trellis.device(color=FALSE)
  }
  col.points <- rep(col.points, length.out=2)
  col.lines <- rep(col.lines, length.out=2)
  mymain <- list(label=main, cex=cex.main)
  myxlab <- list(label=xlab, cex=cex.lab)
  myylab <- list(label=ylab, cex=cex.lab)
  myrot <- switch(as.character(las), "0"=list(x=list(rot=0),y=list(rot=90)), "1"=list(x=list(rot=0),y=list(rot=0)),
                  "2"=list(x=list(rot=90),y=list(rot=0)), "3"=list(x=list(rot=90),y=list(rot=90)))
  myscales <- c(list(draw=axes,relation=relation,cex=cex.axis,tck=tck,tick.number=tick.number), myrot)
  mystrip <- list(cex=cex.strip)

  ## 5 CREATE TRELLIS OBJECT
  fixed.ylim <- FALSE
  if(nsexes==1 && !fit)
  {
    x <- x[x$ObsFit=="Obs",]
    col.points <- ifelse(x$P==0, "transparent", col.points)  # pch=NA doesn't work if pch is a character
    mycex <- cex.points*sqrt(x$P/mean(x$P)) + 1/1000  # cex=0 is illegal on PDF device
    graph <- xyplot(Year~Age|switch(what,c="Commercial C@A","Survey C@A"), data=x, panel=panel.bubble,
                    main=mymain, xlab=myxlab, ylab=myylab, par.strip.text=mystrip, scales=myscales,
                    pch=pch, cex=mycex, col=col.points, ...)
    graph$y.limits <- rev(graph$y.limits)
    fixed.ylim <- TRUE
  }
  if(nsexes==1 && fit)
  {
    graph <- xyplot(P~Age|factor(Year), groups=ObsFit, data=x, panel=panel.fit, type=c("l","p"), as.table=TRUE,
                    main=mymain, xlab=myxlab, ylab=myylab, par.strip.text=mystrip, scales=myscales,
                    pch=pch, cex=cex.points, col.points=col.points[x$Sex], lty=lty.lines, lwd=lwd.lines,
                    col.lines=col.lines[x$Sex], ...)
  }
  if(nsexes==2 && !fit)
  {
    x <- x[x$ObsFit=="Obs",]
    graph <- xyplot(P~Age|factor(Year), groups=Sex, data=x, panel=panel.obs, type="l", as.table=TRUE,
                    main=mymain, xlab=myxlab, ylab=myylab, par.strip.text=mystrip, scales=myscales,
                    lty=lty.lines, lwd=lwd.lines, col=col.lines, ...)
  }
  if(nsexes==2 && fit)
  {
    graph <- xyplot(P~Age|factor(Year)*Sex, groups=ObsFit, data=x, panel=panel.fit, type=c("l","p"), as.table=TRUE,
                    main=mymain, xlab=myxlab, ylab=myylab, par.strip.text=mystrip, scales=myscales,
                    pch=pch, cex=cex.points, col.points=col.points[x$Sex], lty=lty.lines, lwd=lwd.lines,
                    col.lines=col.lines[x$Sex], ...)
  }
  if(!log.transform && !fixed.ylim)  # leave ylim alone if log-transformed or bubble plot
  {
    if(is.list(graph$y.limits))                                                 # set lower ylim to 0
      graph$y.limits <- lapply(graph$y.limits, function(y){y[1]<-0;return(y)})  # multi-panel plot
    else
      graph$y.limits[1] <- 0                                                    # single-panel plot
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


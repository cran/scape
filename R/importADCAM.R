importADCAM <- function(dir)
{
  ## Implementation note: keep original column names in xYearAge/xYear/xAge, and
  ##                      use 'tmp' data frames for readable code

  ## 1  Import results
  xYearAge <- read.table(paste(dir,"resultsbyyearandage",sep="/"), header=TRUE,
                         na.strings="-1")
  xYear <- read.table(paste(dir,"resultsbyyear",sep="/"), header=TRUE,
                      na.strings="-1")
  xAge <- read.table(paste(dir,"resultsbyage",sep="/"), header=TRUE,
                     na.strings="-1")

  ## 2  Examine periods and years
  periods <- scan(paste(dir,"catchparameters.dat",sep="/"), nlines=1,
                  quiet=TRUE)
  periods <- c(xYear$year[1], periods+1)
  catch.years <- xYear$year[!is.na(xYear$CatchIn1000tons)]
  survey.years <- xYear$year[!is.na(xYear$ObsSurveyBiomass1)]
  years <- seq(min(catch.years,survey.years), max(catch.years+1,survey.years))
  series <- c(paste(periods, c(periods[-1]-1,max(catch.years)), sep="-"),
              "Survey")  # 1980-1996, 1997-2010, Survey
  ages <- xAge$age

  ## 3  Read N, Dev, B, Sel
  model <- list()
  tmp <- xYearAge[xYearAge$year<=max(years),]
  model$N <- data.frame(Sex="Unisex", Year=tmp$year, Age=tmp$age, N=tmp$N,
                        stringsAsFactors=FALSE)
  tmp <- model$N[model$N$Year==min(years)&model$N$Age>1,]
  model$Dev$Initial <- log(tmp$N) - mean(log(tmp$N))
  names(model$Dev$Initial) <- ages[-1]
  tmp <- model$N[model$N$Age==1,]
  model$Dev$Annual <- log(tmp$N) - mean(log(tmp$N))
  names(model$Dev$Annual) <- years
  tmp <- xYear[xYear$year<=max(years),]
  model$B <- data.frame(Year=tmp$year, VB=tmp$RefBio2, SB=tmp$Spawningstock,
                        Y=tmp$CatchIn1000tons, R=tmp$Recruitment)
  Sel <- data.frame(Series=rep(series,each=nrow(xAge)), Sex="Unisex",
                    Age=xAge$age, P=as.numeric(NA), stringsAsFactors=FALSE)
  for(i in seq_along(periods))
  {
    tmp <- xYearAge[xYearAge$year==periods[i],]
    Sel$P[Sel$Series==series[i]] <- ifelse(tmp$F==0, NA, tmp$F/max(tmp$F))
  }
  Sel$P[Sel$Series=="Survey"] <- ifelse(xAge$SurveylnQ1==1, NA, xAge$SurveylnQ1)
  tmp <- Sel[Sel$Series=="Survey",]
  Sel$P[Sel$Series=="Survey"] <- tmp$P / max(tmp$P,na.rm=TRUE)
  model$Sel <- Sel

  ## 4  Read Survey, CAc, CAs
  tmp <- xYear[xYear$year<=max(years),]
  model$Survey <- data.frame(Series=1L, Year=tmp$year,
                             Obs=tmp$ObsSurveyBiomass1,
                             CV=ifelse(is.na(tmp$ObsSurveyBiomass1),NA,0),
                             Fit=ifelse(tmp$CalcSurveyBiomass1==0,NA,
                                 tmp$CalcSurveyBiomass1))
  tmp <- xYearAge[xYearAge$year<=max(catch.years),]
  model$CAc <- data.frame(Series=1L, Year=tmp$year, SS=tmp$CatchDiff,
                          Sex="Unisex", Age=tmp$age, Obs=tmp$ObsCno,
                          Fit=ifelse(tmp$CalcCno==0,NA,tmp$CalcCno),
                          stringsAsFactors=FALSE)
  tmp <- xYearAge[xYearAge$year%in%survey.years,]
  model$CAs <- data.frame(Series=1L, Year=tmp$year,
                          SS=ifelse(tmp$SurveyResiduals1==0,NA,
                              tmp$SurveyResiduals1),
                          Sex="Unisex", Age=tmp$age, Obs=tmp$ObsSurveyNr1,
                          Fit=ifelse(tmp$CalcSurveyNr1==0,NA,tmp$CalcSurveyNr1),
                          stringsAsFactors=FALSE)

  ## 5  Create attributes
  attr(model,"call") <- match.call()
  class(model) <- "scape"

  model
}

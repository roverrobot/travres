## westafricatravel.R
## Created:  1 Nov 2014
## Changed:
##

source("common.R") # stuff common to multiple R scripts

library("tikzDevice")
tikz("westafricatravel.tex",width=6,height=8,standAlone=TRUE)

## plot parameters
par(mar=mymar)
country.names <- c("Guinea","Liberia","Sierra.Leone")
ncountries <- length(country.names)
par(mfrow=c(ncountries,1))

## MONTHLY TRAVEL INTO CANADA FROM WEST AFRICAN COUNTRIES
with(west.africa.monthly,{
  myxlim <- c(1990,2014+9/12) + c(-1,1)*0.5/12
  time <- year + (month-0.5)/12
  for (i in 1:ncountries) {
    country.name <- country.names[i]
    country <- get(country.name)
    myylim <- c(0,max(country))
    plot(0,0,xlim=myxlim,ylim=myylim,cex.axis=mycex.axis,
         xaxs="i",yaxs="i",bty="L",xlab="",ylab="",
         type="n",pch=21,bg="grey",las=1)#,xpd=NA)
    abline(v=1991:2020,col="grey",lty="dotted")
    points(time,country,type="b",pch=21,bg="red",xpd=NA)
    cname.no.dots <- gsub('.',' ',country.name,fixed=TRUE)
    legend("topleft",cex=1.5,bty="n",
           legend=sprintf("\\textbf{From %s to Canada}",cname.no.dots))
  }
})
dev.off()

## SAVE SUMMARY FROM WHICH MEDIANS SINCE 2010 CAN BE EXTRACTED
start.year <- 2010
wam <- west.africa.monthly[west.africa.monthly$year >= start.year,
                           country.names]
summary(wam)
wamtab <- round(rbind(sapply(wam,min),sapply(wam,median),sapply(wam,max)))
rownames(wamtab) <- c("Min","Median","Max")
colnames(wamtab) <- gsub("."," ",colnames(wamtab),fixed=TRUE)
wamtab
library(Hmisc)

x=latex(wamtab, file = "wamtab.tex", table.env = FALSE,
      title=sprintf("\\emph{Since %d}",start.year))

## travel.R
## Created: 31 Oct 2014 (based on ../../PLoSCompBio/data/travel.sm)
## Changed: 12 Nov 2014
##

source("common.R") # stuff common to multiple R scripts

##pdf("travel.pdf",width=6,height=8)
library("tikzDevice")
tikz("travel.tex",width=6,height=8,standAlone=TRUE)

## plot parameters
par(mfrow=c(5,1))
mymar["left"] <- mymar["left"] + 3
par(mar=mymar)

## parameters
endyear <- 2014
myxlim <- c(1945,endyear+1)

## ANNUAL TRAVEL TO US AND CANADA ON LOG SCALE
yrange <- range(c(travel.to.us$us, travel.to.ca$Canadians, travel.to.ca$nonUSaC),na.rm=TRUE)
with(travel.to.us,{
  plot(year,us,ylim=yrange,xlim=myxlim,xaxs="i",bty="L",xlab="",ylab="",
       log="y",yaxt="n",cex.axis=mycex.axis,
       type="n",las=1,xpd=NA)
  grid(lty="dotted")
  points(year,us,type="b",pch=21,bg="black",xpd=NA)
})
eaxis(2, at.small=FALSE, lab.type="latex", cex.axis=mycex.axis)
title(ylab="Annual Travellers",cex.lab=mycex.axis)
with(travel.to.ca,
     points(year,Canadians,type="b",pch=21,bg="red",las=1,xpd=NA))
## remove NAs so type="b" works as desired:
with(subset(travel.to.ca,!is.na(nonUSaC)),
     points(year,nonUSaC,type="b",pch=21,bg="blue",xpd=NA))
legend("bottomright",pch=21,pt.bg=c("black","red","blue"),bty="n",
       legend=c("To United States (not from Canada)","To Canada (Canadians)","To Canada (non-Canadians, not from US)"))

## ANNUAL TRAVEL TO CANADA ON LINEAR SCALE
## with(travel.to.ca,{
##   plot(year,Canadians,xlim=myxlim,xaxs="i",bty="L",xlab="",ylab="",
##        type="b",pch=21,bg="red",las=1,xpd=NA)
##   points(year,nonUSaC,type="b",pch=21,bg="blue",xpd=NA)
## })

## TRAVEL TO CANADA BY COUNTRY
with(travel.to.ca.ann.bycountry,{
  myxlim <- range(year) + c(-1,1)
  myylim <- c(0,max(nonresident.nonUS))
  plot(year,nonresident.nonUS,xlim=myxlim,ylim=myylim,cex.axis=mycex.axis,
       yaxt="n",xaxs="i",yaxs="i",bty="L",xlab="",ylab="",
       type="n",las=1,xpd=NA)
  eaxis(2, at.small=FALSE, lab.type="latex", cex.axis=mycex.axis)
  grid()
  points(year,nonresident.nonUS,type="b",pch=21,bg="blue",xpd=NA)
  points(year,Europe,type="b",pch=21,bg="red",xpd=NA)
  points(year,Asia,type="b",pch=21,bg="green",xpd=NA)
  points(year,Africa,type="b",pch=21,bg="yellow",xpd=NA)
  legend("topleft",pch=21,pt.bg=c("blue","red","green","yellow"),bty="n",
         legend=c("To Canada from all countries except US",
           "To Canada from Europe",
           "To Canada from Asia",
           "To Canada from Africa"))
  ## smaller vertical scale plot:
  myylim <- c(0,max(Oceania*1.1))
  plot(year,Oceania,xlim=myxlim,ylim=myylim,cex.axis=mycex.axis,
       xaxs="i",yaxs="i",bty="L",xlab="",ylab="",
       type="n",las=1,xpd=NA)
  grid()
  points(year,Oceania,type="b",pch=21,bg="cyan",xpd=NA)
  points(year,SouthAmerica,type="b",pch=21,bg="red",xpd=NA)
  points(year,Africa,type="b",pch=21,bg="yellow",xpd=NA)
  points(year,China,type="b",pch=21,bg="green",xpd=NA)
  points(year,Taiwan,type="b",pch=21,bg="black",xpd=NA)
  legend("topleft",pch=21,
         pt.bg=c("cyan","red","yellow","green","black"),bty="n",
         legend=c("To Canada from Oceania",
           "To Canada from South America",
           "To Canada from Africa",
           "To Canada from China",
           "To Canada from Taiwan"))
})

## MONTHLY TRAVEL INTO US
with(travel.to.us.monthly,{
  myxlim <- c(min(year),2014+9/12) + c(-1,1)*0.5/12
  time <- year + (month-0.5)/12
  y <- Overseas
  myylim <- c(0,max(y))
  plot(time,y,xlim=myxlim,ylim=myylim,cex.axis=mycex.axis,
       yaxt="n",xaxs="i",yaxs="i",bty="L",xlab="",ylab="",
       type="b",pch=21,bg="black",las=1,xpd=NA)
  eaxis(2, at.small=FALSE, lab.type="latex", cex.axis=mycex.axis)
  abline(v=1997:2020,col="grey",lty="dotted")
  points(time[month==9],y[month==9],cex=1.5,col="red")#,xpd=NA)
})

## MONTHLY TRAVEL INTO CANADA
with(travel.to.ca.monthly,{
  time <- year + (month-0.5)/12
  points(time,total,type="b",pch=21,bg="blue")#,xpd=NA)
  points(time[month==9],total[month==9],cex=1.5,col="red")#,xpd=NA)
})

legend("topright",pch=21,col=c("black","black","red"),
       pt.bg=c("black","blue","white"),bty="n",
       legend=c("To United States","To Canada","\\textit{September}"))

## MONTHLY TRAVEL INTO CANADA FROM HONG KONG AND CHINA
with(travel.to.ca.monthly,{
  myxlim <- c(1996,2014+9/12) + c(-1,1)*0.5/12
  myylim <- c(0,max(Hong.Kong*2.4))
  time <- year + (month-0.5)/12
  plot(time,Hong.Kong,xlim=myxlim,ylim=myylim,cex.axis=mycex.axis,
       xaxs="i",yaxs="i",bty="L",xlab="",ylab="",
       type="b",pch=21,bg="green",las=1)#,xpd=NA)
  abline(v=1997:2020,col="grey",lty="dotted")
  points(time,China,type="b",pch=21,bg="red")#,xpd=NA)
  points(time,Africa,type="b",pch=21,bg="yellow")#,xpd=NA)
})
legend("topleft",pch=21,pt.bg=c("green","red","yellow"),bty="n",
       legend=c("From Hong Kong to Canada",
         "From China to Canada",
         "From Africa to Canada"))

dev.off()

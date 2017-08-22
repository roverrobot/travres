## travel.R
## Created: 31 Oct 2014
## Changed:

library("gdata") # for read.xls()

## ANNUAL TRAVEL TO US

travel.to.us <- read.table("data/travel_to_us.dat")
colnames(travel.to.us) <- c("year","us")

## ANNUAL TRAVEL TO CANADA

travel.to.ca.nonUSaC <- read.table("data/travel_to_ca_non_USaC.dat")
colnames(travel.to.ca.nonUSaC) <- c("year","ca.nonUSaC")

travel.to.ca.canadians <- read.table("data/travel_to_ca_canadians.dat")
colnames(travel.to.ca.canadians) <- c("year","ca.canadians")

travel.to.ca <- cbind(travel.to.ca.canadians, travel.to.ca.nonUSaC$ca.nonUSaC)
colnames(travel.to.ca) <- c("year","Canadians","nonUSaC")

## ANNUAL TRAVEL TO CANADA BY COUNTRY
## FIX: xls reading is failing... getting garbage I don't understand...
travel.to.ca.ann.bycountry.xls <- read.xls("data/StatCanCdnAnn72-13.xls",sheet=3,skip=6,blank.lines.skip=TRUE)
## remove last three lines, which are comments:
n <- nrow(travel.to.ca.ann.bycountry.xls)
print(n)
travel.to.ca.ann.bycountry.xls <- travel.to.ca.ann.bycountry.xls[1:(n-3),]
## fix column names:
correct.colnames <- c("year","nonresident.nonUS","Europe","Africa","Asia","HongKong","India","China","Pakistan","Phillipines","Singapore","Taiwan","Vietnam","NorthAmerica","Mexico","SouthAmerica","Oceania")
colnames(travel.to.ca.ann.bycountry.xls) <- correct.colnames

travel.to.ca.ann.bycountry <- read.csv("data/travel.to.ca.ann.bycountry.csv",comment.char="#",header=FALSE)
colnames(travel.to.ca.ann.bycountry) <- correct.colnames

## MONTHLY TRAVEL INTO US
travel.to.us.monthly <- read.xls("data/USmonthly1996to2005.xls",sheet=1,skip=5,blank.lines.skip=TRUE)
## remove last three lines, which are comments:
n <- nrow(travel.to.ca.ann.bycountry.xls)
print(n)
travel.to.ca.ann.bycountry.xls <- travel.to.ca.ann.bycountry.xls[1:(n-3),]
## fix column names:
correct.colnames <- c("year","month","month.name","total","Canada","Mexico","Overseas","Europe","Asia","Middle.East","Africa","Oceania","South.America","Central.America","Carribean")
colnames(travel.to.us.monthly)[1:length(correct.colnames)] <- correct.colnames

## MONTHLY TRAVEL INTO CANADA

travel.to.ca.monthly <- read.csv("data/travel.to.ca.monthly.csv",comment.char="#",header=FALSE)
correct.colnames <- c("date","year","month","total","Europe","Africa","Asia","Hong.Kong","India","Japan","China","Pakistan","Phillipines","Singapore","Taiwan","Vietnam","North.America","Mexico","South.America","Oceania")
colnames(travel.to.ca.monthly) <- correct.colnames

## MONTHLY TRAVEL INTO CANADA FROM OLD STYLE FILE

travel.to.ca.monthly <- read.csv("data/travel.to.ca.monthly.csv",comment.char="#",header=FALSE)
correct.colnames <- c("date","year","month","total","Europe","Africa","Asia","Hong.Kong","India","Japan","China","Pakistan","Phillipines","Singapore","Taiwan","Vietnam","North.America","Mexico","South.America","Oceania")
colnames(travel.to.ca.monthly) <- correct.colnames

## MONTHLY TRAVEL INTO CANADA FROM CANSIM

africa.monthly <- read.csv("data/africa.monthly.csv")

## convert date to month number and full year number
month.names.abbrev <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month.nums <- 1:12
names(month.nums) <- month.names.abbrev
d.raw <- as.character(africa.monthly[,"date"])
d.split <- t(as.data.frame(strsplit(d.raw,"-")))
dim(d.split)
head(d.split)
d.month.name <- d.split[,1]
d.month <- month.nums[d.month.name]
d.year <- as.numeric(d.split[,2])
d.year <- d.year + ifelse(d.year < 40, 2000, 1900)

africa.monthly$year <- d.year
africa.monthly$month <- d.month

## West Africa only:
west.africa.monthly <- subset(africa.monthly,year>=1990,select=c(year,month,date,total.nonUS,Africa.total,Guinea,Liberia,Sierra.Leone))

## SAVE ALL DATA IN Rdata FILE
save(file="traveldata.rda",
     travel.to.us,
     travel.to.ca,
     travel.to.ca.ann.bycountry,
     travel.to.us.monthly,
     travel.to.ca.monthly,
     africa.monthly,
     west.africa.monthly
     )

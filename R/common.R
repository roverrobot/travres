## load travel data
load("traveldata.rda")
source("newaxis.R") # eaxis() function

mycex.axis <- 1.5
## margin settings
mar.orig <- par("mar")
names(mar.orig) <- c("bottom", "left", "top", "right")
mar.wideright <- mar.orig
mar.wideright["right"] <- mar.wideright["right"] + 3
mymar <- mar.orig
mymar[c("top","bottom","right")] <- c(1,2,1)

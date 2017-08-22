require(gdata)
d=read.xls("Ebola.xls", sheet=2, header=TRUE, 
           na.strings=c("", " ", "-"))
# cross tabulate
y=xtabs(Value~Country+Date+Category+Localite+Sources, data=d)
l=dim(y)
# pick out national data
E = matrix(0, nrow=l[2], ncol=l[1]+1)
E = as.data.frame(E)
colnames(E) = c("Date", dimnames(y)$Country)
# For each country on each single day and the records labeled "Cases",
# find the largest value among all sources and region
# assume that it is the national count
for (i in 1:l[1])
    for(j in 1:l[2])
        E[j, i+1] = max(y[i, j,"Cases", ,], na.rm=TRUE)
E[,1] = dimnames(y)$Date
E=E[2:nrow(E),]

# fill in NA for 0s in between values (except for leading 0s)
for (j in 2:ncol(E))
    for(i in 2:nrow(E))
        if (E[i, j] == 0 && (is.na(E[i, j-1]) || E[i,j-1] >0) )
            E[i,j] = NA
write.table(E, file="Ebola", col.names=TRUE, row.names=FALSE)


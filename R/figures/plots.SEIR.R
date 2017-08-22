# senario:
# t=0 corresponds to the first import
# constant import rate
# vary the following parameters:
#   the reproductive ratio R (pick R=1.8 and R=1.8/5)
#   the time to implement restriction
#   the leak probability of the restriction
require(ggplot2)
cb = library(RColorBrewer, logical.return=TRUE)

source("diseases.R")

# generate a data frame for the plot
# for R0 and R0/Tinf
# some initial conditions I0s
# the import rate cim
# some leak probabilities ps
# some travel restriction times t0s
plot.data <- function(t, I0, cim=1, ps, t0s, disease) {
    # the data frame is stored in data
    deaths = data.frame()
    beds = data.frame()
    Tinf = disease$Tinf
    for (R in c(disease$R0, disease$R0/disease$Tinf)) {
        disease$Tinf = disease$Tinf*R/disease$R0
        for (I0 in I0s)
            for (p in ps) 
                for (t0 in t0s) {
                    X = solution(t, t0, p, R, I0, cim, disease)
                    deaths = rbind(deaths, data.frame(
                        t = X[,1],
                        count = X[, "D"],
                        R = paste("R",R,sep="="), 
                        I0 = paste("I0",I0,sep="="), 
                        leak = paste("leak",p, sep="="),
                        t0 = paste("t_r",t0, sep="="),
                        disease = disease$name
                    ))
                    beds = rbind(beds, data.frame(
                        t = X[,1],
                        count = X[, "Hd"] + X[, "Hl"],
                        R = paste("R",R,sep="="), 
                        I0 = paste("I0",I0,sep="="), 
                        leak = paste("leak",p, sep="="),
                        t0 = paste("t_r",t0, sep="="),
                        disease = disease$name
                    ))
                    
                }
    }
    list(beds=beds, deaths=deaths)
}

ebola.data=plot.data(t, I0s, cim=cim, ps, t0s, ebola)
sars.data=plot.data(t, I0s, cim=cim, ps, t0s, sars)

plot.curves <-function(data, ylab, m=NA) {
    ind = which(data$R=="R=0.36")
    if (is.na(m)) m = max(data$count[ind])
    
    fig <- ggplot(data=data, aes(x=t, y=count, linetype=leak, col=R)) + geom_line() +
        theme_bw() + facet_grid(t0 ~ disease,scales="free") +
        scale_y_continuous(ylab, limits=c(0, m)) +
        scale_x_continuous("days")

    if (cb) {
        cbPalette <- c("#000000", brewer.pal(3,"Set1"))
        fig <- fig +
#            scale_fill_manual(values=cbPalette) +
            scale_colour_manual(values=cbPalette)
    }

}

if (exists("use.pdf") && use.pdf) pdf(file="deaths.seir.pdf", width=5, height=5)
print(plot.curves(rbind(ebola.data$deaths, sars.data$deaths), ylab="total deaths", m=NA))
if (exists("use.pdf") && use.pdf) dev.off()

if (exists("use.pdf") && use.pdf) pdf(file="beds.seir.pdf", width=5, height=5)
print(plot.curves(rbind(ebola.data$beds, sars.data$beds), ylab="hospital beds occupied", m=15))
if (exists("use.pdf") && use.pdf) dev.off()

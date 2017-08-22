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

source("models.R")

# generate a data frame for the plot for 
#   R0 and R0/Tinf
#   an initial condition I0
#   the import rate cim
#   some leak probabilities ps
#   some travel restriction times t0s
#   the disease parameters
#   linear=TRUE: use the linear model rather than the full SEIR model
plot.data <- function(t, I0, cim, ps, t0s, disease, linear=TRUE) {
    # the data frame is stored in data
    deaths = data.frame()
    beds = data.frame()
    Tinf = disease$Tinf
    for (R in c(disease$R0, disease$R0/disease$Tinf)) {
        disease$Tinf = disease$Tinf*R/disease$R0
        for (p in ps) 
            for (t0 in t0s) {
                X = solution(t, t0, p, R, I0, cim, disease, linear)
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
    list(linear=linear, beds=beds, deaths=deaths)
}

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

# split off the file extension from the base from the filename base.extension
filename.split <- function(filename) {
    l = strsplit(filename, "\\.")[[1]]
    if (length(l)==1) {
        type = ""
        base = filename
    } else {
        type = l[length(l)]
        base = paste(l[-length(l)], sep=".", collapse=".")
    }
    list(type=type, base=base)
}

# create a plot with
#   file: the filename of the plot, if NA, then plot on screen
#         the type of file is determined by its suffix. Currently it supports:
#         "pdf": print in PDF
#         "tex": print in tex
#         "png": print in PNG
#   fig:  the ggplot figure to be printed
print.plot <- function(file=NA, fig, width=5, height=5) {
    if (!is.na(file)) {
        switch(filename.split(file)$type,
               tex = {
                   texlib = library("tikzDevice", logical.return = TRUE)
                   if (!texlib) stop("tikzDevice package not found")
                   tikzDevice(file=file, width=width, height=height, standAlone=TRUE)
               },
               pdf = pdf(file=file, width=width, height=height),
               ong = png(file=file, width=width, height=height)
        )
    }
    print(fig)
    if (!is.na(file)) dev.off()
}

boolean.part <- function(s, map, var=NA) {
    l = names(map)
    if (!(s %in% l)) {
        NA 
    } else {
        if (!is.na(var)) stop(paste("already specified", paste(l, collapse=" ")))
        map[[s]]
    }
}

parse.filename = function (filename) {
    if (is.na(filename)) return (list(beds=NA, linear=NA, I0=NA, m=NA))
    # split off the file extension (type) from the base
    l = filename.split(filename)
    # split the base of filename into parts separated by "_"
    l = strsplit(tolower(l$base), "_")[[1]]
    # understand each part
    i = 1
    beds=NA
    linear = NA
    I0 = NA
    m = NA
    while (i <= length(l)) {
        x = boolean.part(l[i], map=list(beds=TRUE, deaths=FALSE), beds)
        if (!is.na(x)) {
            beds = x
            i = i + 1
            next
        }
        x = boolean.part(l[i], map=list(exp=TRUE, seir=FALSE), linear)
        if (!is.na(x)) {
            linear = x
            i = i + 1
            next
        }
        var = l[i]
        i = i+1
        number = as.numeric(l[i])
        if (var == "i0") {
            if (!is.na(I0)) stop("I0 has already been specified")
            I0 = number
            i = i+1
            next
        }  
        if (var == "m") {
            if (!is.na(m)) stop("m has already been specified")
            m = number
            i = i+1
            next
        }
        stop(paste("Does not understand", var))
    }
    list(beds=beds, linear=linear, I0=I0, m=m)
}

# create a plot according to the filename
# file: the filename for the figure, file=NA for on screen
#   the filename can be in the form of part1_part2_part3_part4.type
#   the supported types are pdf, png and tex
#   part 1 is either beds or deaths
#   part 2 is either exp or SEIR (case insensitive)
#   part 3 is "I0_" followed by a number,
#   part 4 is "m_" followed by a number
# if file is not NA and a given part is missing, the the value is taken from the value
#   given in other parameters
# linear: whether to use the linear (exp) model or the full SEIR model
# I0: the initial number of infectious individuals
# width and height are the size of the figure in inches
make.plot <- function(file=NA, beds=TRUE, linear=TRUE, I0=0, m=NA, width=5, height=5) {
    parms = parse.filename(file)
    if (!is.na(parms$I0)) I0 = parms$I0
    if (!is.na(parms$m)) m = parms$m
    if (!is.na(parms$beds)) beds = parms$beds
    if (!is.na(parms$linear)) linear = parms$linear
    ebola.data=plot.data(t, I0, cim=cim, ps, t0s, ebola, linear=linear)
    sars.data=plot.data(t, I0, cim=cim, ps, t0s, sars, linear=linear)
    if (beds) {
        data = rbind(ebola.data$beds, sars.data$beds)
        ylab = "hospital beds occupied"
    } else {
        data = rbind(ebola.data$deaths, sars.data$deaths)
        ylab = "total deaths"
    }
    print.plot(file=file, fig=plot.curves(data, ylab=ylab, m=m), width=width, height=height)
}

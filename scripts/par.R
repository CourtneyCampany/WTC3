source("functions and packages/functions.R")
source("functions and packages/packages.R")
source("scripts/plot_objects.R")

par <- read.csv("raw data/par.csv")
treat <- read.csv("raw data/chamber_trt.csv")

#format function
par<- parformat(par)

par_leaf <- subset(par, ID !="shade-high")
Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")

#data format for bar
parbar <- subset(par_leaf, select = c("par", "month", "leaf_type"))
parbar$month <- factor(parbar$month, levels = Morder)
levels(parbar$month)


png(filename = "output/presentations/ppfd.png", width = 11, height = 8.5, units = "in", res= 400)
bar(par, c(leaf_type, month), parbar, col=c("yellowgreen", "green4"), xlab="", ylab="", ylim=c(0, 2000), 
      half.errbar=FALSE)
title(ylab=parlab, mgp=ypos)
#dev.copy2pdf(file="output/ppfd.pdf")
dev.off()


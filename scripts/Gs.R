source("functions and packages/functions.R")
source("functions and packages/packages.R")
source("scripts/plot_objects.R")

#gm data, no drought, no shade high
gmes <- read.csv("calculated data/gm_sunsha.csv")

#gm treatment means--------------------------------------------------------
gm_agg <- summaryBy(gm+Photo+Cond+VpdL ~ month+ leaf+ temp+ drydown, data=gmes, FUN=c(mean, se))
gm_ch <- summaryBy(. ~ chamber+leaf+month+temp+drydown, data=gmes, FUN=c(mean), keep.names=TRUE)


#gs vs Photo stats




#plot conductance and Photosynthesis
windows()
plot(Photo~Cond, data=gm_ch,col=cols[temp], pch=16,subset=leaf == "sun", ylab="", xlab=condlab,ylim = c(0, 30))
  points(Photo~Cond, data=gm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
  title(ylab=satlab, mgp=ypos)
  legend("topleft", leglab, pch=pch4, col=col4, inset = 0.03)
dev.copy2pdf(file="output/A_gs.pdf")
dev.off()

#plot conductance and vpd
windows()
plot(Cond~VpdL, data=gm_ch,col=cols[temp], pch=16,subset=leaf == "sun", ylab="", xlab=vpdlab, ylim=c(0, 0.5), xlim=c(0,4))
points(Cond~VpdL, data=gm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
title(ylab=condlab, mgp=ypos)
legend("topright", leglab, pch=pch4, col=col4, inset = 0.03)
dev.copy2pdf(file="output/vpd_gs.pdf")
dev.off()


#plot A and vpd
windows()
plot(Photo~VpdL, data=gm_ch,col=cols[temp], pch=16,subset=leaf == "sun", ylab="", xlab=vpdlab, ylim=c(0,30), xlim=c(0,4))
points(Photo~VpdL, data=gm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
title(ylab=satlab, mgp=ypos)
legend("topright", leglab, pch=pch4, col=col4, inset = 0.03)
dev.copy2pdf(file="output/vpd_A.pdf")
dev.off()


####bar plot of gm and gs

#gs v gm reorder data
cond <- subset(gm_agg, select = c("Cond.mean", "leaf", "temp"))
cond$type <- "gs"
names(cond)[1] <- "rate"

gm <- subset(gm_agg, select = c("gm.mean", "leaf", "temp"))
gm$type <- "gm"
names(gm)[1] <- "rate"
bardata <- rbind(cond, gm)
bardata$trt <- as.factor(ifelse(bardata$temp == "ambient", "Ambient ", "Elevated (+3C)"))
sunbar <- subset(bardata, leaf=="sun")
shabar <- subset(bardata, leaf=="shade")


#barplots
windows()
bar(rate, c( type,trt), sunbar, ylim=c(0, .5), ylab="", xlab="",col = "green4", density = c(200,5))
title(ylab=ratelab, mgp=ypos, cex=1.2)
title(main="Sun Leaves", line=-1.5, font.main=1, adj=.05,cex.main=1.2)
dev.copy2pdf(file="output/gmgs_sun.pdf")
dev.off()


windows()
bar(rate, c( type,trt), shabar, ylim=c(0, .5), ylab="", xlab="", col = "yellowgreen", density = c(200,5))
title(ylab=ratelab, mgp=ypos)
title(main="Shade Leaves", line=-1.5, font.main=1, adj=.05,cex.main=1.2)
dev.copy2pdf(file="output/gmgs_sha.pdf")
dev.off()

#options
col=c("yellowgreen", "green4")
border=c("blue", "blue","red","red"),
border=c("blue", "blue","red","red"),


#for pp, turn ylab, and axes off
windows()
bar(rate, c( type,trt), shabar, ylim=c(0, .5), ylab="", xlab="", col = "yellowgreen", density = c(200,5), axes=FALSE)
title(main="Shade Leaves", line=-1.5, font.main=1, adj=.05,cex.main=1.2)
dev.copy2pdf(file="output/gmgs_sha_pp.pdf")
dev.off()



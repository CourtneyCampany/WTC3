source("functions and packages/functions.R")
source("functions and packages/packages.R")
source("scripts/read_data.R")
source("scripts/plot_objects.R")


###there is no ch10 shade in march, check for this in script and paired comparisons

#gm data, no drought, no shade high
gmes <- read.csv("calculated data/gm_sunsha.csv")

#gm treatment means--------------------------------------------------------
gm_agg <- summaryBy(gm+Photo+Cond ~ month+ leaf+ temp+ drydown, data=gmes, FUN=c(mean, se))


#leaf data 
leaf <- leafformat(leaf)
leaf_watered <- subset(leaf, drydown =="control")
leaf_watered$massarea <- with(leaf_watered, (leaf_mass/leaf_area)*10000)
lma_agg <- summaryBy(massarea+leaf_area+leaf_mass ~ month+ leaf+ temp+ drydown, data=leaf_watered, FUN=c(mean, se))

gm_ch <- summaryBy(gm+Photo+Cond ~ chamber+month+leaf+temp, data=gmes, FUN=c(mean), keep.names=TRUE)
lma_ch <- summaryBy(massarea+leaf_area+leaf_mass ~ chamber+month+leaf+temp, data=leaf_watered, 
                    FUN=c(mean), keep.names=TRUE)

#need to merge the datasets
leaf_gm <- merge(gm_agg, lma_agg)
lfgm_ch <- merge(gm_ch, lma_ch)


#leaf stats
lmagm_lm <- lm(massarea.mean ~ gm.mean, data=leaf_gm)
summary(lmagm_lm)

massgm_lm <- lm(leaf_mass.mean ~ gm.mean, data=leaf_gm)
summary(massgm_lm)

areagm_lm <- lm(leaf_area.mean ~ gm.mean, data=leaf_gm)
summary(areagm_lm)

xmin <-min(lfgm_ch$massarea) 
xmax <- max(lfgm_ch$massarea)

#individual plots
#gm v Cond
#sun leaves (no drought)
gmlma_sun <- lm(gm~ massarea, data=lfgm_ch, leaf=="sun")
summary(gmlma_sun)
#shade leaves (no drought)
gmlma_sha <- lm(gm~ massarea, data=lfgm_ch, leaf=="shade")
summary(gmlma_sha)

#means
plot(gm.mean~massarea.mean, data=leaf_gm,col=cols[temp], pch=16,subset=leaf == "sun",xlab=lmalab,
     ylab= "", ylim=c(0, .6), xlim=c(75, 175))
points(gm.mean~massarea.mean, data=leaf_gm,col=cols[temp], pch=1,subset=leaf == "shade")
title(ylab=gmlab, mgp=ypos)
ablineclip(gmlma_sun, lty=1, x1=xmin, x2=xmax)
ablineclip(gmlma_sha, lty=2, x1=xmin, x2=xmax)
legend("topright", leglab, pch=pch4, col=col4, inset = 0.03)

#by chamber
windows()
plot(gm~massarea, data=lfgm_ch,col=cols[temp], pch=16,subset=leaf == "sun",xlab=lmalab,
     ylab= "", ylim=c(0, 1), xlim=c(50, 200))
points(gm~massarea, data=lfgm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
title(ylab=gmlab, mgp=ypos)
ablineclip(gmlma_sun, lty=1, x1=xmin, x2=xmax)
ablineclip(gmlma_sha, lty=2, x1=xmin, x2=xmax)
legend("topright", leglab, pch=pch4, col=col4, inset = 0.03)
dev.copy2pdf(file="output/gm_lma.pdf")
dev.off()

windows()
plot(gm~leaf_area, data=lfgm_ch,col=cols[temp], pch=16,subset=leaf == "sun", xlab=arealab,
     ylab= "", ylim=c(0, 1), xlim=c(0, 100))
points(gm~leaf_area, data=lfgm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
title(ylab=gmlab, mgp=ypos)
#ablineclip(gmlma_sun, lty=1, x1=xmin, x2=xmax)
#ablineclip(gmlma_sha, lty=2, x1=xmin, x2=xmax)
legend("topright", leglab, pch=pch4, col=col4, inset = 0.03)
dev.copy2pdf(file="output/gm_larea.pdf")
dev.off()

windows()
plot(gm~leaf_mass, data=lfgm_ch,col=cols[temp], pch=16,subset=leaf == "sun",xlab="Leaf Mass  (g)",
     ylab= "", ylim=c(0, 1), xlim=c(0, 1))
points(gm~leaf_mass, data=lfgm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
title(ylab=gmlab, mgp=ypos)
#ablineclip(gmlma_sun, lty=1, x1=xmin, x2=xmax)
#ablineclip(gmlma_sha, lty=2, x1=xmin, x2=xmax)
legend("topright", leglab, pch=pch4, col=col4, inset = 0.03)
dev.copy2pdf(file="output/gm_lmass.pdf")
dev.off()








windows(8,12)
par(mfrow=c(3,1),  omi=c(1,0,0.1,0.1), mar=c(0,7,0,0), cex.axis=1.2, cex.lab=1.2)   

# First Pane
plot(massarea.mean ~ gm.mean, data=leaf_gm,col=cols[temp], pch=16,subset=leaf == "sun", xlab="", ylab= "", axes=FALSE, 
     xlim=c(0, .6), ylim=c(0, 200))
  axis(1, labels = FALSE)
  axis(2, labels=TRUE)
  title(ylab=lmalab, mgp=ypos)
points(massarea.mean ~ gm.mean, data=leaf_gm,col=cols[temp], pch=1,subset=leaf == "shade")
  ablineclip(lmagm_lm, lty=2, x1=xmin, x2=xmax)
  legend("topright", leglab, pch=pch4, col=col4, inset = 0.03)
  box()

plot(leaf_mass.mean ~ gm.mean, data=leaf_gm,col=cols[temp], pch=16,subset=leaf == "sun", xlab="", axes=FALSE,
     xlim=c(0, .6), ylim=c(0, 1), ylab= "")
  axis(1, labels = FALSE)
  axis(2, labels=TRUE)
  title(ylab=masslab, mgp=ypos)
points(leaf_mass.mean ~ gm.mean, data=leaf_gm,col=cols[temp], pch=1,subset=leaf == "shade")
  ablineclip(massgm_lm, lty=2, x1=xmin, x2=xmax)
  box()

plot(leaf_area.mean ~ gm.mean, data=leaf_gm,col=cols[temp], pch=16,subset=leaf == "sun", 
     xlab="", ylab= "", xlim=c(0, .6), ylim=c(0, 75))
  axis(1, labels = TRUE)
  axis(2, labels=TRUE)
title(ylab=arealab, mgp=ypos)
points(leaf_area.mean ~ gm.mean, data=leaf_gm,col=cols[temp], pch=1,subset=leaf == "shade")
  ablineclip(areagm_lm, lty=2, x1=xmin, x2=xmax)
mtext(gmlab, side=1, outer=TRUE, line=3.5, cex=1)
  box()

dev.copy2pdf(file="output/gm_lma.pdf")
dev.off()




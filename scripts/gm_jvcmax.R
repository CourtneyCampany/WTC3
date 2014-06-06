source("scripts/plot_objects.R")


library(plantecophys)

#read in jmax vcmax
jvc<- read.csv("calculated data/tdl_aci.csv")
jvc_agg <- summaryBy(Vcmax+Jmax~leaf+temp, data=jvc, FUN=c(mean))

jvc_agg[,]

#simulate ACi curves for each leaf, ambient and elevated T
sunAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc_agg[3,3], Jmax=jvc_agg[3,4])
sunET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc_agg[4,3], Jmax=jvc_agg[4,4])
shaAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc_agg[1,3], Jmax=jvc_agg[1,4])
shaET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc_agg[2,3], Jmax=jvc_agg[2,4])


png(filename = "output/presentations/Aci.png", width = 11, height = 8.5, units = "in", res= 400)
plot(sunAT_sim$Ci, sunAT_sim$ALEAF, col="forestgreen", pch=21,  cex=1.1,xlab=cilab, ylab="", type="l", lwd=2)
  points(sunET_sim$Ci, sunET_sim$ALEAF, col="forestgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
  points(shaAT_sim$Ci, shaAT_sim$ALEAF, col="yellowgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2)
  points(shaET_sim$Ci, shaET_sim$ALEAF, col="yellowgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
legend("topleft", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03)
title(ylab=satlab, mgp=ypos)
#dev.copy2pdf(file="output/Aci.pdf")
dev.off()

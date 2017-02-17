# source("functions and packages/packages.R")
# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")

#read in parameters from aci curves with gm included
coefs <- read.csv("calculated_data/aci_gm_param.csv")

#####redo aci curves with gmes 
gm <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <-summaryBy(gm ~ leaflight+temp, data=gm, FUN=mean, keep.names=TRUE)


####model ACC with above parameters 

ci_seq <- seq(50, 2000, length=101)

acisunat<- Aci(ci_seq, Vcmax=coefs[3,3], Jmax=coefs[3,4], Rd=coefs[3,5], gmeso=gm_agg[5,3])
acisunet<- Aci(ci_seq, Vcmax=coefs[4,3], Jmax=coefs[4,4], Rd=coefs[4,5], gmeso=gm_agg[6,3])
acishaat<- Aci(ci_seq, Vcmax=coefs[1,3], Jmax=coefs[1,4], Rd=coefs[1,5], gmeso=gm_agg[3,3])
acishaet<- Aci(ci_seq, Vcmax=coefs[2,3], Jmax=coefs[2,4], Rd=coefs[2,5], gmeso=gm_agg[4,3])

acisunat$Cc <- with(acisunat, Ci - ALEAF / gm_agg[5,3])
acisunet$Cc <- with(acisunet, Ci - ALEAF / gm_agg[6,3])
acishaat$Cc <- with(acishaat, Ci - ALEAF / gm_agg[3,3])
acishaet$Cc <- with(acishaet, Ci - ALEAF / gm_agg[4,3])

###plot of model ACC curves
# windows(8,6)

par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(ALEAF~Cc, data=acisunat, pch=16, col=suncol2, type='l',lwd=3,ylab=photolab, xlab=cclab)
  points(ALEAF~Cc, data=acisunet,  col=suncol2,type='l',lwd=3, lty=2)
  points(ALEAF~Cc, data=acishaat,  col=lightscol2,type='l',lwd=3)
  points(ALEAF~Cc, data=acishaet,  col=lightscol2,type='l',lwd=3, lty=2)
  legend("topleft", c("Sun-AT", "Sun-ET", "Shade-AT", "Shade-ET"), lty=c(1,2, 1,2),lwd=2,
          col=c(suncol2, suncol2,lightscol2,lightscol2),inset = 0.01, bty='n',cex=.8)


###inset as in aci
par(fig=c(0.45, 0.95, 0.15,0.55), mar=c(2,2,0,0),new=T, cex=.7, las=1,  cex.axis=.7, cex.lab=.7, tcl=-.25)

plot(ALEAF~Cc ,data= acisunat, ylim=c(0, 15.5), xlim=c(45,250), xlab="", ylab="",xaxt="n", yaxt="n", pch="")
axis(2, mgp=c(3, .5, 0))
axis(1, mgp=c(3, 0, 0))

  points(ALEAF~Cc, col=suncol2, data=acisunat, type="l", lwd=2)
  points(ALEAF~Cc, col=suncol2, data=acisunet, type="l", lwd=2, lty=2)
  points(ALEAF~Cc, col=lightscol2, data=acishaat, type="l", lwd=2)
  points(ALEAF~Cc, col=lightscol2, data=acishaet, type="l", lwd=2, lty=2)  


# dev.copy2pdf(file="master_scripts/paper_figures/Acc_model.pdf")
# dev.off()
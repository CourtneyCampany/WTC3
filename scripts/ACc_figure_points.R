# source("functions and packages/packages.R")
# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")
library(plantecophys)

##read data sets----------------------------------------------------------------------------------------------------------------

sun <- read.csv("calculated_data/sunleafaci.csv")
  sun <- droplevels(subset(sun, chamber != "ch04"))
  
shade <- read.csv("calculated_data/shadeleafaci.csv")
  
meso <- read.csv("calculated_data/gm_means.csv")



# fit normal Aci curve (no gmeso) by leaf type and temp trt--------------------------------------------------------------------------
fit_sun_at <- fitacis(droplevels(sun[sun$temp == "ambient",]), "chamber")
fit_sun_et <- fitacis(droplevels(sun[sun$temp == "elevated",]), "chamber")

fit_shade_at <- fitacis(droplevels(shade[shade$temp == "ambient",]), "chamber")
fit_shade_et <- fitacis(droplevels(shade[shade$temp == "elevated",]), "chamber")

# simulate from normal Aci curve (use mean values by treatment)----------------------------------------------------------------------
ci_seq <- seq(50, 2000, length=101)

simaci_sun_at <- Aci(Ci=ci_seq, Vcmax=mean(coef(fit_sun_at)$Vcmax), 
                     Jmax=mean(coef(fit_sun_at)$Jmax), Rd=mean(coef(fit_sun_at)$Rd))

simaci_sun_et <- Aci(Ci=ci_seq, Vcmax=mean(coef(fit_sun_et)$Vcmax), 
                     Jmax=mean(coef(fit_sun_et)$Jmax), Rd=mean(coef(fit_sun_et)$Rd))

simaci_shade_at <- Aci(Ci=ci_seq, Vcmax=mean(coef(fit_shade_at)$Vcmax), 
                       Jmax=mean(coef(fit_shade_at)$Jmax), Rd=mean(coef(fit_shade_at)$Rd))

simaci_shade_et <- Aci(Ci=ci_seq, Vcmax=mean(coef(fit_shade_et)$Vcmax), 
                       Jmax=mean(coef(fit_shade_et)$Jmax), Rd=mean(coef(fit_shade_et)$Rd))

# calculate Cc for the simulated curves using treatment specific gmes----------------------------------------------------------------
simaci_sun_at$Cc <- with(simaci_sun_at, Ci - ALEAF / meso[5,3])
simaci_sun_et$Cc <- with(simaci_sun_at, Ci - ALEAF / meso[6,3])
simaci_shade_at$Cc <- with(simaci_sun_at, Ci - ALEAF / meso[1,3])
simaci_shade_et$Cc <- with(simaci_sun_at, Ci - ALEAF / meso[2,3])

# calculate Cc on measurements using treatment specific gmes-----------------------------------------------------------------------
sun_at <- sun[sun$temp == "ambient",]
sun_et <- sun[sun$temp == "elevated",]
shade_at <- shade[shade$temp == "ambient",]
shade_et <- shade[shade$temp == "elevated",]

sun_at$Cc <- with(sun_at, Ci - Photo/meso[5,3])  
sun_et$Cc <- with(sun_et, Ci - Photo/meso[6,3])  
shade_at$Cc <- with(shade_at, Ci - Photo/meso[1,3])  
shade_et$Cc <- with(shade_et, Ci - Photo/meso[2,3])  


###PLotting---------------------------------------------------------------------------------------------------------------

###plot of model ACC curves
 # windows(8,6)

par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(ALEAF~Cc, data=simaci_sun_at, col=suncol2, type='l',lwd=3,ylab=photolab, xlab=cclab, ylim=c(0,40))
  lines(ALEAF~Cc, data=simaci_sun_et,  col=suncol2,lwd=3, lty=2)
  lines(ALEAF~Cc, data=simaci_shade_at,  col=lightscol2,lwd=3)
  lines(ALEAF~Cc, data=simaci_shade_et,  col=lightscol2,lwd=3, lty=2)
  legend("topleft", c("Sun-AT", "Sun-ET", "Shade-AT", "Shade-ET"), lty=c(1,2, 1,2),lwd=2,
          col=c(suncol2, suncol2,lightscol2,lightscol2),inset = 0.01, bty='n',cex=.8)
  
###Add points from ACi curves, where Cc is predicted
  points(Photo~Cc ,data= sun_at, col=suncol50, pch=16)
  points(Photo~Cc ,data= sun_et, col=suncol50,  pch=17)
  points(Photo~Cc ,data= shade_at,  col=lights50col,  pch=16)
  points(Photo~Cc ,data= shade_et,  col=lights50col,  pch=17)


###initial curve inset
par(fig=c(0.425, 0.95, 0.125,0.475), mar=c(2,2,0,0),new=T, cex=.7, las=1,  cex.axis=.7, cex.lab=.7, tcl=-.25)

plot(ALEAF~Cc ,data= simaci_sun_at, ylim=c(0, 20), xlim=c(45,150), xlab="", ylab="",xaxt="n", yaxt="n", pch="")
axis(2, mgp=c(3, .5, 0))
axis(1, mgp=c(3, 0, 0))

  lines(ALEAF~Cc, col=suncol2, data=simaci_sun_at,  lwd=2)
  lines(ALEAF~Cc, col=suncol2, data=simaci_sun_et,  lwd=2, lty=2)
  lines(ALEAF~Cc, col=lightscol2, data=simaci_shade_at,  lwd=2)
  lines(ALEAF~Cc, col=lightscol2, data=simaci_shade_et,  lwd=2, lty=2)
  
  ###Add points from ACi curves, where Cc is predicted
  points(Photo~Cc ,data= sun_at, col=suncol50,  pch=16)
  points(Photo~Cc ,data= sun_et, col=suncol50,  pch=17)
  points(Photo~Cc ,data= shade_at,  col=lights50col,  pch=16)
  points(Photo~Cc ,data= shade_et,  col=lights50col,  pch=17)


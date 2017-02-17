library(plantecophys)
library(doBy)
library(plotrix)
library(scales)

source("functions and packages/functions.R")
source("functions/plot_objects.R")


####dataset for ACi curve predictions------------------------------------------------------------------------------

#read in gm data set (no drought) and CI from discrimination
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
Ci_bar <- read.csv("calculated_data/Ci_bar.csv")
  ##calculate CC
  gmes$Cc<- with(gmes, Ci-Photo/gm)
  gmes$cc_ci<- with(gmes, Ci/Cc)

  ##need to combine Ci_bar with CC and gm
  
  ###get average by id
  gm_agg <- summaryBy(Photo+Cond+Ci+Cc+gm+VpdL+xsi+DELTA+cc_ci+PARi ~ id+leaf +light+temp+leaflight, 
                    data=gmes, FUN=mean, keep.names=TRUE)

  ##remove shade-high
  gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
  gm_sunsha <- droplevels(gm_sunsha)
  gm_sunsha$id <- gsub("-high", "", gm_sunsha$id)
  gm_sunsha$id <- gsub("-low", "", gm_sunsha$id)

  ##merge
  gm_c13 <- merge(gm_sunsha, Ci_bar[, c(2,8)], by="id")

    
####use plant ecophys to predict aci curve--------------------------------------------------------------------------
  #### plotbits

  shade <- gm_c13[gm_c13$leaf=="shade",]
  sun <- gm_c13[gm_c13$leaf=="sun",]
  
  #read in jmax vcmax
  jvc<- read.csv("calculated_data/tdl_aci.csv")

  
  #simulate ACi curves for each leaf, ambient and elevated T
  sunAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[3,3], Jmax=jvc[3,4],PPFD=mean(sun$PARi))
  sunET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[4,3], Jmax=jvc[4,4], PPFD=mean(sun$PARi))
  shaAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[1,3], Jmax=jvc[1,4], PPFD=mean(shade$PARi))
  shaET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[2,3], Jmax=jvc[2,4],PPFD=mean(shade$PARi))
  
  ###test altering jmax and vcamx at 1800 and at shade light to get better git 
  shaAT_sim2 <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[1,3]*.7, Jmax=jvc[1,4]*.7, PPFD=mean(1800))
  shaET_sim2 <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[2,3]*.7, Jmax=jvc[2,4]*.7, PPFD=mean(1800))
  
  shaAT_sim3 <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[1,3]*1.75, Jmax=jvc[1,4]*1.75, PPFD=mean(shade$PARi))
  shaET_sim3 <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[2,3]*1.75, Jmax=jvc[2,4]*1.75,PPFD=mean(shade$PARi))
  
  
  ###testing aci fit to data---------------------------------------------------------------------------------------
  par(mar=c(5,5,2,2))
  plot(Photo~Ci, data=gm_c13, type='n', xlim=c(0, 1000), ylim=c(0, 30),xlab=cilab, ylab=satlab)
  points(Photo~Ci, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  
  points(shaAT_sim$Ci, shaAT_sim$ALEAF, col=shacol, pch=21,  cex=1.1, type="l", lwd=2)
  points(shaET_sim$Ci, shaET_sim$ALEAF, col=shacol, pch=21,  cex=1.1, type="l", lwd=2, lty=2)
  
  points(shaAT_sim2$Ci, shaAT_sim2$ALEAF, col="blue", pch=21,  cex=1.1, type="l", lwd=2)
  points(shaET_sim2$Ci, shaET_sim2$ALEAF, col="blue", pch=21,  cex=1.1, type="l", lwd=2, lty=2)
  
  points(shaAT_sim3$Ci, shaAT_sim3$ALEAF, col="orange", pch=21,  cex=1.1, type="l", lwd=2)
  points(shaET_sim3$Ci, shaET_sim3$ALEAF, col="orange", pch=21,  cex=1.1, type="l", lwd=2, lty=2)
  
  
  legend("topleft", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03)
  legend("bottomright", leaflab2, pch=16,inset = 0.03, col=leafcol) 

  
  
  ###aci curves at current light with saturating light parameters, plus gas exchange
  windows(10,8)
  par(mar=c(5,5,2,2))
  plot(sunAT_sim$Ci, sunAT_sim$ALEAF, col=suncol, pch=21,  cex=1.1,xlab=cilab, ylab=satlab, type="l", lwd=2)
  points(sunET_sim$Ci, sunET_sim$ALEAF, col=suncol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
  points(shaAT_sim$Ci, shaAT_sim$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2)
  points(shaET_sim$Ci, shaET_sim$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
  
  points(Photo~Ci, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  points(Photo~Ci, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25)
  
  legend("topleft", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03)
  legend("bottomright", leaflab2, pch=16,inset = 0.03, col=leafcol) 
  
  dev.copy2pdf(file="master_scripts/figures/aci_predict.pdf")
  dev.off()

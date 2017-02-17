source("functions and packages/packages.R")
source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")

treatments <- read.csv("raw data/temp_trt.csv") 

###Photosythesis data-----------------------------------------------------------------------------------------------
photo <- read.csv("calculated_data/gmes_wellwatered.csv")
###get average by id
photo_agg <- summaryBy(Photo ~ chamber+id+leaf +light+temp+leaflight+Month, data=photo, FUN=mean, keep.names=TRUE)
photo2 <- summaryBy(Photo~ chamber+leaf+temp+leaflight+Month, data=photo_agg, FUN=mean, keep.names=TRUE)

###data for aci curves----------------------------------------------------------------------------------------------  
  
  #read ACi datasets 
acishade <- read.csv("raw data/shadeaci.csv")
acishade <- merge(acishade, treatments)
  
  #clean from previous script
  acishade_clean <- acishade[acishade$chamber != "ch02",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch07",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch09",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch11",]

  shade_redo <- read.csv("raw data/shadeaci_redo.csv")
  shade_redo <- merge(shade_redo, treatments)
  
  sunaci <- read.csv("raw data/sunaci.csv")
  sunaci <-merge(sunaci, treatments)
    
  #clean from previous script
  sunaci_clean <- sunaci[sunaci$chamber != "ch06", ]
  sunaci_clean2 <- sunaci_clean[sunaci_clean$chamber != "ch04", ]
  
  tdlaci2 <- read.csv("raw data/tdlaci2.csv")
  tdlaci2 <-merge(tdlaci2, treatments)
  
  #read in jmax vcmax
  jvc<- read.csv("calculated_data/tdl_aci.csv")
  
  #simulate ACi curves at 1800 par
  sunAT_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[3,3], Jmax=jvc[3,4],PPFD=1800)
  sunET_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[4,3], Jmax=jvc[4,4], PPFD=1800)
  shaAT_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[1,3], Jmax=jvc[1,4], PPFD=1800)
  shaET_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[2,3], Jmax=jvc[2,4],PPFD=1800)
  

###Narea data------------------------------------------------------------------------------------------------------
leaf_chem <- read.csv("raw data/leaf_chem.csv")
leaf_mass <- read.csv("raw data/leaf_data.csv")
  
#get vcmax per chamber 
aciparam <- read.csv("calculated_data/aciparameters.csv")
  
#calculate nitrogen content in leaves
canopy_chem <- merge(leaf_mass[leaf_mass$wp_type=="mid",c(1, 3:4, 7:8)], leaf_chem[, 4:8], by=c("Month", "chamber", "leaf"))
  #add treatments  
  canopy_chem <- addtrt_func(canopy_chem)
  canopy_chem$leafN <- with(canopy_chem, leaf_mass *(n_perc/100))
  canopy_chem$lma <- with(canopy_chem, (leaf_mass/leaf_area)*10000) #g/m2
  canopy_chem$leafN_area <- with(canopy_chem, lma *(n_perc/100))
  canopy_chem<- add_campaign(canopy_chem)
  
  canopy_chem2 <- canopy_chem[canopy_chem$drydown=="control",]
  canopy_chem3 <- canopy_chem2[, c("chamber", "Month", "leaf", "leafN_area")]
  
##vcmax and Narea dataset  (with stats for results)----------------------------------------------------------------
Nagg <- summaryBy(leafN_area ~ chamber + leaf, data=canopy_chem)
  N_aci <- merge(aciparam, Nagg, by= c("chamber", "leaf"))

# library(nlme)
# library(lme4)
# library(lmerTest)
# library(LMERConvenienceFunctions)
nitrovc_mod <- lm(Vcmax~leafN_area.mean, data=N_aci)
# nitrovc_mod2 <- lme(Vcmax~leafN_area.mean, random=~1|chamber,data=N_aci)
# nitrovc_mod3 <- lmer(Vcmax~leafN_area.mean+ (1|chamber),data=N_aci)
# anova(nitrovc_mod3)
# summary(nitrovc_mod3)
# mcp.fnc(nitrovc_mod3)
# source("functions and packages/r2glmm.R")
# rsquared.glmm(nitrovc_mod3)
# 
# summary(nitrovc_mod)
# summary(nitrovc_mod2)
# anova(nitrovc_mod,nitrovc_mod3)

####photosynthesis and narea dataset (with stats)--------------------------------------------------------------------
###merge photo with leaf N datasets

Anitro <- merge(photo2, canopy_chem3, by=c("chamber", "Month", "leaf"))  

photoN_mod <- lm(Photo~leafN_area, data=Anitro[Anitro$leaflight != "shade-high",])
# photoN_mod2 <- lme(Photo~leafN_area, random=~1|chamber,data=Anitro[Anitro$leaflight != "shade-high",])
# photoN_mod3<- lmer(Photo~leafN_area+ (1|chamber), data=Anitro[Anitro$leaflight != "shade-high",])
# anova(photoN_mod3)
# summary(photoN_mod3)
# mcp.fnc(photoN_mod3)
# rsquared.glmm(photoN_mod3)
# 
# summary(photoN_mod)
# summary(photoN_mod2)


###plotting
palette(c(shacol, suncol))
  
# windows(8,10)

par(fig=c(0, 1, .5, 1),mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
  
#panel 1: aci curves with inset
plot(Photo~Ci ,data= acishade_clean, col=lightscol, ylim=c(0, 42), xlim=c(0,2000), xlab=cilab, 
       ylab=photolab, pch=c(16, 17)[pch=acishade_clean$temp])
  points(Photo~Ci ,data= shade_redo, col=lightscol,  pch=c(16, 17)[pch=shade_redo$temp])
  points(Photo~Ci ,data= sunaci_clean2,  col=suncol,  pch=c(16, 17)[pch=sunaci_clean2$temp])
  points(Photo~Ci ,data= tdlaci2,  col=suncol,  pch=c(16, 17)[pch=tdlaci2$temp])

  points(sunAT_sim2$Ci, sunAT_sim2$ALEAF, col=suncol2, cex=1.1,xlab=cilab, ylab="", type="l", lwd=2)
  points(sunET_sim2$Ci, sunET_sim2$ALEAF, col=suncol2, cex=1.1,xlab="", ylab="", type="l", lwd=2, lty=2)
  points(shaAT_sim2$Ci, shaAT_sim2$ALEAF, col=lightscol2, cex=1.1,xlab="", ylab="", type="l", lwd=2)
  points(shaET_sim2$Ci, shaET_sim2$ALEAF, col=lightscol2, cex=1.1,xlab="", ylab="", type="l", lwd=2, lty=2)
  
  legend("topleft", c("Sun", "Shade-High Light", "AT", "ET"), pch=c(-1,-1, 16,17), 
         lty=c(1,1, 1,2), lwd=2,
         col=c(suncol, lightscol2, "black", "black"),inset = 0.01, bty='n',cex=.8)
  text(x=2020, y=42, "(a)", cex=1)
  
###panel 2: leafNarea ~ vcmax (bottom left)
par(fig=c(0, 1, 0, .5),new=T,mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))

plot(Vcmax~leafN_area.mean, data=N_aci, col=as.factor(leaf),  ylim=c(40, 133),xlim=c(1.5,3.5),
       pch=c(16, 17)[pch=N_aci$temp],xlab=narealab, ylab=vclab)
  ablineclip(nitrovc_mod, x1=min(N_aci$leafN_area.mean), x2=max(N_aci$leafN_area.mean), lwd=2, lty=3)
  legend("topleft", leglab2, pch=c(16,17,16,17), col=trtcols,inset = 0.01, bty='n',cex=.7)
  text(x=3.525, y=133, "(b)", cex=1)
  
###inset figure  
par(fig=c(0.525, 0.95, 0.58,0.75), mar=c(2,2,0,0),new=T, cex=.7, las=1,  cex.axis=.7, cex.lab=.8, tcl=-.25)
  
plot(Photo~Ci ,data= acishade_clean, ylim=c(0, 15.5), xlim=c(45,300), xlab="", ylab="",xaxt="n", yaxt="n", pch="")
  axis(2, mgp=c(3, .5, 0))
  axis(1, mgp=c(3, 0, 0))
  
  points(Photo~Ci ,data= acishade_clean, col=lightscol,pch=c(16, 17)[pch=acishade_clean$temp])
  points(Photo~Ci ,data= shade_redo,col=lightscol, pch=c(16, 17)[pch=shade_redo$temp])
  points(Photo~Ci ,data= sunaci_clean2, col=suncol, pch=c(16, 17)[pch=sunaci_clean2$temp])
  points(Photo~Ci ,data= tdlaci2, col=suncol, pch=c(16, 17)[pch=tdlaci2$temp])
  
  points(sunAT_sim2$Ci, sunAT_sim2$ALEAF, col=suncol2,  type="l", lwd=2)
  points(sunET_sim2$Ci, sunET_sim2$ALEAF, col=suncol2,  type="l", lwd=2, lty=2)
  points(shaAT_sim2$Ci, shaAT_sim2$ALEAF, col=lightscol2,  type="l", lwd=2)
  points(shaET_sim2$Ci, shaET_sim2$ALEAF, col=lightscol2,  type="l", lwd=2, lty=2)  

  
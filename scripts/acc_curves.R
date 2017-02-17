source("functions and packages/packages.R")
source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")

treatments <- read.csv("raw data/temp_trt.csv")

#####redo aci curves with gmes 
gm <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <-summaryBy(gm ~ leaflight+temp, data=gm, FUN=mean, keep.names=TRUE)

#read ACi datasets 
acishade <- read.csv("raw data/shadeaci.csv")
  acishade <- merge(acishade, treatments)
  
  acishade_at <- acishade[acishade$temp =="ambient",]
    acishade_at <- droplevels(acishade_at)
  acishade_et <- acishade[acishade$temp =="elevated",]
    acishade_et <- droplevels(acishade_et)
  
shade_redo <- read.csv("raw data/shadeaci_redo.csv")
  shade_redo <- merge(shade_redo, treatments) 
  
  shade_redo_at <- shade_redo[shade_redo$temp =="ambient",]
    shade_redo_at <- droplevels(shade_redo_at)
  shade_redo_et <- shade_redo[shade_redo$temp =="elevated",]
    shade_redo_et <- droplevels(shade_redo_et)
  
sunaci <- read.csv("raw data/sunaci.csv")
  sunaci <- merge(sunaci, treatments)
  
  sunaci_at <- sunaci[sunaci$temp =="ambient",]
    sunaci_at <- droplevels(sunaci_at)
  sunaci_et <- sunaci[sunaci$temp =="elevated",]
    sunaci_et <- droplevels(sunaci_et)
  
tdlaci2 <- read.csv("raw data/tdlaci2.csv")
  tdlaci2 <- merge(tdlaci2, treatments) 
  tdlaci2 <- droplevels(tdlaci2)

###fit curves using gmes data by treatment-------------------------------------------------------------------------------------

####1: shade leaves with AT
shadefit_at<-fitacis(acishade_at, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi", 
                                                          gmeso=gm_agg[4,3]), Tcorrect=TRUE)
shade_at_coef <- coef(shadefit_at)


####2: shade leaves with ET
shadefit_et<-fitacis(acishade_et, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi", 
                     gmeso=gm_agg[4,4]), Tcorrect=TRUE)
shade_et_coef <- coef(shadefit_et)


####3: shade redos with AT
shaderedo_at_fit<-fitacis(shade_redo_at, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi", 
                       gmeso=gm_agg[4,3]), Tcorrect=TRUE)
shaderedo_at_coef <- coef(shaderedo_at_fit)

####4: shade redos with eT
shaderedo_et_fit<-fitacis(shade_redo_et, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi", 
                       gmeso=gm_agg[4,4]), Tcorrect=TRUE)
shaderedo_et_coef <- coef(shaderedo_et_fit)

####5: sun leaves with at
sun_at_fit<-fitacis(sunaci_at, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi", 
                    gmeso=gm_agg[5,3]), Tcorrect=TRUE)
sun_at_coef <- coef(sun_at_fit)

####6: sun leaves with et
sun_et_fit<-fitacis(sunaci_et, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi", 
                    gmeso=gm_agg[6,3]), Tcorrect=TRUE)
sun_et_coef <- coef(sun_et_fit)

####7: sun redos with at
sun_redo<-fitacis(tdlaci2, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi", 
                  gmeso=gm_agg[5,3]), Tcorrect=TRUE)
sun_redo_coef <- coef(sun_redo)

####merge all aci curve coefs into one dataframe----------------------------------------------------------------------------

#shade
shade_coefs <- rbind(shade_at_coef,shade_et_coef)
shade_coefs2 <-  rbind(subset(shade_coefs, !(shade_coefs$chamber %in% c("ch02","ch07","ch09","ch11"))), 
                       shaderedo_at_coef, shaderedo_et_coef)

shade_coefs2$leaf <- "shade"

##sun
sun_coefs <- rbind(sun_at_coef,sun_et_coef[sun_et_coef$chamber != "ch04",],sun_redo_coef)
sun_coefs$leaf <- "sun"

###full dataframe with aci coefs by leaf type and chamber with temp trt gmes

#write a df with coefs for sun and shade leaves
wtc_aci <- rbind(sun_coefs, shade_coefs2)
wtc_aci <- merge(wtc_aci, treatments, by = "chamber")

coefs <- summaryBy(Vcmax+Jmax+Rd~ leaf+temp, data=wtc_aci, FUN=mean, keep.names = TRUE)

write.csv(coefs, "calculated_data/aci_gm_param.csv", row.names=FALSE)

####model ACC with above parameters 

ci_seq <- seq(50, 1000, length=101)

acisunat<- Aci(ci_seq, Vcmax=coefs[3,3], Jmax=coefs[3,4], Rd=coefs[3,5], gmeso=gm_agg[5,3])
acisunet<- Aci(ci_seq, Vcmax=coefs[4,3], Jmax=coefs[4,4], Rd=coefs[4,5], gmeso=gm_agg[6,3])
acishaat<- Aci(ci_seq, Vcmax=coefs[1,3], Jmax=coefs[1,4], Rd=coefs[1,5], gmeso=gm_agg[3,3])
acishaet<- Aci(ci_seq, Vcmax=coefs[2,3], Jmax=coefs[2,4], Rd=coefs[2,5], gmeso=gm_agg[4,3])

acisunat$Cc <- with(acisunat, Ci - ALEAF / gm_agg[5,3])
acisunet$Cc <- with(acisunet, Ci - ALEAF / gm_agg[6,3])
acishaat$Cc <- with(acishaat, Ci - ALEAF / gm_agg[3,3])
acishaet$Cc <- with(acishaet, Ci - ALEAF / gm_agg[4,3])

###plot of model ACC curves
windows(8,6)
#png(filename = "figpic/acc.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(ALEAF~Cc, data=acisunat, pch=16, col=suncol2, type='l',lwd=3,ylab=satlab, xlab=cclab)
  points(ALEAF~Cc, data=acisunet, pch=17, col=suncol2,type='l',lwd=3, lty=2)
  points(ALEAF~Cc, data=acishaat, pch=16, col=lightscol2,type='l',lwd=3)
  points(ALEAF~Cc, data=acishaet, pch=16, col=lightscol2,type='l',lwd=3, lty=2)
  legend("topleft", c("Sun-AT", "Sun-ET", "Shade-AT", "Shade-ET"), lty=c(1,2, 1,2),lwd=2,
         col=c(suncol2, suncol2,lightscol2,lightscol2),inset = 0.01, bty='n',cex=.8)

dev.copy2pdf(file="master_scripts/paper_figures/Acc_model.pdf")
dev.off()




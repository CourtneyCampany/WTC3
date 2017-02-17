source("functions and packages/functions.R")
library(doBy)
#####redo aci curves with gmes to get new Jmax and Vcmax

#read in gmes, get simple means by treatment
gm <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <-summaryBy(gm ~ leaflight+temp, data=gm, FUN=mean, keep.names=TRUE)
# write.csv(gm_agg,"gm_means.csv", row.names = FALSE)

#read in temperature treatments
treatments <- read.csv("raw data/temp_trt.csv") 


##shade leaves-------------------------------------------------------------------------------------------------------------
acishade <- read.csv("raw data/shadeaci.csv")
  acishade <- merge(acishade, treatments)
  
shade_redo <- read.csv("raw data/shadeaci_redo.csv")
  shade_redo <- merge(shade_redo, treatments)

#clean from previous script
acishade_clean <- acishade[!(acishade$chamber %in% c("ch02","ch07","ch09","ch11")),]
acishade_clean$leaf <- "shade"


###make clean shade dfr and write
# shade_aci <- rbind(shade_redo, acishade_clean)
# write.csv(shade_aci,"shadeleafaci.csv", row.names = FALSE)


##have to seperates temperature treatment so datasets
acishade_clean_at <- acishade_clean[acishade_clean$temp == "ambient",]
  acishade_clean_at <-droplevels(acishade_clean_at)

acishade_clean_et <- acishade_clean[acishade_clean$temp == "elevated",]
  acishade_clean_et <- droplevels(acishade_clean_et)

shade_redo_at <- shade_redo[shade_redo$temp == "ambient",]
  shade_redo_at <- droplevels(shade_redo_at)

shade_redo_et <- shade_redo[shade_redo$temp == "elevated",]
  shade_redo_et <- droplevels(shade_redo_et)


##sunleaves-------------------------------------------------------------------------------------------------------------------
sunaci <- read.csv("raw data/sunaci.csv")
  sunaci <-merge(sunaci, treatments)

#clean from previous script  (lets see if i need this yet)
# sunaci_clean <- sunaci[sunaci$chamber != "ch06", ]
#   sunaci_clean2 <- sunaci_clean[sunaci_clean$chamber != "ch04", ]
  
sunaci_at <- sunaci[sunaci$temp == "ambient",]
  sunaci_at <- droplevels(sunaci_at)

sunaci_et <- sunaci[sunaci$temp == "elevated",]
  sunaci_et <- droplevels(sunaci_et)

  
##this dataframe is only at treatments  
tdlaci2 <- read.csv("raw data/tdlaci2.csv")
  tdlaci2 <-merge(tdlaci2, treatments)
  tdlaci2 <- droplevels(tdlaci2)


# sun_aci <- rbind(sunaci, tdlaci2)  
# write.csv(sun_aci,"sunleafaci.csv", row.names = FALSE)
  
library(plantecophys)

##fitaci with gmes for sun and shade leaves by temperature treatment----------------------------------------------------------

# Photosynthetic parameters for E. globulus (taken from gmes_functions.R; not documented there!)
  
# Do not need Rd; this is estimated (badly) from the A-Ci curves    
# k25r=0.728, Ea_r = 72.311
#    x$Rd <- k25r * exp(Ea_r*((x$Tleaf+273.15)-298)/(298*Rgc*(x$Tleaf+273.15)))
    
# Estimate GammaStar at 25 degrees, as Tleaf was controlled for 25C.
Rgc <- 8.314472
Ea_g <- 20.437
k25g <- 38.89
Gstar <- k25g * exp(Ea_g*((25 + 273.15)-298)/(298*Rgc*(25 + 273.15)))
    
  
#shade leaves
fitacishade_at<-fitacis(acishade_clean_at, "chamber", varnames = list(ALEAF="Photo", 
                Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), GammaStar=Gstar)

fitacishade_et<-fitacis(acishade_clean_et, "chamber", varnames = list(ALEAF="Photo", 
                Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), GammaStar=Gstar)

fitacishade_redo_at <- fitacis(shade_redo_at, "chamber", varnames = list(ALEAF="Photo", 
                      Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), GammaStar=Gstar)

fitacishade_redo_et <- fitacis(shade_redo_et, "chamber", varnames = list(ALEAF="Photo", 
                      Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), GammaStar=Gstar)

#sun leaves
fitacitdlaci2 <- fitacis(tdlaci2, "chamber", varnames = list(ALEAF="Photo", 
                             Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), GammaStar=Gstar)

fitacisunaci_clean2_at <- fitacis(sunaci_at, "chamber", varnames = list(ALEAF="Photo", 
                                  Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), GammaStar=Gstar)

fitacisunaci_clean2_et <- fitacis(sunaci_et, "chamber", varnames = list(ALEAF="Photo", 
                                  Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), GammaStar=Gstar)


#extract jmax and vcmax (compare to standard aci curves)
shade_at_coef <- coef(fitacishade_at)
shade_et_coef <- coef(fitacishade_et)
shade_redo_at_coef <- coef(fitacishade_redo_at)
shade_redo_et_coef <- coef(fitacishade_redo_et)

tdlaci2_coef <- coef(fitacitdlaci2)
sunaci_clean2_at_coef <- coef(fitacisunaci_clean2_at)
sunaci_clean2_et_coef <- coef(fitacisunaci_clean2_et)

#generate treatment means for vcmax and jmax

#merge shade, add leaf type designation
jmaxvcmax_gmes_sha <- rbind(shade_at_coef, shade_et_coef)
jmaxvcmax_gmes_sha <- rbind(jmaxvcmax_gmes_sha, shade_redo_at_coef)
jmaxvcmax_gmes_sha <- rbind(jmaxvcmax_gmes_sha, shade_redo_et_coef)
jmaxvcmax_gmes_sha$leaf <- "shade"


jmaxvcmax_gmes_sun <- rbind(tdlaci2_coef, sunaci_clean2_at_coef)
jmaxvcmax_gmes_sun <- rbind(jmaxvcmax_gmes_sun, sunaci_clean2_et_coef)
jmaxvcmax_gmes_sun$leaf <- "sun"

jmaxvcmax_gmes <- rbind(jmaxvcmax_gmes_sun, jmaxvcmax_gmes_sha)


#order chambers
chamberorder<-order(jmaxvcmax_gmes$chamber, by=jmaxvcmax_gmes$Vcmax)
jmaxvcmax_gmes <- jmaxvcmax_gmes[chamberorder,]
jmaxvcmax_gmes <- merge(jmaxvcmax_gmes, treatments, by = "chamber")

write.csv(jmaxvcmax_gmes, file = "calculated_data/aciparameters_gm.csv", row.names=FALSE)   


###before means/stats ch4 sun must be thrown out due to bad curve fitting
jmaxvcmax_clean <- jmaxvcmax_gmes[!(jmaxvcmax_gmes$chamber == "ch04" & jmaxvcmax_gmes$leaf == "sun"),]
  
library(doBy)
aci_means <- summaryBy(Vcmax+Jmax ~ temp+leaf , data = jmaxvcmax_gmes,  FUN=c(mean,se))
aci_means2 <- summaryBy(Vcmax+Jmax ~ leaf , data = jmaxvcmax_gmes,  FUN=c(mean,se))

##compare these to infinite gm parameters
 # aci_nogm <- read.csv("calculated_data/aci_sunsha.csv")

#write to csv
write.csv(jmaxvcmax_clean, file = "calculated_data/jmax_vcmax_gmes.csv", row.names=FALSE) 


##run some stats on jmax, vcmax----------------------------------------------------------------------------------------


##can use these but confirm whether curves 4,6 from sun are bad

jmaxvcmax_clean$tukeyid <- as.factor(paste(jmaxvcmax_clean$leaf, jmaxvcmax_clean$temp, sep="-"))
library(visreg)
library(multcomp)

J_temp <- lme(Jmax ~ temp ,random=~1|chamber, data=jmaxvcmax_clean) ##no warming effect
summary(J_temp)
anova(J_temp)
visreg(J_temp)

##full model

J_leaf <- lme(Jmax~ tukeyid, random=~1|chamber, data=jmaxvcmax_clean)
summary(J_leaf)
anova(J_leaf)
visreg(J_leaf)

tukey_Jmax<- glht(J_leaf, linfct = mcp(tukeyid = "Tukey"))
Jmax_siglets<- cld(tukey_Jmax)
Jmax_siglets2 <- Jmax_siglets$mcletters$Letters

write.csv(Jmax_siglets2, "master_scripts/sigletters/slr_jmaxgm.csv", row.names=FALSE)


#2: Vcmax: no effect of ET

vc_temp <- lme(Vcmax ~ temp ,random=~1|chamber, data=jmaxvcmax_clean)
summary(vc_temp)
anova(vc_temp)
visreg(vc_temp)

##full model

vc_leaf <- lme(Vcmax~ tukeyid, random=~1|chamber, data=jmaxvcmax_clean)
summary(vc_leaf)
anova(vc_leaf)
visreg(vc_leaf)

tukey_vcmax<- glht(vc_leaf, linfct = mcp(tukeyid = "Tukey"))
vcmax_siglets<- cld(tukey_vcmax)
vcmax_siglets2 <- vcmax_siglets$mcletters$Letters

write.csv(vcmax_siglets2, "master_scripts/sigletters/slr_vcmaxgm.csv", row.names=FALSE)


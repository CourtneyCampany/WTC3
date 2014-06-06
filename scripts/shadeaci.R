#generate jcmax and vcmax for sun and shade leaves
#need to add in sun leaves and redo some shade leaves


library(plantecophys)
library(doBy)

source("functions.R")


#read shade ACi datasets 
acishade <- read.csv("raw data/shadeaci.csv")
tdlaci <- read.csv("raw data/tdlaci.csv")

tdlaci2 <- read.csv("raw data/tdlaci2.csv")

#read plot summary
plotsumm <- read.csv("raw data/HFE3 chamber treatments.csv")

#acishade1 = subset(acishade, select = c("chamber", "Photo", "Tleaf", "Ci", "PARi"))

fitaci<-fitacis(acishade, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), 
                 Tcorrect=TRUE)

shade_coef <- coef(fitaci)
plot(fitaci, how="manyplots")

#second data set, had redos so subset by sun or shade

sunleaves <- subset(tdlaci, leaf=="sun")
shadeleaves <- subset(tdlaci, leaf =="shade")

fitsun<-fitacis(sunleaves, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), 
                  Tcorrect=TRUE)

fitshade<-fitacis(shadeleaves, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), 
                  Tcorrect=TRUE)


aci_coef <- coef(fitsun)
aci_coef <- coef(fitshade)

plot(fitsun, how="manyplots")
plot(fitshade, how="manyplots")

#third data set, had two redos
fitaci3<-fitacis(tdlaci2, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), 
                Tcorrect=TRUE)

shade_coef <- coef(fitaci3)
plot(fitaci3, how="manyplots")


####pick best ones, including redos and write a df with coefs for sun and shade leaves


#generate treatment means for vcmax and jmax
shade_coef <- merge(shade_coef, plotsumm, by = "chamber")
acishade_means <- summaryBy(Vcmax+Jmax ~ temp_treatment , data = shade_coef,  FUN=c(mean,se))
 


#write to csv
write.csv(jmax_vcmax, file = "calculated data/jmax_vcmax.csv", row.names=FALSE)   



#test
temp <- subset(acishade, chamber== "5")

fittemp <- fitaci(temp, varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), 
                  quiet=FALSE, Tcorrect=TRUE)


#look at plot, summary of the fit, extract the values, extract fitted values,
# look at the non linear regression fit, and plot modelled vs measured
plot(fittemp)
summary(fittemp)
coef(fittemp)
fitted(fittemp)
summary(fittemp$nlsfit)
with(fittemp$df, plot(Amodel, Ameas))
abline(0,1)
#use estimated parameters to estimate PS (useful later on)
fittemp$Photosyn(Ci=285)

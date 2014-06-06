source("functions and packages/functions.R")
library(plantecophys)
library(doBy)

#generate jcmax and vcmax for sun and shade leaves

#read ACi datasets 
acishade <- read.csv("raw data/shadeaci.csv")
shade_redo <- read.csv("raw data/shadeaci_redo.csv")
sunaci <- read.csv("raw data/sunaci.csv")
tdlaci2 <- read.csv("raw data/tdlaci2.csv")
#read plot summary
plotsumm <- read.csv("raw data/chamber_trt.csv")


#fit curves for shade leaves
shadefit<-fitacis(acishade, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), 
                 Tcorrect=TRUE)
shade_coef <- coef(shadefit)
plot(shadefit, how="manyplots")

#run shade redos
shaderedo_fit<-fitacis(shade_redo, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), 
                 Tcorrect=TRUE)
sha_redo_coef <- coef(shaderedo_fit)
plot(shaderedo_fit, how="manyplots")

#merge shade_coef with redos, add leaf designation
shade_coefs <- rbind(subset(shade_coef, !(shade_coef$chamber %in% c("ch02","ch07","ch09","ch11"))), sha_redo_coef)
shade_coefs$leaf <- "shade"
#order chambers
chamberorder1<-order(shade_coefs$chamber, by=shade_coefs$Vcmax)
shade_coefs <- shade_coefs[chamberorder1,]


####now complete all sun leaves
sun_fit<-fitacis(sunaci, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), 
                  Tcorrect=TRUE)

sun_coef <- coef(sun_fit)
plot(sun_fit, how="manyplots")


#third data set, had two redos
sun_redo<-fitacis(tdlaci2, "chamber", varnames = list(ALEAF="Photo", Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), 
                Tcorrect=TRUE)

sun_redo_coef <- coef(sun_redo)
plot(sun_redo, how="manyplots")
#need to merge sun redo with sun (replace 6 and 4 in sun redo with sun dfr)

sun_coefs <- rbind(subset(sun_coef, chamber != c(6,4)), sun_redo_coef)
sun_coefs$chamber <- gsub("r", "", sun_coefs$chamber)
sun_coefs$leaf <- "sun"
#order chambers
chamberorder<-order(sun_coefs$chamber, by=sun_coefs$Vcmax)
sun_coefs <- sun_coefs[chamberorder,]

#write a df with coefs for sun and shade leaves
tdlaci <- rbind(sun_coefs, shade_coefs)
tdlaci <- merge(tdlaci, plotsumm, by = "chamber")
write.csv(tdlaci, "calculated data/tdl_aci.csv", row.names=FALSE)


#generate treatment means for vcmax and jmax
aci_means <- summaryBy(Vcmax+Jmax ~ leaf+temp , data = tdlaci,  FUN=c(mean,se))
bar(Vcmax, c(temp, leaf), tdlaci)
bar(Jmax, c(temp, leaf), tdlaci)



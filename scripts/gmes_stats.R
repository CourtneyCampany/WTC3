source("functions and packages/functions.R")
source("functions and packages/packages.R")
source("scripts/read_data.R")

#gmes paired comparisons
gm_pairs <- gmformat(gm)

#3 dfr by leaf type
gm_sun <- subset(gm_pairs, ID == "sun-high")
gm_shade <- subset(gm_pairs, ID == "shade-low")
gm_shadehi <- subset(gm_pairs, ID == "shade-high")

#run a  test on one paired comparison (one chamber set of amb an elev leaves at growing temperature)
amb <- subset(gm_sun, month =="dec" & id =="bb" & temp =="ambient")
elev <- subset(gm_sun, month =="dec" & id =="bb" & temp =="elevated")

sampleTT <- t.test(amb$gm, elev$gm)
summary(sampleTT)




getp(sampleTT )
getdiffmean(sampleTT)


amb <- subset(gm_sun[c("temp","pair","gm")],temp=="ambient")
#names(amb)[3] <- "amb.gm"
elev <- subset(gm_sun[c("temp","pair","gm")],temp=="elevated")
#names(elev)[3] <- "elev.gm"
gm_trt <- rbind(amb,elev)

tt <- dlply(gm_trt, .(pair), function() t.test(x$amb.gm, x$elev.gm))




gm_sp <- split(gm_trt,gm_trt$pair)





#run all pairs (by leaf type)

ttest_gm <- function(df){
  
  amb <- subset(df[c("temp","pair","gm")],temp=="ambient")
  names(amb)[3] <- "amb.gm"
  elev <- subset(df[c("temp","pair","gm")],temp=="elevated")
  names(elev)[3] <- "elev.gm"
  
  gm_trt <- merge(amb,elev,by="pair")
  
  gm_sp <- split(gm_trt,gm_trt$pair)
  
  runTT <- lapply(gm_sp,function(x) t.test(x$amb.gm, x$elev.gm))
  
  pval <- sapply(runTT,getp)
  pval.melt <- melt(pval)
  
  meandiff <- sapply(runTT,getdiffmean)
  meandiff.melt <- melt(meandiff)
  
  stats <- merge(pval, meandiff,by.x="x2")
}

ttest_gm(gm_sun)

return(stats)


#then loop through the function to run differnt months, change x to pull months out of big gmes data??? 
chamnrs <- levels(treeC$chamber)
for(i in 1:length(chamnrs)){
  ttest_jan(chamnrs[i])
}








#test with one month(these data are old)------------------------------------------------------

#for this there are equal number of observations for each paired compaison (sun, shadehigh, and shadelow)
#when there was an equal number the observation with the highest xsi value was omitted

#read data
gm_jan <- read.csv("raw data/gmes_jan_paired.csv")
#leaf id variable
gm_jan$leafID <- as.factor(paste(gm_jan$leaf, gm_jan$type, gm_jan$pairs, sep = "-"))


#run a  test on one paired comparison (one set of leaves at growing temperature)
temp <- subset(gm_jan, leafID =="sun-high" & pairs =="a")

amb <- subset(temp, temp=="ambient")
elev <- subset(temp, temp=="elevated")


sampleTT <- t.test(amb$gm, elev$gm, paired=T)

getp <- function(x)x$p.value
getp(sampleTT )

getdiffmean <- function(m)m$estimate
getdiffmean(sampleTT)

###try first way with lapply

#how to run them all

#function to split into groups, run ttest, and reutrn output
#split by pairs, 


ttest_gm <- function(df){
  

  amb <- subset(df[c("temp","leafID","gm")],temp=="ambient")
  names(amb)[3] <- "amb.gm"
  elev <- subset(df[c("temp","leafID","gm")],temp=="elevated")
  names(elev)[3] <- "elev.gm"
  
  gm_trt <- merge(amb,elev,by="leafID")

  gm_sp <- split(gm_trt,gm_trt$leafID)
  
  runTT <- lapply(gm_sp,function(x) t.test(x$amb.gm, x$elev.gm, paried =T))
  
  pval <- sapply(runTT,getp)
  pval.melt <- melt(pval)
  
  meandiff <- sapply(runTT,getdiffmean)
  meandiff.melt <- melt(meandiff)
  
  stats <- merge(pval, meandiff,by.x="x2")
}

return(stats)



  #then loop through the function to run differnt months, change x to pull months out of big gmes data??? 
  chamnrs <- levels(treeC$chamber)
for(i in 1:length(chamnrs)){
  ttest_jan(chamnrs[i])
}

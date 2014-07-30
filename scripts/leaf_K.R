source("functions and packages/functions.R")
source("functions and packages/packages.R")
treat <- read.csv("raw data/temp_trt.csv")

#read and formate leaf water potential---------------------------------------------------------------
leafdata <- read.csv("raw data/leaf_data.csv")

wp <- leafdata[, c(1, 3:6)]

#reshape data
wp_day <- (cast(wp, Month+ chamber + leaf ~ wp_type, value = "water_potential"))

#add treatments
wptrt_func <- function(x){
  x <- merge(x, treat)
  x$drydown <- ifelse(x$Month %in% c("Mar", "Apr") & x$chamber %in%c("ch01", "ch03", "ch04", "ch06", "ch08", "ch11"), 
                      "drought", "control")
  return(x)
}

WP <- wptrt_func(wp_day)


#read in licor data and extract transpiration-----------------------------------------------------------------------
licor <- read.csv("raw data/licor_master.csv")
#there is a extra space somewhere wiht "shade", use str_trim
library(stringr)
licor$leaf <- str_trim(licor$leaf)

transp <- subset(licor, select = c("campaign", "chamber", "leaf", "PAR", "Trmmol"))
  transp <- chlab_func(transp)
  transp <- add_Month(transp)

transp_agg <- summaryBy(Trmmol ~ Month +chamber+leaf+PAR, data= transp, FUN=c(mean), keep.names=TRUE)
transp_agg$id <- paste(transp_agg$leaf, transp_agg$PAR, sep="-")

#remove shade high
leafK <- subset(transp_agg, transp_agg$id !=  "shade-high")
#format transp dfr to match that of WP
  leafK$Month <- as.factor(leafK$Month)
  leafK$chamber<- as.factor(leafK$chamber)
  leafK$leaf<- as.factor(leafK$leaf)

#merge
leafcond <- merge(leafK[,c(1:3, 5)], WP)
  leafcond$drydown <- as.factor(leafcond$drydown)

leafcond$leafK <- with(leafcond, Trmmol/(mid-pre))
  Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")
  leafcond$Month <- factor(leafcond$Month, levels = Morder)

###different data substs
leafK_ambT<- subset(leafcond, temp == "ambient")
leafK_eleT<- subset(leafcond, temp == "elevated")
leafKdrought <- subset(leafcond, Month %in% c("Mar", "Apr"))
leafK_nodrought <- subset(leafcond, drydown != "drought")

windows()
bar(leafK, c(leaf, Month), leafcond, col=c("yellowgreen", "green4"), ylim=c(0, .5),xlab="", 
    ylab="Leaf Hydraulic Conductance",half.errbar=FALSE)


windows()
par(mfrow=c(2,1),  omi=c(1,0,0.1,0.1), mar=c(0,7,0,0))   
bar(leafK, c(leaf, Month), data=leafK_ambT,col=c("yellowgreen", "green4"), ylim=c(0, .5),
    xlab="", ylab = "", half.errbar=FALSE)
  title(main="ambient Temperature", line=-1)
bar(leafK, c(leaf, Month), leafK_eleT, col=c("yellowgreen", "green4"), ylim=c(0, .5),half.errbar=FALSE)
  title(main="elevated Temperature", line=-1)


windows()
bar(leafK, c(leaf, drydown), leafKdrought, col=c("yellowgreen", "green4"), ylim=c(0, .5),half.errbar=FALSE)
title(main="Drought", line=-1)


bar(leafK, c(leaf, Month), leafK_nodrought, col=c("yellowgreen", "green4"),ylim=c(0, .5),half.errbar=FALSE)
title(main="Well Watered", line=-1)


#look for bad data in Jan (could be low trmmol)
leafjan <- subset(leafcond, Month == "Jan" & leaf == "sun")

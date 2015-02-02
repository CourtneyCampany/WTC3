source("functions and packages/functions.R")
source("functions and packages/packages.R")
treat <- read.csv("raw data/temp_trt.csv")

#read and formate leaf water potential---------------------------------------------------------------
leafdata <- read.csv("raw data/leaf_data.csv")

wp <- leafdata[, c(1, 3:6)]

#reshape data
wp_day <- (cast(wp, Month+ chamber + leaf ~ wp_type, value = "water_potential"))

#add treatments------------------------------------------------------------------------------------------------------
wptrt_func <- function(x){
  x <- merge(x, treat)
  x$drydown <- ifelse(x$Month %in% c("Mar", "Apr") & x$chamber %in%c("ch01", "ch03", "ch04", "ch06", "ch08", "ch11"), 
                      "drought", "control")
  return(x)
}
#--------------------------------------------------------------------------------------------------------------------

WP <- wptrt_func(wp_day)

  #convert to millipascals
  WP$pre_mp <- with(WP, ((pre/10)*-1))
  WP$mid_mp <- with(WP, ((mid/10)*-1))


#read in licor data and extract transpiration-----------------------------------------------------------------------
licor <- read.csv("raw data/licor_master.csv")
#there is a extra space somewhere wiht "shade", use str_trim
library(stringr)
licor$leaf <- str_trim(licor$leaf)

transp <- licor[,c("campaign", "chamber", "leaf", "PAR", "Trmmol")]
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

leafcond$wpdiff <- with(leafcond, abs(mid_mp - pre_mp))
leafcond$leafK <- with(leafcond, Trmmol/wpdiff)
  Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")
  leafcond$Month <- factor(leafcond$Month, levels = Morder)

#write.csv(leafcond, "calculated data/leaf_conductance.csv", row.names=FALSE)

#bad data and missing value cleaning

  #need to remove ch 10 in march (in complete dataset)
  leafcond2 <- subset(leafcond,  chamber != "ch10" | Month != "Mar")
  leafcond2 <- droplevels(leafcond2)
  #remove jan ch08, possible bad data for sun leaf (really low A and gs)
  leafcond3 <- subset(leafcond2, Month != "Jan" | chamber != "ch08")


###treatment data subsets
leafK_ambT<- subset(leafcond3, temp == "ambient")

leafK_eleT<- subset(leafcond3, temp == "elevated")

leafKdrought <- subset(leafcond3, Month %in% c("Mar", "Apr"))

leafK_nodrought <- subset(leafcond3, drydown != "drought")


#plotting--------------------------------------------------------------------------------------
ypos <- c(2.5,1,0)
leafK_lab <- expression(Leaf-specific~Hydraulic~Conducatuce~~(mmol~H[2]*O~m^-2~s^-1~MPa^-1))

#all data no treatments

pdf(file="output/leafK_all.pdf", onefile=TRUE) 

#windows()
bar(leafK, c(leaf, Month), leafcond3, col=c("yellowgreen", "green4"), ylim=c(0, 4),xlab="", 
    ylab="",half.errbar=FALSE)
  title(main="all data", line=-1, adj=0.05, cex.main=1)
  title(ylab=leafK_lab, mgp=ypos)

#split et and at 
bar(leafK, c(leaf, Month), data=leafK_ambT,col=c("yellowgreen", "green4"),ylim=c(0, 4),
    xlab="", ylab = "", half.errbar=FALSE)
  title(main="Ambient Temperature", line=-1, adj=0.05, cex.main=1)
  title(ylab=leafK_lab, mgp=ypos)

bar(leafK, c(leaf, Month), leafK_eleT, col=c("yellowgreen", "green4"), ylim=c(0, 4),
    xlab="", ylab = "",half.errbar=FALSE)
    title(main="Elevated Temperature", line=-1, adj=0.05, cex.main=1)
    title(ylab=leafK_lab, mgp=ypos)

#well watered trees
bar(leafK, c(leaf, Month), leafK_nodrought, col=c("yellowgreen", "green4"),ylim=c(0, 4),half.errbar=FALSE, 
    xlab="",ylab="")
  title(main="Well Watered", line=-1, adj=0.05, cex.main=1)
  title(ylab=leafK_lab, mgp=ypos)

#Drought trees
bar(leafK, c(leaf, drydown), leafKdrought, col=c("yellowgreen", "green4"), ylim=c(0, 4),
    half.errbar=FALSE, ylab="", xlab="")
title(main="Drought", line=-1, adj=0.05, cex.main=1)
title(ylab=leafK_lab, mgp=ypos)

dev.off() 
#-----------------------------------------------------------------------------------------------------------------
#look for bad data in Jan
Ejan <- subset(transp, Month == "Jan")
sunjan <- subset(leafcond, Month == "Jan" & leaf == "sun")
shajan <- subset(leafcond, Month == "Jan" & leaf == "shade")
####bad data
#sun- jan = ch2 midday wp really high, ch8/5 transpiration low
#sha - jan = ch 9/2 transpiration too high




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

transp <- subset(licor, select = c("campaign", "chamber", "leaf", "PAR", "Trmmol"))
  transp <- chlab_func(transp)
  transp <- add_Month(transp)

transp_agg <- summaryBy(Trmmol ~ Month +chamber+leaf+PAR, data= transp, FUN=c(mean), keep.names=TRUE)
transp_agg$leaf <- as.character(transp_agg$leaf)

#remove shade high
leafK <- subset(transp_agg, transp_agg$leaf != "shade")

leafK <- subset(transp_agg, transp_agg$leaf != "shade" & transp_agg$PAR != "high")

#format transp dfr to match that of WP
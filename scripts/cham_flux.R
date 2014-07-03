require(plotBy)
require(lubridate)
require(plyr)

#read in tdl summary 
tdl_sites <- read.csv("raw data/flux_sites.csv")
tdl_sites$month <- as.character(tdl_sites$month)
tdl_sites$id <- paste(tdl_sites$month, tdl_sites$day, sep="-")

#tdl chamber flux--------------------------------------------------------------------------------------------
tdl_format_func <- function(x) { 
                          c(x$Date <- as.Date(x$Date), 
                          x$time <- as.POSIXct(paste(x$Date, x$time), format="%Y-%m-%d %H:%M:%S %p"),
                          x$day <- gsub("day_", "", x$day),
                          x$month <- tolower(month(x$Date, label=TRUE, abbr=FALSE)),
                          x$id <- paste(x$month, x$day, sep="-"))
                          return(x)
}


#read in summary files for all volumes into a list
chamfluxs <- list.files(path = "raw data/chamber_flux/", pattern="^chflux", full.names = TRUE)
chflux_list <- lapply(chamfluxs, function(x) read.csv(x))

#test formating 
# test <- as.data.frame(chflux_list[5])
# a <- tdl_format_func(test)

#no format each lost
chflux_ls <- llply(chflux_list, function(x) tdl_format_func(x))
# a<-as.data.frame(chflux_ls[4])
# str(chflux_list[4])

# merges with sites 
tdlchams <- llply(chflux_ls, function(x) join(x, tdl_sites[,c(3:5,7)], by=c("id", "SiteOutput"), type="left"))
#b<- as.data.frame(tdlchams[3])

##function that takes each cm list, calculates xsi, 
xsi_function <- function(x){
  x$co2_tot <- (x$CorrConcA_Avg+x$CorrConcB_Avg)/(1-.00474)
  x_sp <- split(x, ref_samp)
  
}

###ddply to split dfr by chamber
###if diff betwen ref and sample then use diff  (will inherently divide a-b)
ddply(data, .(chamber), diff(variable))


#for more complicated then make a simple function

xsifunc <- function(x){
  a <- with(x, CorrConcA_Avg[ref_samp=="a"] )
  b <- with(x, CorrConcA_Avg[ref_samp=="b"] )
  xsi <- a/(a-b)
  
}


####ref a and b needed for xsi and delta calcs


#read in chamber flux from HIEv download--------------------------------------------------------------------------

chamflux_15 <- read.csv("raw data/chamberflux.csv")
chamflux_15$chamber <- as.factor(chamflux_15$chamber)

#subset chamberdelta
fluxdelta_15 <- subset(chamflux_15, select = c("chamber", "RefCO2", "PAR", "DeltaCO2","FluxCO2", "FluxH2O","datetime",
                                               "date","period","Tair_al"))
  
fluxdelta_15$date <- ymd(fluxdelta_15$date)
fluxdelta_15$datetime <- ymd_hms(fluxdelta_15$datetime)
#fluxdelta_15$datetime <- parse_date_time(fluxdelta_15$datetime, format="%Y-%m-%d %I:%M:%S %p")
fluxdelta_15$month <- tolower(month(fluxdelta_15$date, label=TRUE, abbr=FALSE))

#create a list that has this data split into months
fluxdelta15_ls <- dlply(fluxdelta_15, .(month))

#plot one month of flux CO2 with all chambers
windows()
plotBy(DeltaCO2~ datetime|chamber, type='l', data=fluxdelta15_ls[[8]])


#######figure out calculation parameters
##first subset specific dates and then merge those wiht tdl

#dsamp-dref - delta co2
#xsi = a-b (from tdl)
#totalco2


require(plotBy)
require(lubridate)
require(plyr)
library(reshape)

#read in tdl summary 
tdl_sites <- read.csv("raw data/flux_sites.csv")
tdl_sites$month <- as.character(tdl_sites$month)
tdl_sites$id <- paste(tdl_sites$month, tdl_sites$day, sep="-")
tdl_sites$chamber <- gsub("ref", 99, tdl_sites$chamber)

#tdl chamber flux--------------------------------------------------------------------------------------------
tdl_format_func <- function(x) { 
                          c(x$Date <- dmy(x$Date), 
                          x$time <- as.POSIXct(paste(x$Date, x$time), format="%Y-%m-%d %H:%M:%S %p"),
                          x$day <- gsub("day_", "", x$day),
                          x$month <- tolower(month(x$Date, label=TRUE, abbr=FALSE)),
                          x$id <- paste(x$month, x$day, sep="-"),
                          nr<- nrow(x)/26,
                          x$run <- rep(1:nr, each=26),
                          x$co2_tot <- ((x$CorrConcA_Avg+x$CorrConcB_Avg)/(1-.00474)),
                          dfr <- subset(x, SiteOutput != c("3", "4")),
                          nr2<- nrow(dfr)/12,
                          dfr$innerrun <- rep(1:nr2, each=12))
                          return(dfr)
}


#read in summary files for all volumes into a list
chamfluxs <- list.files(path = "raw data/chamber_flux/", pattern="^chflux", full.names = TRUE)
chflux_list <- lapply(chamfluxs, function(x) {a <- read.csv(x)
                                             a<- a[complete.cases(a),]
                                              droplevels(a)})


#sapply(chflux_list,  nrow)/26
#test formating 
#test2 <- as.data.frame(chflux_list[[11]])
#a <- tdl_format_func(test2)

#run tdl format
chflux_ls <- llply(chflux_list, function(x) tdl_format_func(x))

test2 <- chflux_ls[[4]]

# merges with sites 
#######doesnt merge chambers properly (removed ref gases, sites 3and 4 to make it work)
tdlchams <- llply(chflux_ls, function(x) join(x, tdl_sites[,c(3:5,7)], by=c("id", "SiteOutput"), type="left"))
#test <- as.data.frame(tdlchams[[8]])
#write.csv(test, "testtdl.csv", row.names=FALSE)

test <- tdlchams[[4]]
xsidfr <- cast(test, id + run + chamber + innerrun~ ref_samp, value = "co2_tot")


########calculate gm parameters dsamp-dref, xsi, big D
xsifunc <- function(x){
  xsidfr <- cast(x, run + id+chamber + innerrun~ ref_samp, value = "co2_tot")
  xsidfr$xsi <- with(xsidfr, b/(b-a))
  return(xsidfr)
}
  
deltafunc <- function(x){
  deltadfr <- cast(x, time+run + chamber + innerrun~ ref_samp, value = "Corrdel13C_Avg")
  deltadfr$delta <- with(deltadfr, b-a)
  return(deltadfr)
}


xsi_calc <- lapply(tdlchams, function(x) xsifunc(x))
delta_calc <- lapply(tdlchams, function(x) deltafunc(x))

t <- xsi_calc[[3]]

#merge these two drs and then calculate DELTA
###will require delta13sample*******

####should I use the CO2 from chamber flux with CO2 from ambient to calculate xsi?  Use the two tdl deltas
#for delta c13?



# xsifunc <- function(x){
#   a <- with(x, CorrConcA_Avg[ref_samp=="a"] )
#   b <- with(x, CorrConcA_Avg[ref_samp=="b"] )
#   xsi <- a/(a-b)
#   
# }


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


#chamber flux by month

#tdl chamber flux-------------------------------------------------------

#read in summary files for all volumes into a list
chamfluxs <- list.files(path = "raw data/chamber_flux/", pattern="^chflux", full.names = TRUE)
chflux_list <- lapply(chamfluxs, function(x) read.csv(x))

#read in chamber flux from HIEv download

chamflux_15 <- read.csv("raw data/chamberflux.csv")

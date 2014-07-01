###download hiev chamber flux-------------------------------------------
library(devtools)
library(HIEv)

setToken("AEKkuqC79jxDpBvm6WMW ")

#search and download chamber flux data for experiment dates
wtc_search <- searchHIEv(filename="WTCFLUX", "2013-10-01", "2014-05-01")

#search for which one of the files above has all the data I need
wtcflux_15 <-downloadCSV(filename="WTC_Temp_CM_WTCFLUX_20130910-20140531_L0_V1.csv")

write.csv(wtcflux_15, "raw data/chamberflux.csv", row.names=FALSE)
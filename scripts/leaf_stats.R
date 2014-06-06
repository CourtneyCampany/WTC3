source("functions and packages/functions.R")
source("functions and packages/packages.R")
source("scripts/read_data.R")
source("scripts/plot_objects.R")

#run formatting functions
leaf <- leafformat(leaf)
leaf$Date <- paste(15, leaf$month, leaf$year, sep="-")
leaf$Date <- as.Date(leaf$Date, format = "%d-%B-%Y")


plot(lma~Date, data=leaf,col=cols[temp], pch=16,subset=leaf_type == "sun")
plot(lma~Date, data=leaf,col=cols[temp], pch=16,subset=leaf_type == "shade")

plot(wp~Date, data=leaf,col=cols[temp], pch=16,subset=leaf_type == "sun")
plot(wp~Date, data=leaf,col=cols[temp], pch=16,subset=leaf_type == "shade")

leaf_agg <- summaryBy(leaf_mass+leaf_area+lma~temp+leaf_type+Date, data=leaf, FUN=c(mean,se))
wp_agg <- summaryBy(wp~temp+leaf_type+wp_type+Date, data=leaf, FUN=c(mean,se))
wp_agg$wp_type <- as.factor(wp_agg$wp_type)


plot(leaf_area.mean~Date, data=leaf_agg,col=cols[temp], pch=16,subset=leaf_type == "sun")
plot(lma.mean~Date, data=leaf_agg,col=cols[temp], pch=16,subset=leaf_type == "shade")

plot(wp.mean~Date, data=wp_agg,col=cols[temp], pch=pchs[wp_type], subset=leaf_type == "shade")
plot(wp.mean~Date, data=wp_agg,col=cols[temp], pch=pchs[wp_type], subset=leaf_type == "sun")


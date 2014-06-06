#gmes from leaf level (shade and sun leaves) and either high or low light
#light levels are set on a per tree basis
#run paired ttests for each chamber comparison, should be 3-4 measurements per leaf per tree

#read data
gmes_oct <- read.csv("raw data/gmes_oct.csv")
gmes_oct$chamber <- as.factor(gmes_oct$chamber)

gmes_jan <- read.csv("raw data/gmes_jan.csv")
gmes_jan$chamber <- as.factor(gmes_jan$chamber)


#combine leaf and light variables to make a unique ID
gmes_oct$leafID <- as.factor(paste(gmes_oct$leaf, gmes_oct$par_type, sep = "-"))
gmes_jan$leafID <- as.factor(paste(gmes_jan$leaf, gmes_jan$type, sep = "-"))



#quick plots

#Jan-------------------------------------------------------------------------------------------

jan_plot <- subset(gmes_jan, select = c("chamber", "photo", "cond", "leafID", "gm", "leaf_temp", "temp"))


with(subset(jan_plot,leafID=="sun-high"), plot(gm~leaf_temp, col=temp, ylim=c(0.0, .50), xlim=c(18, 35)))
points(gm~leaf_temp, data=subset(jan_plot,leafID=="shade-high"),  pch=16, col=temp)
points(gm~leaf_temp, data=subset(jan_plot,leafID=="shade-low"),  pch=15, col=temp)



#linear model to show fit on graph
sunmodel <- lm(gm~ leaf_temp, data = subset(jan_plot,leafID=="sun-high"))
summary(sunmodel)

shadehighmodel <- lm(gm~ leaf_temp, data = subset(jan_plot,leafID=="shade-high"))
summary(shadehighmodel)

shadelowmodel <- lm(gm~ leaf_temp, data = subset(jan_plot,leafID=="shade-low"))
summary(shadelowmodel)


with(subset(jan_plot,leafID=="sun-high"), plot(gm~leaf_temp, pch=16,col="red",ylim=c(0.0, .50), xlim=c(18, 35)))
points(gm~leaf_temp, data=subset(jan_plot,leafID=="shade-high"),  pch=16, col="blue")
points(gm~leaf_temp, data=subset(jan_plot,leafID=="shade-low"),  pch=16, col="forestgreen")
abline(sunmodel, col="red", lty=1)
abline(shadehighmodel, col="blue", lty=2)
abline(shadelowmodel, col="forestgreen", lty=3, lwd=3)


#Oct-------------------------------------------------------------------------------------------
gmes_plot <- subset(gmes_oct, select = c("chamber", "leafID", "gmes", "leaf_temp", "temp"))



with(subset(gmes_plot,leafID=="sun-high"), plot(gmes~leaf_temp, col=temp))
 points(gmes~leaf_temp, data=subset(gmes_plot,leafID=="shade-high"),  pch=16, col=temp)
  points(gmes~leaf_temp, data=subset(gmes_plot,leafID=="shade-low"),  pch=15, col=temp)

with(subset(gmes_plot,leafID=="sun-high"), plot(gmes~leaf_temp, pch=16,col="red"))
points(gmes~leaf_temp, data=subset(gmes_plot,leafID=="shade-high"),  pch=16, col="blue")
points(gmes~leaf_temp, data=subset(gmes_plot,leafID=="shade-low"),  pch=16, col="forestgreen")



#linear model to show fit on graph
diurnalmodel <- lm(treeC ~ Cflux, data = dayfluxtomass)
summary(diurnalmodel)

modeltrt<- lm(treeC ~ Cflux*CO2_treatment*Water_treatment, data = dayfluxtomass)
anova(modeltrt)

getP <- function(x)anova(x)[[5]][1]
getP(diurnalmodel)
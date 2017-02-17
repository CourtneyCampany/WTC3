## simulated aci and acc curves with chosen ci point--------------------------------------

source("functions and packages/packages.R")
source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")

## dataset for ACi curve predictions------------------------------------------------------------------------------

#read in gm data set (no drought) and CI from discrimination
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
Ci_bar <- read.csv("calculated_data/Ci_bar.csv")
##calculate CC
gmes$Cc<- with(gmes, Ci-Photo/gm)
gmes$cc_ci<- with(gmes, Ci/Cc)

##need to combine Ci_bar with CC and gm

###get average by id
gm_agg <- summaryBy(Photo+Cond+Ci+Cc+PARi ~ id+leaf +light+temp+leaflight, 
                    data=gmes, FUN=mean, keep.names=TRUE)

shade <- gm_agg[gm_agg$leaflight=="shade-low",]
sun <- gm_agg[gm_agg$leaflight=="sun-high",]
fleck <- gm_agg[gm_agg$leaflight=="shade-high",]

####parameters for aci curves/points
ci_sha <- mean(shade$Ci)
ci_sun <- mean(sun$Ci)
ci_fleck <- mean(fleck$Ci)



cc_sha_mean <- c(mean(shade$Photo), mean(shade$Cc))
cc_sun_mean <- c(mean(sun$Photo),mean(sun$Cc))
cc_fleck_mean <- c(mean(fleck$Photo),mean(fleck$Cc))




#read in jmax vcmax
jvc<- read.csv("calculated_data/aci_sunsha.csv")


####use plant ecophys to predict aci curve--------------------------------------------------------------------------

#simulate ACi curves for each leaf, sun and shade at saturating light then simulated shade
sun_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[2,2], Jmax=jvc[2,3],PPFD= mean(sun$PARi))

fleck_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[1,2], Jmax=jvc[1,3], PPFD= mean(sun$PARi))

sha_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[1,2], Jmax=jvc[1,3], PPFD=mean(shade$PARi))

#simulate ci at 300 for different light scenarios
sun_pt <- Photosyn( Ci=ci_sun, Vcmax=jvc[2,2], Jmax=jvc[2,3],PPFD= mean(sun$PARi))
fleck_pt <- Photosyn(Ci=ci_fleck,  Vcmax=jvc[1,2], Jmax=jvc[1,3], PPFD= mean(sun$PARi))
sha_pt <- Photosyn( Ci=ci_sha,Vcmax=jvc[1,2], Jmax=jvc[1,3], PPFD=mean(shade$PARi))



###ACi 
windows(8,6)
par(mar=c(4,4,1,1), mgp=c(2.5, 1, 0))
plot(Photo~Ci, data=gm_agg, type='n', xlim=c(50, 350), ylim=c(0, 25),xlab=cilab, ylab=satlab)
#plot aci curves
points(sun_sim$Ci, sun_sim$ALEAF, col=suncol2, type="l", lwd=3)
points(fleck_sim$Ci, fleck_sim$ALEAF, col=lightscol2,  type="l", lwd=3)
points(sha_sim$Ci, sha_sim$ALEAF, col=shacol2,  type="l", lwd=3)
#plot mean ci for each leaf
points(ALEAF~Ci, data=sun_pt, pch=16, col=suncol2, cex=1.5)
points(ALEAF~Ci, data=fleck_pt, pch=16, col=lightscol2, cex=1.5)
points(ALEAF~Ci, data=sha_pt, pch=16, col=shacol2, cex=1.5)

arrows(x0=sha_pt[1,1], y0=9, x1 = 240.5, y1 = 13.5, length = 0.15, angle = 30,code = 2, lwd=3)

legend("topleft", c("Sun", "Shade-Low Light", "Shade-High Light"), pch=c(16,16,16), lty=c(1,1,1),  
       col=c(suncol2, shacol2, lightscol2), inset = 0.01, bty="n")
legend(x=240, y=12.5, "Capacity of shade leaves to respond to light", bty='n', cex=.65, xjust=0)


###ACc
library(mgcv)
#SUN leaves
sunmod <- gam(Photo ~ s(Cc, k=5), data=sun)

#predict
#get apprpriate vector of gs from sun leaves
cc_sun <- sun[, "Cc"]

#generate sequence and then predict
ccsun_seq <- seq(min(cc_sun), max(cc_sun), length=101)
ccsun_pred <- predict(sunmod, newdata=data.frame(Cc=ccsun_seq), se.fit=TRUE)


#SHADE leaves
shamod <- gam(Photo ~ s(Cc, k=5), data=shade)

#get apprpriate vector cond from sun leaves
cc_sha <- shade[, "Cc"]

#generate sequence and then predict
ccsha_seq <- seq(min(cc_sha), max(cc_sha), length=101)
ccsha_pred <- predict(shamod, newdata=data.frame(Cc=ccsha_seq), type="link", se.fit=TRUE)


#SUNFLECK leaves
fleckmod <- gam(Photo ~ s(Cc, k=5), data=fleck)

#get apprpriate vector cond from sun leaves
cc_fleck <- fleck[, "Cc"]

#generate sequence and then predict
ccfleck_seq <- seq(min(cc_fleck), max(cc_fleck), length=101)
ccfleck_pred <- predict(fleckmod, newdata=data.frame(Cc=ccfleck_seq), type="link", se.fit=TRUE)


##plot
par(mar=c(4,4,1,1), mgp=c(2.5, 1, 0))
plot(Photo~Cc, data=gm_agg, type='n', xlim=c(0, 400), ylim=c(0, 25),xlab=cclab, ylab=satlab)
#plot aci curves
lines(ccsun_seq, ccsun_pred$fit, lty=1, lwd=2,col=suncol2)
lines(ccsha_seq, ccsha_pred$fit, lty=1, lwd=2,col=shacol2)
lines(ccfleck_seq, ccfleck_pred$fit, lty=1, lwd=2,col=lightscol2)
# mean ci for each leaf
points(cc_sun_mean[1]~cc_sun_mean[2], data=sun_pt, pch=16, col=suncol2, cex=1.5)
points(cc_fleck_mean[1]~cc_fleck_mean[2], data=fleck_pt, pch=16, col=lightscol2, cex=1.5)
points(cc_sha_mean[1]~cc_sha_mean[2], data=sha_pt, pch=16, col=shacol2, cex=1.5)

      

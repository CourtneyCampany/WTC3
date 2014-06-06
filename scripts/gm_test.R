
source("functions and packages/functions.R")
source("functions and packages/packages.R")
source("read_data.R")

gm<- read.csv("gm_raw.csv")
treat <- read.csv("raw data/chamber_trt.csv")

gm <- merge(gm, treat, all=TRUE)
gm$chamber <- as.character(gm$chamber)

gm$drydown <- ifelse(gm$month %in% c("march", "apr") & gm$chamber %in%c("ch01", "ch03", "ch04", "ch06", "ch08", "ch11"), 
                     "drought", "control")

gm$id <- paste(gm$leaf, gm$type, sep="-")

#set an upper limit to reasonable gm values????
gm_limit <- subset(gm, gm <= .65)

#write.csv(gm_limit, "gm_limit.csv", row.names=FALSE)
gm_watered <- subset(gm_limit, drydown =="control")
gm_leaf <- subset(gm_watered, id !="shade-high")

pchs <- c(1, 16)
cols <- c("blue","red")

sun <- subset(gm_leaf, leaf=="sun")
shade <- subset(gm_leaf, leaf=="shade"& type=="low")
shade_cap<- subset(gm_leaf, leaf=="shade" & type=="high")
AT<- subset(gm_leaf, temp =="ambient")
ET<- subset(gm_leaf, temp =="elevated")
dry<- subset(gm_leaf, temp =="drought")
wet<- subset(gm_leaf, temp =="control")

#gm vs cond---------------------------------------------------------------------------

#stats (sun and shade)

#sun leaves (no drought)
gsgm_sun <- lm(gm~ Cond, data=gm_leaf, leaf=="sun")
gsgm_sunwarm <- lm(gm~ Cond*temp, data=gm_leaf, leaf=="sun")
summary(gsgm_sun)
summary(gsgm_sunwarm)

#shade leaves (no drought)
gsgm_sha <- lm(gm~ Cond, data=shade)
gsgm_sha_warm <- lm(gm~ Cond*temp, data=shade)
summary(gsgm_sha)
summary(gsgm_sha_warm)

#plotting
plot(gm~Cond, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "sun", ylim=c(0,1))
abline(gsgm_sun)

plot(gm~Cond, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "shade", ylim=c(0,1))
abline(gsgm_sha)


#gm vs A--------------------------------------------------------------------------

#stats (sun and shade)

#sun leaves (no drought)
Agm_sun <- lm(Photo~ gm, data=gm_leaf, leaf=="sun")
Agm_sunwarm <- lm(Photo~ gm*temp, data=sun)
summary(Agm_sun)
summary(Agm_sunwarm)

#shade leaves (no drought)
Agm_sha <- lm(Photo~ gm, data=shade)
Agm_sha_warm <- lm(Photo~ gm*temp, data=shade)
summary(Agm_sha)
summary(Agm_sha_warm)

#plotting
plot(Photo~ gm, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "sun", xlim=c(0,0.8), ylim=c(0,25))
abline(Agm_sun)

plot(Photo~ gm, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "shade", xlim=c(0,.8), ylim=c(0,25))
abline(Agm_sha)

#gm vs temp--------------------------------------------------------------------------

#stats (sun and shade)

#sun leaves (no drought)
Tgm_sun <- lm(gm~ CTleaf, data=gm_leaf, leaf=="sun")
Tgm_sunwarm <- lm(gm~ CTleaf*temp, data=sun)
summary(Tgm_sun)
summary(Tgm_sunwarm)

#shade leaves (no drought)
Tgm_sha <- lm(gm~ CTleaf, data=shade)
Tgm_sha_warm <- lm(gm~ CTleaf*temp, data=shade)
summary(Tgm_sha)
summary(Tgm_sha_warm)

#plotting
plot(gm~CTleaf, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "sun", ylim=c(0,1))
abline(Tgm_sun)

plot(gm~CTleaf, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "shade", ylim=c(0,1))
abline(Tgm_sha)

#cond vs A--------------------------------------------------------------------------
#stats (sun and shade)

#sun leaves (no drought)
Ags_sun <- lm(Cond~ Photo, data=gm_leaf, leaf=="sun")
Ags_sunwarm <- lm(Cond~ Photo*temp, data=sun)
summary(Ags_sun)
summary(Ags_sunwarm)

#shade leaves (no drought)
Ags_sha <- lm(Cond~ Photo, data=shade)
Ags_sha_warm <- lm(Cond~ Photo*temp, data=shade)
summary(Ags_sha)
summary(Ags_sha_warm)

#plotting
plot(Cond~ Photo, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "sun", ylim=c(0,.25))
abline(Ags_sun)

plot(Cond~ Photo, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "shade", ylim=c(0,.45))
abline(Tgm_sha)

#Cond vs temp--------------------------------------------------------------------------

#stats (sun and shade)

#sun leaves (no drought)
Tgs_sun <- lm(Cond~ CTleaf, data=gm_leaf, leaf=="sun")
Tgs_sunwarm <- lm(Cond~ CTleaf*temp, data=sun)
summary(Tgs_sun)
summary(Tgs_sunwarm)

#shade leaves (no drought)
Tgs_sha <- lm(Cond~ CTleaf, data=shade)
Tgs_sha_warm <- lm(Cond~ CTleaf*temp, data=shade)
summary(Tgs_sha)
summary(Tgs_sha_warm)

#plotting
plot(Cond~CTleaf, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "sun")
abline(Tgs_sun)

plot(Cond~CTleaf, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "shade")
abline(Tgs_sha)

A vs temp--------------------------------------------------------------------------

#stats (sun and shade)

#sun leaves (no drought)
TPhoto_sun <- lm(Photo~ CTleaf, data=gm_leaf, leaf=="sun")
TPhoto_sunwarm <- lm(Photo~ CTleaf*temp, data=sun)
summary(TPhoto_sun)
summary(TPhoto_sunwarm)

#shade leaves (no drought)
TPhoto__sha <- lm(Photo~ CTleaf, data=shade)
TPhoto__sha_warm <- lm(Photo~ CTleaf*temp, data=shade)
summary(TPhoto__sha)
summary(TPhoto__sha_warm)

#plotting
plot(Photo~CTleaf, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "sun", ylim=c(0,30))
abline(TPhoto_sun)

plot(Photo~CTleaf, data=gm_leaf,col=cols[temp], pch=16,subset=leaf == "shade", ylim=c(0,30))
abline(TPhoto__sha)


#means-----------------------------------------------------------------------
gm_leaf <- subset(gm_watered, id !="shade-high")

gmleaf_sp <- split(gm_leaf, id)

#means
gm_agg <- summaryBy(gm+Cond+Photo ~ leaf+month, data=gm_watered,FUN=mean, keep.names=TRUE)

bar(dv = Photo, 
    factors = c(Cond,gm), 
    dataframe = gm_leaf, 
    errbar = TRUE, 
    ylim=c(0, .5))  #I increased the upper y-limit to accommodate the legend.






plot(gm~Ci, data=gm_limit, col=cols[temp], pch=pchs[water])
plot(Delta~CTleaf, data=gm_limit, col=cols[temp], pch=pchs[water])
plot(Photo~CTleaf, data=AT, col=cols[temp], pch=pchs[water])
plot(Photo~CTleaf, data=ET, col=cols[temp], pch=pchs[water])

plot(gm~Ci.Ca, data=gm_limit, col=cols[temp], pch=pchs[water])







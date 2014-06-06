source("functions and packages/functions.R")
source("functions and packages/packages.R")
source("scripts/read_data.R")
source("scripts/plot_objects.R")


#run formatting functions
leaf <- leafformat(leaf)
gm <- gmformat(gm)

#calculate Cc
gm$Cc <- with(gm, Ci-Photo/gm)

#set data limits to get rid of bad data (upper and lower from Flexas review)
gm_limit <- subset(gm, gm <= 1.1 & gm >= .05)

write.csv(gm_limit, "gm_limit.csv", row.names=FALSE)

#take some subsets for further anaylses 
gm_watered <- subset(gm_limit, drydown =="control")
gm_drydown <- subset(gm_limit, drydown =="drought")
gm_leaf <- subset(gm_watered, ID !="shade-high")
gm_shadhi <- subset(gm_watered, ID =="shade-high")

write.csv(gm_shadhi, "calculated data/gm_shadhi.csv", row.names=FALSE)
write.csv(gm_leaf, "calculated data/gm_sunsha.csv", row.names=FALSE)

gm_ch <- summaryBy(. ~ chamber+leaf+month+temp+drydown, data=gm_leaf, FUN=c(mean), keep.names=TRUE)
#gm treatment means--------------------------------------------------------
gm_agg <- summaryBy(gm+Photo+Cond ~ leaf+ month+ temp+ drydown, data=gm_leaf, FUN=c(mean, se))


#leaf stats & treatment means & plots --------------------------------------------------------
lma_agg <- summaryBy(lma+leaf_area+leaf_mass ~ leaf_type+ month+ temp+ drydown, data=leaf, FUN=c(mean, se))

lmasun_lm <- lm(lma ~ temp, data=leaf, leaf_type=="sun")
lmasha_lm <- lm(lma ~ temp, data=leaf, leaf_type=="shade")
summary(lmasun_lm)
summary(lmasha_lm)


#stats on gm, temp, gs and A with sun and shade leaves-------------------------------------

#gm v Cond
#sun leaves (no drought)
gsgm_sun <- lm(gm~ Cond, data=gm_leaf, leaf=="sun")
gsgm_sunwarm <- lm(gm~ Cond*temp, data=gm_leaf, leaf=="sun")
summary(gsgm_sun)
summary(gsgm_sunwarm)

#shade leaves (no drought)
gsgm_sha <- lm(gm~ Cond, data=gm_leaf, leaf=="shade")
gsgm_sha_warm <- lm(gm~ Cond*temp, data=gm_leaf, leaf=="shade")
summary(gsgm_sha)
summary(gsgm_sha_warm)

#gm v A
#sun leaves (no drought)
Agm_sun <- lm(Photo~ gm, data=gm_leaf, leaf=="sun")
Agm_sunwarm <- lm(Photo~ gm*temp, data=gm_leaf, leaf=="sun")
summary(Agm_sun)
summary(Agm_sunwarm)

#shade leaves (no drought)
Agm_sha <- lm(Photo~ gm, data=gm_leaf, leaf=="shade")
Agm_sha_warm <- lm(Photo~ gm*temp, data=gm_leaf, leaf=="shade")
summary(Agm_sha)
summary(Agm_sha_warm)

#gm vs temp
#sun leaves (no drought)
Tgm_sun <- lm(gm~ CTleaf, data=gm_leaf, leaf=="sun")
Tgm_sunwarm <- lm(gm~ CTleaf*temp, data=gm_leaf, leaf=="sun")
summary(Tgm_sun)
summary(Tgm_sunwarm)

#shade leaves (no drought)
Tgm_sha <- lm(gm~ CTleaf, data=gm_leaf, leaf=="shade")
Tgm_sha_warm <- lm(gm~ CTleaf*temp, data=gm_leaf, leaf=="shade")
summary(Tgm_sha)
summary(Tgm_sha_warm)


# plot gm vs cond--------------------------------------------------
windows()
plot(gm~Cond, data=gm_ch,col=cols[temp], pch=16,subset=leaf == "sun", ylim=c(0,1), xlim=c(0, .5), ylab="",
     xlab=condlab)
  points(gm~Cond, data=gm_ch,col=cols[temp], pch=1, subset=leaf == "shade")
  legend("topright", leglab, pch=pch4, col=col4, inset = 0.03)
  title(ylab=gmlab, mgp=ypos, cex=1.2)
  ablineclip(gsgm_sun, lty=1, x1=min(gm_ch$Cond, x2=max(gm_ch$Cond)))
  ablineclip(gsgm_sha, lty=2, x1=min(gm_ch$Cond, x2=max(gm_ch$Cond)))
dev.copy2pdf(file="output/gm_gs.pdf")
dev.off()


#plot gm vs A
windows()
plot(Photo~ gm, data=gm_ch,col=cols[temp],pch=16,subset=leaf=="sun", xlim=c(0,1), ylab="", xlab=gmlab,ylim=c(0,30))
  points(Photo~ gm, data=gm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
  legend("topright", leglab, pch=pch4, col=col4, inset = 0.03)  
  title(ylab=satlab, mgp=ypos, cex=1.2)
  #ablineclip(Agm_sun,lty=1, x1=min(gm_ch$gm, x2=max(gm_ch$gm)))
  #ablineclip(Agm_sha, lty=2, x1=min(gm_ch$gm, x2=max(gm_ch$gm)))
dev.copy2pdf(file="output/gm_A.pdf")
dev.off()


# plot(gm~ Photo, data=gm_ch, col=cols[temp],subset=leaf == "sun", pch=16, ylim=c(0,1.5), xlim=c(5,25))
#   points(gm~ Photo, data=gm_ch, col=cols[temp],subset=leaf == "shade", pch=1, ylim=c(0,1.5), xlim=c(5,25))
#   legend("topright", leglab, pch=pch4, col=col4, inset = 0.03) 


#plot gm vs temp--------------------------------------------------------------------------
windows()
plot(gm~CTleaf, data=gm_ch,col=cols[temp],pch=16,subset=leaf=="sun",
     xlim=c(15,40),ylim=c(0,1),xlab="Leaf Temperature (C)", ylab="")
  points(gm~CTleaf, data=gm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
  legend("topright", leglab, pch=pch4, col=col4, inset = 0.03) 
  title(ylab=gmlab, mgp=ypos, cex=1.2)
  ablineclip(Tgm_sha, lty=1, x1=min(gm_ch$CTleaf, x2=max(gm_ch$CTleaf)))
  ablineclip(Tgm_sun, lty=2, x1=min(gm_ch$CTleaf, x2=max(gm_ch$CTleaf)))
dev.copy2pdf(file="output/gm_T.pdf")
dev.off()

#cond vs temp
windows()
plot(Cond~CTleaf, data=gm_ch,col=cols[temp],pch=16,subset=leaf=="sun",
     xlim=c(15,40),ylim=c(0,.5),xlab="Leaf Temperature (C)", ylab="")
points(Cond~CTleaf, data=gm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
legend("topright", leglab, pch=pch4, col=col4, inset = 0.03) 
title(ylab=condlab, mgp=ypos, cex=1.2)
dev.copy2pdf(file="output/gs_T.pdf")
dev.off()

#plot Cc vs photo--------------------------------------------------------------------------
#sun leaves (no drought)
Acc_sun <- lm(Photo~ Cc, data=gm_leaf, leaf=="sun")
Acc_sunwarm <- lm(Photo~ Cc*temp, data=gm_leaf, leaf=="sun")
summary(Acc_sun)
summary(Acc_sunwarm)

#shade leaves (no drought)
Acc_sha <- lm(Photo~ Cc, data=gm_leaf, leaf=="shade")
Acc_sha_warm <- lm(Photo~ Cc*temp, data=gm_leaf, leaf=="shade")
summary(Acc_sha)
summary(Acc_sha_warm)


#Cc
windows()
plot(Photo~Cc, data=gm_ch,col=cols[temp], pch=16,subset=leaf == "sun", ylab="",ylim=c(0,30), xlim=c(50, 350))
points(Photo~Cc, data=gm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
legend("topleft", leglab, pch=pch4, col=col4, inset = 0.03) 
ablineclip(Acc_sun, lty=1, x1=min(gm_ch$Cc), x2=max(gm_ch$Cc))
ablineclip(Acc_sha, lty=2, x1=min(gm_ch$Cc), x2=max(gm_ch$Cc))
title(ylab=satlab, mgp=ypos)
dev.copy2pdf(file="output/A_Cc.pdf")
dev.off()

#Ci

#sun leaves (no drought)
Aci_sun <- lm(Photo~ Ci, data=gm_leaf, leaf=="sun")
Aci_sunwarm <- lm(Photo~ Ci*temp, data=gm_leaf, leaf=="sun")
summary(Aci_sun)
summary(Aci_sunwarm)

#shade leaves (no drought)
Aci_sha <- lm(Photo~ Ci, data=gm_leaf, leaf=="shade")
Aci_sha_warm <- lm(Photo~ Ci*temp, data=gm_leaf, leaf=="shade")
summary(Aci_sha)
summary(Aci_sha_warm)

#plot
windows()
plot(Photo~Ci, data=gm_ch,col=cols[temp], pch=16,subset=leaf == "sun", ylab="", ylim=c(0,30), xlim=c(50, 350))
points(Photo~Ci, data=gm_ch,col=cols[temp], pch=1,subset=leaf == "shade")
legend("topleft", leglab, pch=pch4, col=col4, inset = 0.03) 
ablineclip(Aci_sun, lty=1, x1=min(gm_ch$Ci), x2=max(gm_ch$Ci))
ablineclip(Aci_sha, lty=2, x1=min(gm_ch$Ci), x2=max(gm_ch$Ci))
title(ylab=satlab, mgp=ypos)
dev.copy2pdf(file="output/A_Ci.pdf")
dev.off()


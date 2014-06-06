
#turning the lights on gm
shahi <- read.csv("calculated data/gm_shadhi.csv")
shahi$variable <- paste(shahi$leaf, shahi$temp, sep="-")

gmleaf <- read.csv("calculated data/gm_sunsha.csv")
gmleaf$variable <- paste(gmleaf$leaf, gmleaf$temp, sep="-")

#paired id dfr
idDF <- data.frame(id = unique(gmleaf$id))

shalow <- subset(gmleaf, leaf=="shade")
sun <- subset(gmleaf, leaf=="sun")

#summary of gm for each pair
shahi_id<- summaryBy(Photo+gm~ variable+id, data=shahi, FUN=c(mean), keep.names=TRUE)
shalow_id <- summaryBy(Photo+gm~ variable+id, data=shalow, FUN=c(mean), keep.names=TRUE)
sun_id <- summaryBy(Photo+gm~ variable+id, data=sun, FUN=c(mean), keep.names=TRUE)

#split by treatment and add id for plotting na
shahi_sp <- dlply(shahi_id, .(variable), function(x) merge(x, idDF, all = TRUE))
shalow_sp <- dlply(shalow_id, .(variable), function(x) merge(x, idDF, all = TRUE))
sun_sp <- dlply(sun_id, .(variable), function(x) merge(x, idDF, all = TRUE))

#plot

#new labels
gmlab <-expression(g[m]~~(mol~m^-2~s^-1))
ypos <- c(2.5,1,0)

leglights <-  c(expression(paste(vs~~Shade~PAR[high]~AT)), 
                expression(paste(vs~~Shade~PAR[high]~ET)),
              expression(paste(vs~~Sun~PAR[high]~AT)))

leglights2<-  c(expression(paste(vs~~Shade~PAR[high]~AT)), 
                expression(paste(vs~~Shade~PAR[high]~ET)),
                expression(paste(vs~~Sun~PAR[high]~AT)),
                expression(paste(vs~~Sun~PAR[high]~ET)))

col2 <-c("gold1", "purple","red")
col3 <-c("gold1", "purple","red", "blue")

shatgm <- expression(g[m]~Shade~PAR[low]~AT~~(mol~m^-2~s^-1))
shatA <- expression(A~Shade~~PAR[low]~AT~~(mol~m^-2~s^-1))
satlab <- expression(A~~(mu*mol~m^-2~s^-1))

plot(shahi_sp[[1]][,4]~shalow_sp[[1]][,4],pch=21,bg="gold1",cex=2.5, ylab="", xlab=shatgm, xlim=c(0,1), ylim=c(0,1)) 
abline(0,1, lty=2)
title(ylab=gmlab, mgp=ypos, cex=1.2)
points(shahi_sp[[2]][,4]~shalow_sp[[1]][,4], pch=21, bg="purple",cex=2.5, ylab="")
points(sun_sp[[1]][,4]~shalow_sp[[1]][,4], pch=21, bg="red",cex=2.5, ylab="") 
legend("topright", leglights, pch=21, pt.bg=col2, pt.cex=2,bg="white",inset = 0.03)
dev.copy2pdf(file="output/presentations/gmlightson.pdf")
dev.off()


plot(shahi_sp[[1]][,3]~shalow_sp[[1]][,3],pch=21,bg="gold1",cex=2.5, ylab="", xlab=shatA, xlim=c(0,25), ylim=c(0,25)) 
abline(0,1, lty=2)
title(ylab=satlab, mgp=ypos, cex=1.2)
points(shahi_sp[[2]][,3]~shalow_sp[[1]][,3], pch=21, bg="purple",cex=2.5, ylab="", xlab=shatA)
points(sun_sp[[1]][,3]~shalow_sp[[1]][,3], pch=21, bg="red",cex=2.5, ylab="") 
#points(sun_sp[[2]][,3]~shalow_sp[[1]][,3], pch=21, bg="blue",cex=2.5, ylab="") 
legend("topright", leglights, pch=21, pt.bg=col2, pt.cex=2,bg="white",inset = 0.03)
dev.copy2pdf(file="output/presentations/Alightson1.pdf")
dev.off()


plot(shahi_sp[[1]][,3]~shalow_sp[[1]][,3],pch=21,bg="gold1",cex=2.5, ylab="", xlab=shatA, xlim=c(0,25), ylim=c(0,25)) 
abline(0,1, lty=2)
title(ylab=satlab, mgp=ypos, cex=1.2)
points(shahi_sp[[2]][,3]~shalow_sp[[1]][,3], pch=21, bg="purple",cex=2.5, ylab="", xlab=shatA)
points(sun_sp[[1]][,3]~shalow_sp[[1]][,3], pch=21, bg="red",cex=2.5, ylab="") 
points(sun_sp[[2]][,3]~shalow_sp[[1]][,3], pch=21, bg="blue",cex=2.5, ylab="") 
legend("topright", leglights2, pch=21, pt.bg=col3, pt.cex=2,bg="white",inset = 0.03)
dev.copy2pdf(file="output/presentations/Alightson2.pdf")
dev.off()




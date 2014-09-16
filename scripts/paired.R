source("functions and packages/functions.R")
source("functions and packages/packages.R")
#source("scripts/read_data.R")
source("scripts/plot_objects_paired.R")


###there is no ch10 shade in march, check for this in script and paired comparisons

#read in gmsunsha
#gm data, no drought, no shade high
gm<- read.csv("calculated data/gm_sunsha.csv")
gm$variable <- paste(gm$leaf, gm$temp, sep="-")

cl <- (c("yellowgreen", "green4"))
sslab <- c("Shade", "Sun")

png(filename = "output/presentations/ciA.png", width = 11, height = 8.5, units = "in", res= 400)
plot(Photo~Ci, data=gm, pch=21, bg=cl[leaf], xlim=c(0,400), ylim=c(0,30), xlab=cilab, ylab="", cex=1.3)
title(ylab=satlab, mgp=ypos, cex=1.2)
legend("topright", sslab, pch=21, pt.bg=cl <- (c("yellowgreen", "green4")), pt.cex=2, bg="white",inset = 0.03) 
dev.off()

png(filename = "output/presentations/gmA.png", width = 11, height = 8.5, units = "in", res= 400)
plot(Photo~gm, data=gm, pch=21, bg=cl[leaf], xlim=c(0,1), ylim=c(0,30), xlab=gmlab, ylab="", cex=1.3)
title(ylab=satlab, mgp=ypos, cex=1.2)
legend("topright", sslab, pch=21, pt.bg=cl <- (c("yellowgreen", "green4")), pt.cex=2, bg="white",inset = 0.03) 
dev.off()

png(filename = "output/presentations/ccA.png", width = 11, height = 8.5, units = "in", res= 400)
plot(Photo~Cc, data=gm, pch=21, bg=cl[leaf], xlim=c(0,400), ylim=c(0,30), xlab=cclab, ylab="", cex=1.3)
title(ylab=satlab, mgp=ypos, cex=1.2)
legend("topright", sslab, pch=21, pt.bg=cl <- (c("yellowgreen", "green4")), pt.cex=2, bg="white",inset = 0.03) 
dev.off()

gm$ratio<- with(gm, gm/Cond)
plot(Photo~ratio, data=gm, pch=21, bg=cl[leaf], xlim=c(0,25), ylim=c(0,30), xlab=cclab, ylab="", cex=1.3)
title(ylab=satlab, mgp=ypos, cex=1.2)
legend("topright", sslab, pch=21, pt.bg=cl <- (c("yellowgreen", "green4")), pt.cex=2, bg="white",inset = 0.03) 


#paired id dfr
idDF <- data.frame(id = unique(gm$id))

#library(reshape)
#gmCst <- cast(gm_id, id ~ variable, value= "gm")

#gm data----------------------------------------------------------------------------------------

#make an emptyr plot for presentation
png(filename = "output/presentations/emptyplot.png", width = 11, height = 8.5, units = "in", res= 400)
plot(gm_sp[[4]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5, ylab="", xlab=suatci, xlim=c(0,1), ylim=c(0,1), type="n") 
abline(0,1, lty=2)
points(.45,.6, pch=21, bg=wa[1], cex=2.5, ylab=shatgm) 
points(.4,.4, pch=21, bg=wa[3], cex=2.5, ylab=shatgm)
points(.65,.3, pch=21, bg=wa[5], cex=2.5, ylab=shatgm) 
ablineclip(.6,0, x2=.45, lty=6, lwd=2.5,col=wa[1])
ablineclip(v=.45, y2=.6, lty=6, lwd=2.5,col=wa[1])
title(ylab=cilab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, pt.cex=2,bg="white",inset = 0.03)  
dev.off()


x <- rnorm(100)
y <- x + rnorm(100)
lmfit <- lm(y~x)
plot(x,y,xlim=c(-3.5,3.5))
ablineclip(lmfit,x1=-2,x2=2,lty=2)
ablineclip(h=0,x1=-2,x2=2,lty=3,col="red")
ablineclip(v=0,y1=-2.5,y2=1.5,lty=4,col="green")





#summary of spot measurements for each id
gm_id <- summaryBy(gm~ variable+id, data=gm, FUN=c(mean), keep.names=TRUE)
gm_sp <- dlply(gm_id, .(variable), function(x) merge(x, idDF, all = TRUE))

plot(gm_sp[[4]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5, ylab="", xlab=suatgm, xlim=c(0,1), ylim=c(0,1)) 
abline(0,1, lty=2)
title(ylab=gmlab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, pt.cex=2,bg="white",inset = 0.03)  
dev.copy2pdf(file="output/presentations/gmpair1.pdf")
dev.off()

plot(gm_sp[[1]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab="", xlim=c(0,1), ylim=c(0,1),xlab=suatgm)     
abline(0,1)
title(ylab=gmlab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, pt.cex=2,bg="white",inset = 0.03) 
dev.copy2pdf(file="output/presentations/gmpair2.pdf")
dev.off()

plot(gm_sp[[2]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab="", xlim=c(0,1), ylim=c(0,1),xlab=suatgm)     
abline(0,1)
title(ylab=gmlab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, pt.cex=2,bg="white",inset = 0.03) 
dev.copy2pdf(file="output/presentations/gmpair3.pdf")
dev.off()

#one figure
plot(gm_sp[[4]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5, ylab="", xlab=suatgm, xlim=c(0,1), ylim=c(0,1)) 
abline(0,1, lty=2)
title(ylab=gmlab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03)  
points(gm_sp[[1]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab=shatgm)   
points(gm_sp[[2]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab=shetgm)    
dev.copy2pdf(file="output/presentations/gmpair4.pdf")
dev.off()

#A data----------------------------------------------------------------------------------------
A_id <- summaryBy(Photo~ variable+id, data=gm, FUN=c(mean), keep.names=TRUE)
A_sp <- dlply(A_id, .(variable), function(x) merge(x, idDF, all = TRUE))


#one figure
plot(A_sp[[4]][,3]~A_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5, ylab="", xlab=suatA, xlim=c(0,25), ylim=c(0,25)) 
abline(0,1, lty=2)
title(ylab=satlab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03)  
points(A_sp[[1]][,3]~A_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab=shatA)   
points(A_sp[[2]][,3]~A_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab=shetA)    
dev.copy2pdf(file="output/presentations/Apair4.pdf")
dev.off()


#Cc data----------------------------------------------------------------------------------------

#summary of spot measurements for each id
Cc_id <- summaryBy(Cc ~variable+id, data=gm, FUN=c(mean), keep.names=TRUE)
Cc_sp <- dlply(Cc_id, .(variable), function(x) merge(x, idDF, all = TRUE))

plot(Cc_sp[[4]][,3]~Cc_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5, ylab="", xlab=suatcc, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cclab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03)  
dev.copy2pdf(file="output/presentations/ccpair1.pdf")
dev.off()

plot(Cc_sp[[1]][,3]~Cc_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab="", xlab=suatcc,xlim=c(0,400), ylim=c(0,400))  
abline(0,1, lty=2)
title(ylab=cclab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
dev.copy2pdf(file="output/presentations/ccpair2.pdf")
dev.off()

plot(Cc_sp[[2]][,3]~Cc_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab="", xlab=suatcc,xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cclab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
dev.copy2pdf(file="output/presentations/ccpair3.pdf")
dev.off()

plot(Cc_sp[[4]][,3]~Cc_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5, ylab="", xlab=suatcc, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cclab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(Cc_sp[[1]][,3]~Cc_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab=shatgm)
points(Cc_sp[[2]][,3]~Cc_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab=shetgm) 
dev.copy2pdf(file="output/presentations/ccpair4.pdf")
dev.off()

#Ci data----------------------------------------------------------------------------------------
Ci_id <- summaryBy(Ci ~variable+id, data=gm, FUN=c(mean), keep.names=TRUE)
Ci_sp <- dlply(Ci_id, .(variable), function(x) merge(x, idDF, all = TRUE))

plot(Ci_sp[[4]][,3]~Ci_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5,,ylab="", xlab=suatci, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cilab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(Ci_sp[[1]][,3]~Ci_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab="")     
points(Ci_sp[[2]][,3]~Ci_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab="")   
dev.copy2pdf(file="output/presentations/cipair4.pdf")
dev.off()

plot(Ci_sp[[4]][,3]~Ci_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5,,ylab="", xlab=suatci, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cilab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
dev.copy2pdf(file="output/presentations/cipair1.pdf")
dev.off()

plot(Ci_sp[[1]][,3]~Ci_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5,,ylab="", xlab=suatci, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cilab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
dev.copy2pdf(file="output/presentations/cipair2.pdf")
dev.off()

plot(Ci_sp[[2]][,3]~Ci_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5,,ylab="", xlab=suatci, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cilab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
dev.copy2pdf(file="output/presentations/cipair3.pdf")
dev.off()


#test with transparency

wa1 <- alpha(wa[1], 0.15)
wa3 <- alpha(wa[3], 0.15)
wa5 <- alpha(wa[5], 0.15)
ypos1 <- c(2.25,1,0)

#Ci------------------------------------
par(cex.lab=1.5)

png(filename = "output/presentations/ciscaled1.png", width = 11, height = 8.5, units = "in", res= 400)
plot(Ci_sp[[4]][,3]~Ci_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5,,ylab="", xlab=suatci, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cilab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(Ci_sp[[1]][,3]~Ci_sp[[3]][,3], pch=16, col=wa1, cex=2.5, ylab="")     
points(Ci_sp[[2]][,3]~Ci_sp[[3]][,3], pch=16, col=wa5, cex=2.5, ylab="")   
#dev.copy2pdf(file="output/presentations/ciscaled.pdf")
dev.off()

png(filename = "output/presentations/ciscaled2.png", width = 11, height = 8.5, units = "in", res= 400)
plot(Ci_sp[[4]][,3]~Ci_sp[[3]][,3], pch=16, col=wa3, cex=2.5,ylab="", xlab=suatci, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cilab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(Ci_sp[[1]][,3]~Ci_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab="")     
points(Ci_sp[[2]][,3]~Ci_sp[[3]][,3], pch=16, col=wa5, cex=2.5, ylab="")  
#dev.copy2pdf(file="output/presentations/ciscaled2.pdf")
dev.off()

png(filename = "output/presentations/ciscaled3.png", width = 11, height = 8.5, units = "in", res= 400)
plot(Ci_sp[[4]][,3]~Ci_sp[[3]][,3], pch=16, col=wa3, cex=2.5,,ylab="", xlab=suatci, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cilab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(Ci_sp[[1]][,3]~Ci_sp[[3]][,3], pch=16, col=wa1, cex=2.5, ylab="")     
points(Ci_sp[[2]][,3]~Ci_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab="") 
#dev.copy2pdf(file="output/presentations/ciscaled3.pdf")
dev.off() 

#Cc------------------------------------
png(filename = "output/presentations/ccscaled1.png", width = 11, height = 8.5, units = "in", res= 400)
plot(Cc_sp[[4]][,3]~Cc_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5,,ylab="", xlab=suatcc, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cclab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(Cc_sp[[1]][,3]~Cc_sp[[3]][,3], pch=16, col=wa1, cex=2.5, ylab="")     
points(Cc_sp[[2]][,3]~Cc_sp[[3]][,3], pch=16, col=wa5, cex=2.5, ylab="")   
#dev.copy2pdf(file="output/presentations/ccscaled.pdf")
dev.off()

png(filename = "output/presentations/ccscaled2.png", width = 11, height = 8.5, units = "in", res= 400)
plot(Cc_sp[[4]][,3]~Cc_sp[[3]][,3], pch=16, col=wa3, cex=2.5,ylab="", xlab=suatcc, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cclab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(Cc_sp[[1]][,3]~Cc_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab="")     
points(Cc_sp[[2]][,3]~Cc_sp[[3]][,3], pch=16, col=wa5, cex=2.5, ylab="")  
#dev.copy2pdf(file="output/presentations/ccscaled2.pdf")
dev.off()

png(filename = "output/presentations/ccscaled3.png", width = 11, height = 8.5, units = "in", res= 400)
plot(Cc_sp[[4]][,3]~Cc_sp[[3]][,3], pch=16, col=wa3, cex=2.5,,ylab="", xlab=suatcc, xlim=c(0,400), ylim=c(0,400)) 
abline(0,1, lty=2)
title(ylab=cclab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(Cc_sp[[1]][,3]~Cc_sp[[3]][,3], pch=16, col=wa1, cex=2.5, ylab="")     
points(Cc_sp[[2]][,3]~Cc_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab="") 
#dev.copy2pdf(file="output/presentations/ccscaled3.pdf")
dev.off() 

#gm------------------------------------
png(filename = "output/presentations/gmscaled1.png", width = 11, height = 8.5, units = "in", res= 400)
plot(gm_sp[[4]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5,,ylab="", xlab=suatgm, xlim=c(0,1), ylim=c(0,1)) 
abline(0,1, lty=2)
title(ylab=gmlab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(gm_sp[[1]][,3]~gm_sp[[3]][,3], pch=16, col=wa1, cex=2.5, ylab="")     
points(gm_sp[[2]][,3]~gm_sp[[3]][,3], pch=16, col=wa5, cex=2.5, ylab="")   
#dev.copy2pdf(file="output/presentations/gmscaled.pdf")
dev.off()

png(filename = "output/presentations/gmscaled2.png", width = 11, height = 8.5, units = "in", res= 400)
plot(gm_sp[[4]][,3]~gm_sp[[3]][,3], pch=16, col=wa3, cex=2.5,ylab="", xlab=suatgm, xlim=c(0,1), ylim=c(0,1)) 
abline(0,1, lty=2)
title(ylab=gmlab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(gm_sp[[1]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab="")     
points(gm_sp[[2]][,3]~gm_sp[[3]][,3], pch=16, col=wa5, cex=2.5, ylab="")  
#dev.copy2pdf(file="output/presentations/gmscaled2.pdf")
dev.off()

png(filename = "output/presentations/gmscaled3.png", width = 11, height = 8.5, units = "in", res= 400)
plot(gm_sp[[4]][,3]~gm_sp[[3]][,3], pch=16, col=wa3, cex=2.5,,ylab="", xlab=suatgm, xlim=c(0,1), ylim=c(0,1)) 
abline(0,1, lty=2)
title(ylab=gmlab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(gm_sp[[1]][,3]~gm_sp[[3]][,3], pch=16, col=wa1, cex=2.5, ylab="")     
points(gm_sp[[2]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab="") 
#dev.copy2pdf(file="output/presentations/gmscaled3.pdf")
dev.off() 

#Photosynthesis------------------------------------

png(filename = "output/presentations/Ascaled1.png", width = 11, height = 8.5, units = "in", res= 400)
plot(A_sp[[4]][,3]~A_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5, ylab="", xlab=suatA, xlim=c(0,25), ylim=c(0,25)) 
abline(0,1, lty=2)
title(ylab=satlab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(A_sp[[1]][,3]~A_sp[[3]][,3], pch=16, col=wa1, cex=2.5, ylab="")     
points(A_sp[[2]][,3]~A_sp[[3]][,3], pch=16, col=wa5, cex=2.5, ylab="")   
#dev.copy2pdf(file="output/presentations/Ascaled.pdf")
dev.off()

png(filename = "output/presentations/Ascaled2.png", width = 11, height = 8.5, units = "in", res= 400)
plot(A_sp[[4]][,3]~A_sp[[3]][,3], pch=16, col=wa3, cex=2.5,ylab="", xlab=suatA, xlim=c(0,25), ylim=c(0,25)) 
abline(0,1, lty=2)
title(ylab=satlab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(A_sp[[1]][,3]~A_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab="")     
points(A_sp[[2]][,3]~A_sp[[3]][,3], pch=16, col=wa5, cex=2.5, ylab="")  
#dev.copy2pdf(file="output/presentations/Ascaled2.pdf")
dev.off()

png(filename = "output/presentations/Ascaled3.png", width = 11, height = 8.5, units = "in", res= 400)
plot(A_sp[[4]][,3]~A_sp[[3]][,3], pch=16, col=wa3, cex=2.5,,ylab="", xlab=suatA, xlim=c(0,25), ylim=c(0,25)) 
abline(0,1, lty=2)
title(ylab=satlab, mgp=ypos1, cex=1.5)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03) 
points(A_sp[[1]][,3]~A_sp[[3]][,3], pch=16, col=wa1, cex=2.5, ylab="")     
points(A_sp[[2]][,3]~A_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab="") 
#dev.copy2pdf(file="output/presentations/Ascaled3.pdf")
dev.off() 

#plot objects----------------------------------------------------
pchs <- c(1, 16)
pch4 <- c(16,16, 1, 1)
cols <- c("blue","red")
col4 <- c("blue", "red", "blue", "red")


gmlab <-expression(g[m]~~(mol~m^-2~s^-1))
lmalab <- expression(LMA~~(g~m^-2))
masslab <- "Leaf Mass  (g)"
arealab <- expression(Leaf~Area~~(cm^2))
ypos <- c(2.5,1,0)
satlab <- expression(A~~(mu*mol~m^-2~s^-1))
condlab <- expression(g[s]~~(mol~m^-2~s^-1))
vpdlab <- "VPD  (kPa)"
ratelab <- expression(mol~m^-2~s^-1)
parlab <- expression(PPFD~~(mol~m^-2~s^-1))

leglab <-  c(expression(paste(AT, " " ,"sun")), expression(paste(ET, " " ,"sun")),
           expression(paste(AT," " ,"shade")), expression(paste(ET, " " ,"shade")))

leglab2<- c(expression(paste(Sun, "-" ,"AT")), expression(paste(Sun, "-" ,"ET")),
            expression(paste(Shade,"-" ,"AT")), expression(paste(Shade, "-" ,"ET")))

colaci <-c("forestgreen", "forestgreen", "yellowgreen", "yellowgreen")

cilab <- expression(C[i]~~(ppm))

##plot bits-------------------------------------------------------------------
#gm labels
suatgm <- expression(g[m]~~Sun~AT~~(mol~m^-2~s^-1))
suetgm <- expression(g[m]~~Sun~ET~~(mol~m^-2~s^-1))
shatgm <- expression(g[m]~~Shade~AT~~(mol~m^-2~s^-1))
shetgm <- expression(g[m]~~Shade~ET~~(mol~m^-2~s^-1))

#cc labels
suatcc <- expression(Cc~~Sun~AT~~(ppm))
suetcc <- expression(Cc~~Sun~ET~~(ppm))
shatcc <- expression(Cc~~Shade~AT~~(ppm))
shetcc <- expression(Cc~~Shade~ET~~(ppm))

#ci labels
suatci <- expression(Ci~~Sun~AT~~(ppm))
suetci <- expression(Ci~~Sun~ET~~(ppm))
shatci <- expression(Ci~~Shade~AT~~(ppm))
shetci <- expression(Ci~~Shade~ET~~(ppm))

#A labels
suatA <- expression(A~~Sun~AT~~(ppm))
suetA <- expression(A~~Sun~ET~~(ppm))
shatA <- expression(A~~Shade~AT~~(ppm))
shetA <- expression(A~~Shade~ET~~(ppm))


wa<- palette(wes.palette(5, "Zissou"))
paircol <- c(wa[3], wa[1],wa[5])

gmlab <-expression(g[m]~~(mol~m^-2~s^-1))
ypos <- c(2.5,1,0)

pairlab <-  c(expression(paste(vs~~Sun~ET)), 
              expression(paste(vs~~Shade~AT)),
             expression(paste(vs~~Shade~ET)))

satlab <- expression(A~~(mu*mol~m^-2~s^-1))

cilab <- expression(Ci~~(ppm))
cclab <- expression(Cc~~(ppm))
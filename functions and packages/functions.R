#chamber label function--------------------------------------------------------------------------------
chlab_func <- function(x){
  x$chamber <- ifelse(x$chamber <= 9, paste("0", x$chamber, sep=""), x$chamber)
  x$chamber <- paste("ch", x$chamber, sep="")
  return(x)
}

# standard error function--------------------------------------------------------------------------------
se <- function(x) sd(x)/sqrt(length(x))

#bar plot function--------------------------------------------------------------------------------

bar <- function(dv, factors, dataframe, percentage=FALSE, errbar=!percentage, half.errbar=TRUE, conf.level=.95, 
                xlab=NULL, ylab=NULL, main=NULL, names.arg=NULL, bar.col="black", whisker=.015,args.errbar=NULL,
                legend=TRUE, legend.text=NULL, args.legend=NULL,legend.border=FALSE, box=TRUE, args.yaxis=NULL, 
                mar=c(5,4,3,2),...){
  axes=!percentage
  dv.name<-substitute(dv)
  if(length(dv.name)>1) stop("'dv' only takes one variable")
  dv.name<-as.character(dv.name)
  dv<-dataframe[[dv.name]]
  fnames<-substitute(factors)
  if(length(fnames)==1){
    factors<-as.character(fnames)
    nf<-1
  }else{
    factors<-as.character(fnames[-1L])
    nf<-length(factors)
  }
  if(nf>2) stop("This function accepts no more than 2 factors \n",
                "\t-i.e., it only plots one-way or two-way designs.")
  if(percentage & errbar){
    warning("percentage=TRUE; error bars were not plotted")
    errbar<-FALSE
  }
  if(!percentage) xbars<-tapply(dv, dataframe[,factors], mean, na.rm=TRUE)
  else {
    xbars<-tapply(dv, list(interaction(dataframe[,factors], lex.order=TRUE)), mean, na.rm=TRUE)
    if(sum(na.omit(dv)!=0&na.omit(dv)!=1)>0) 
      stop("Data points in 'dv' need to be 0 or 1 in order to set 'percentage' to TRUE")
    xbars<-rbind(xbars, 1-xbars)*100
  }
  if(errbar){
    se<-tapply(dv, dataframe[,factors], sd, na.rm=TRUE)/sqrt(tapply(dv, dataframe[,factors], length))
    conf.level=1-(1-conf.level)/2
    lo.bar<-xbars-se*qnorm(conf.level)
    hi.bar<-xbars+se*qnorm(conf.level)	
  }
  extras<-list(...)
  if(legend & !percentage){
    if(is.null(legend.text))
      legend.text<-sort(unique(dataframe[[factors[1]]]))
    args.legend.temp<-list(x="topright", bty=if(!legend.border)"n" else "o",
                           inset=c(0,0))
    if(is.list(args.legend))
      args.legend<-modifyList(args.legend.temp, args.legend)
    else 
      args.legend<-args.legend.temp
  } else if(legend & percentage){
    if(is.null(legend.text)) 
      legend.text<-c("1", "0")
    args.legend.temp<-list(x="topright", bty=if(!legend.border)"n" else "o",
                           inset=c(0,0))
    if(is.list(args.legend))
      args.legend<-modifyList(args.legend.temp, args.legend)
    else 
      args.legend<-args.legend.temp
  } else if(!legend){
    args.legend<-NULL
    legend.text<-NULL
  }
  if(errbar && legend && !percentage) ymax<-max(hi.bar)+max(hi.bar)/20
  else if(errbar && legend && percentage) ymax<-115
  else if(errbar && !legend) ymax <- max(xbars)
  else if(!errbar && legend && percentage) ymax<-110	
  else if(!errbar) ymax<-max(xbars) + max(xbars)/20
  if(!percentage){
    args.barplot<-list(beside=TRUE, height=xbars, ylim=c(0, ymax), main=main, names.arg=names.arg,
                       col=hcl(h=seq(0,270, 270/(length(unique(dataframe[[factors[1]]]))))[-length(unique(dataframe[[factors[1]]]))]),
                       legend.text=legend.text, args.legend=args.legend, xpd=TRUE,
                       xlab=if(is.null(xlab)) factors[length(factors)] else xlab,
                       ylab=if(is.null(ylab)) dv.name else ylab, axes=axes)
  }else{
    args.barplot<-list(beside=TRUE, height=xbars, ylim=c(0, ymax),  main=main, names.arg=names.arg,
                       col=hcl(h=seq(0,270, 270/(length(unique(dataframe[[factors[1]]]))))[-length(unique(dataframe[[factors[1]]]))]),
                       legend.text=legend.text, args.legend=args.legend, xpd=TRUE,
                       xlab=if(is.null(xlab)) " "[length(factors)] else xlab,
                       ylab=if(is.null(ylab)) "percentage" else ylab, axes=axes)		
  }
  args.barplot<-modifyList(args.barplot, extras)
  errbars = function(xvals, cilo, cihi, whisker, nc, args.errbar = NULL, half.errbar=TRUE) {
    if(half.errbar){
      cilo<-(cihi+cilo)/2
    }
    fixedArgs.bar = list(matlines, x=list(xvals), 
                         y=lapply(split(as.data.frame(t(do.call("rbind", 
                                                                list(cihi, cilo)))),1:nc),matrix, 
                                  nrow=2, byrow=T))
    allArgs.bar = c(fixedArgs.bar, args.errbar)
    whisker.len = whisker*(par("usr")[2] - par("usr")[1])/2
    whiskers = rbind((xvals - whisker.len)[1,],
                     (xvals + whisker.len)[1,])
    fixedArgs.lo = list(matlines, x=list(whiskers), 	
                        y=lapply(split(as.data.frame(t(do.call("rbind", 
                                                               list(cilo, cilo)))), 1:nc), matrix, nrow=2, byrow=T))
    allArgs.bar.lo = c(fixedArgs.lo, args.errbar)
    fixedArgs.hi = list(matlines, x=list(whiskers), 
                        y=lapply(split(as.data.frame(t(do.call("rbind", 
                                                               list(cihi, cihi)))), 1:nc), matrix, nrow=2, byrow=T))
    allArgs.bar.hi = c(fixedArgs.hi, args.errbar)  
    invisible(do.call(mapply, allArgs.bar))
    if(!half.errbar) invisible(do.call(mapply, allArgs.bar.lo))
    invisible(do.call(mapply, allArgs.bar.hi))
  }
  par(mar=mar)
  errloc<-as.vector(do.call(barplot, args.barplot))
  if(errbar){
    errloc<-rbind(errloc, errloc)
    lo.bar<-matrix(as.vector(lo.bar))
    hi.bar<-matrix(as.vector(hi.bar))
    args.errbar.temp<-list(col=bar.col, lty=1)
    args.errbar<-if(is.null(args.errbar)|!is.list(args.errbar)) 
      args.errbar.temp
    else if(is.list(args.errbar)) 
      modifyList(args.errbar.temp, args.errbar)
    errbars(errloc, cilo=lo.bar, cihi=hi.bar, nc=1, whisker=whisker, 
            args.errbar=args.errbar, half.errbar=half.errbar)
  }
  if(box) box()
  if(percentage){
    args.yaxis.temp<-list(at=seq(0,100, 20), las=1)
    args.yaxis<-if(!is.list(args.yaxis)) args.yaxis.temp else modifyList(args.yaxis.temp, args.yaxis)
    do.call(axis, c(side=2, args.yaxis))
  }
}



#leafformat func------------------------------------------------
leafformat <- function(x) {
  x$Date <- as.Date(x$Date, format = "%d/%m/%Y")
  x$month <- month(x$Date, label=TRUE)
  x$year <- year(x$Date)
  x$wp <- with(x, ((water_potential/10)*-1))
  x <- merge(x, treat, all=TRUE)
  x$chamber <- as.character(x$chamber)
  x$drydown <- ifelse(x$month %in% c("Mar", "Apr") & x$chamber %in%c("ch01", "ch03", "ch04", "ch06", "ch08", "ch11"), 
                      "drought", "control")
  x$lma <- with(x, leaf_mass/leaf_area)
  x_ss <- subset(x, select= -c(Date, water_potential))
  monthorder<-order(x_ss$month, by=x_ss$chamber)
  x_ss <- x_ss[monthorder,]
  row.names(x_ss)<-NULL
  
  return(x_ss)
}


#gmdata format func--------------------------------------------------
gmformat <- function(df) {
  df <- merge(df, treat, all=TRUE)
  df$chamber <- as.character(df$chamber)
  df$ID <- paste(df$leaf, df$type, sep="-")
  df$ID <- as.factor(df$ID)
  df$drydown <- ifelse(df$month %in% c("march", "apr") & df$chamber %in%c("ch01", "ch03", "ch04", "ch06", "ch08", "ch11"), 
                       "drought", "control")
  return(df)
}

#par data format func---------------------------------------------------------------
parformat <- function(df) {
  df <- merge(df, treat, all=TRUE)
  df$chamber <- as.character(df$chamber)
  df$ID <- paste(df$leaf_type, df$par_type, sep="-")
  df$ID <- as.factor(df$ID)
  df$drydown <- ifelse(df$month %in% c("march", "apr") & df$chamber %in%c("ch01", "ch03", "ch04", "ch06", "ch08", "ch11"), 
                       "drought", "control")
  return(df)
}

#extract summary data---------------------------------------------------------------
getp <- function(x)x$p.value
getdiffmean <- function(m)m$estimate


#adds campaign Date and orders by ID and Date-------------------------------------------------------------
add_Month<- function(x){
  
  x$Month <-ifelse(x$campaign == 1, "Oct", x$campaign)
  x$Month <-ifelse(x$campaign == 2, "Dec", x$Month )
  x$Month <-ifelse(x$campaign == 3, "Jan", x$Month )
  x$Month <-ifelse(x$campaign == 4, "Feb", x$Month )
  x$Month <-ifelse(x$campaign == 5, "Mar", x$Month )
  x$Month <-ifelse(x$campaign == 6, "Apr", x$Month )
  return(x)
}


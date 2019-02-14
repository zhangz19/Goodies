

rm(list=ls())
options(scipen = 1)
library(gamlss)
library(gamlss.add)
require(arules)

load('demo.rda')

# Agresti 2002, P435: Kappa coefficients and Weighted Kappa
# "Mon Oct 03 02:14:44 2011" by Zhen Zhang
# kappa is for nomial while weighted kappa for ordinal
# weights = 2: interpretable as intra-class correlation coefficient
# weights = 1: directly related to Cicchetti's C statistics (Cicchetti 1972)
myKappa <- function(dat, weights = 2){
  n <- sum(dat); Pd <- diag(dat)/n; Pc <- colSums(dat)/n; Pr <- rowSums(dat)/n
  P0 <- sum(Pd); Pe <- sum(Pc * Pr)
  kappa <- (P0 - Pe) / (1 - Pe) 
  tsum <- 0
  for(a in 1:(r <- nrow(dat))){
    for(b in 1:r){ tsum <- tsum + dat[a,b]/n*(Pr[b] + Pc[a])^2 }
  }
  kappa.var <- 1/n*(P0*(1-P0)/(1-Pe)^2 + 2*(1-P0)*(2*P0*Pe - sum(Pd*(Pc + Pr)))/(1-Pe)^3 +
                      (1-P0)^2*(tsum - 4*Pe^2)/(1-Pe)^4)
  kappa.std <- sqrt(kappa.var)
  
  weighted.agree <- s1 <- 0; Wmat <- matrix(0,r,r)
  for(a in 1:r){
    for(b in 1:r){ 
      if(weights == 1) w <- 1 - abs(a-b)/(r-1)
      if(weights == 2) w <- 1 - (a-b)^2/(r-1)^2 # squared weight suggested by Fleiss, Cohen (1973)
      weighted.agree <- weighted.agree + w*dat[a,b]/n
      s1 <- s1 + w*Pc[a]*Pr[b] 
      Wmat[a,b] <- w
    }
  }
  weighted.kappa <- (weighted.agree - s1) / (1 - s1) # Cohen (1968) Weighted kappa
  
  # now calculate the large sample standard error of weighted kappa
  # Fleiss, Cohen & Everitt, 1969 
  Widot <-  as.vector(Wmat %*% Pc); Wdotj <- as.vector(Wmat %*% Pr)
  Widotmat <- matrix(rep(Widot, r), ncol=r)
  Wdotjmat <- matrix(rep(Wdotj, r), ncol=r, byrow=T)
  A <- (1+s1) - sum(dat/n * Wmat * (Widotmat + Wdotjmat))
  B <- (1+s1)^2 - sum(dat/n * (Widotmat + Wdotjmat)^2)
  C <- 1 - sum(dat/n * Wmat^2)
  wkappa.var <- 1/(n*(1-s1)^2)*(2*A*(1-weighted.kappa) - B*(1-weighted.kappa)^2 - C)
  wkappa.std <- sqrt(wkappa.var)
  
  out <- as.data.frame(cbind(P0, kappa, kappa.std, kappa-kappa.std*qnorm(.975), kappa+kappa.std*qnorm(.975), 
                             weighted.agree, weighted.kappa, weighted.kappa-wkappa.std*qnorm(.975), weighted.kappa+wkappa.std*qnorm(.975) )); row.names(out) <- ''
  names(out) <- c('agreement','kappa','stdev','lower','upper','weighted.agreement', 'weighted.kappa','w.lower','w.upper')
  return(out)
}


#++++++ create 3D contour plot based on gamlss interpolated results
nscen <- length(uscen <- levels(dat$scen))
J <- length(cols <- c('#1a9850','#66bd63','#a6d96a','#d9ef8b','#fed976','#feb24c','#fd8d3c','#e31a1c')) 
#get it here: http://colorbrewer2.org/

pchs <- c(21,24,22,23,25,8,9,4,3)
cexs <- rep(1,J); cexs[pchs==21] <- 1.3

cuts <- log(cuts0 <- c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 0.75, 10, 62))
cuts[1] <- -1e4; cuts[length(cuts)] <- 1e4  # as predicted values may exceed the oberved range

#cuts <- discretize(dat$y[dat$type=='subtype1'], method="frequency", categories=J, onlycuts=TRUE)

### you could try with random effects, but seems have prediction issue
#mat <- dat[dat$type=='subtype1',]
#m0 <- gamlss(y ~ ga(~s(x1, x2)) + random(site), family=NO(), data=mat)

saveDir <- './'
p1 <- mats <- as.list(rep(NA, nscen))
logConst <- numeric(nscen)
type <- 'subtype1'
for(s in 1:nscen){  #for each scenario
  inds <- which(dat$test==type & dat$scen==uscen[s])
  mat <- droplevels(dat[inds,])
  
  # for subtype1, take average for each x1 and x2
  if(type=='subtype1'){
    grp <- paste(mat$x1, mat$x2, sep='-');  grp <- factor(grp,levels=unique(grp))
    mat <- aggregate(mat$pec, list(grp),mean, na.rm=T)
    mat[,1] <- as.character(mat[,1])
    tmp <- unlist(regexec('-',mat[,1]))
    nstop <- max(nchar(mat[,1]))+1
    mat$x1 <- as.numeric(substr(mat[,1],1,tmp-1))
    mat$x2 <- as.numeric(substr(mat[,1],tmp+1,nstop))
    mat <- mat[,-1]; names(mat)[1] <- 'y'
  }
  
  y0 <- mat$y; ind1 <- which(y0==0)
  if(length(ind1)) { logConst[s] <- min(y0[-ind1])/2; y0[ind1] <- logConst[s] }  #imput
  mat$y <- log10(y0)
  mat <- mat[,c('x1','x2','y')]
  
  a <- gamlss(y ~ ga(~s(log10(x1), log10(x2))), family = NO(), data = mat)
  mat$predY <- fitted(a)
  p1[[s]] <- a
  mats[[s]] <- mat
  logConst0 <- logConst[s]
  model <- p1[[s]]
}


# graphics
png(paste('./tex/c9comp.png',sep=''),units='in', height=6.4,width=6.6, pointsize=12, res=400)

tab <- matrix(NA,9,3)
m <- matrix(c(1:9,10,10,10),nrow = 4,ncol = 3,byrow = TRUE)
layout(mat = m, heights = c(rep(.32,3),1-(.32*3)))
par(oma = c(0, 0, 0, 0))
par(mar=c(2,2,1,0)+.2, mgp=c(1.2,.3,0), tck=-0.01, cex.axis=1, cex.lab=1.2, cex.main=1, xpd=T) 
for(s in 1:nsite){
  print(s)
  inds <- which(dat$type=='ptest' & dat$site==usite[s])
  mat <- dat[inds,]
  # for p-test, take average for each x1 and x2
  grp <- paste(mat$x1, mat$x2, sep='-'); grp <- factor(grp,levels=unique(grp))
  mat <- aggregate(mat$y, list(grp),mean, na.rm=T)
  mat[,1] <- as.character(mat[,1])
  tmp <- unlist(regexec('-',mat[,1]))
  nstop <- max(nchar(mat[,1]))+1
  mat$x1 <- as.numeric(substr(mat[,1],1,tmp-1))
  mat$x2 <- as.numeric(substr(mat[,1],tmp+1,nstop))
  mat <- mat[,-1]; names(mat)[1] <- 'y'
  y0 <- mat$y; ind1 <- which(y0==0)
  if(length(ind1)) { y0[ind1] <- logConst[s] } 
  mat$y <- log(y0)
  
  len <- 1e2
  x1s <- seq(min(mat$x1)-.5, max(mat$x1)+20, length=len)
  x2s <- seq(min(mat$x2)-.5, max(mat$x2)+25, length=len)
  xy <- expand.grid(x1s, x2s)
  names(xy) <- c('x1','x2')
  
  if(s==1) yhat <- matrix(NA, len*len, nsite)
  yhat[,s] <- predict(p1[[s]], newdata=xy)
  
  par(mar=c(1.5,1.5,.5,.5)+.5,mgp=c(1,.3,0), tck=-0.01, cex.axis=1, cex.lab=1, cex.main=1.1)
  image(x=x1s, y=x2s, z=matrix(yhat[,s],len), col=cols, breaks=cuts,main=nams[s], xlab='x1', ylab='x2')   
  # or like terrain.colors(50)
  contour(x=x2s, y=x1s, z=matrix(yhat[,s],len), add=T,levels=cuts,labcex =.7, main='',
          labels=round(exp(cuts),3),method='flattest',col='blue')
  points(mat$x1, mat$x2,pch=16,cex=.9)
  
  inds <- which(mat$x1>=100&mat$x2>=100)
  y <- as.integer(discretize(mat$y[inds], method="fixed", categories=cuts))
  y1 <- as.integer(discretize(fitted(p1[[s]])[inds], method="fixed", categories=cuts))
  tmp <- table(y, y1) #confusion matrix
  wKappa <- myKappa(tmp, weights = 2)
  tab[s,] <- c(wKappa$agreement, wKappa$kappa, wKappa$weighted.kappa)
  legend(x=715.3,y=-50, 
         legend=substitute( paste(delta,a1),list(a1=paste(" = ",100*round(wKappa$agreement,2),'%',sep='')) ),
         bty='n',text.col='red',text.font=3)
  indE <- which(y != y1)
  points(mat$x1[indE], mat$x2[indE],pch=22,cex=2.5,col='red')
  print(sum( (y[indE]==4&y1[indE]==5)|(y[indE]==5&y1[indE]==4)))
  
  points(mat$x1[inds], mat$x2[inds],pch=pchs[y],cex=.2+cexs[y]) # bg='black',
  lines(c(0,100), c(100,100),col='black',lwd=2)
  lines(c(100,100), c(0,100),col='black',lwd=2)
  lines(c(0,100), c(0,0),col='black',lwd=2)
  lines(c(0,0), c(0,100),col='black',lwd=2)
}

#labs <- gsub(' ','',levels(y)); labs <- gsub(',',', ',labs); labs[1] <- '<0.01'; labs[length(labs)] <- '>=10'
labs <- c('<0.001','[0.001, 0.01)  ','[0.01, 0.05)','[0.05, 0.1)','[0.1, 0.5)','[0.5, 0.75)','[0.75, 10)','>=10')
par(mar=c(0,0,0,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
foo <- legend('top',legend=labs, bty='n', pch=pchs, pt.cex=1.4,
              col='black', text.col='blue', inset=-.05,horiz = T, cex=.9)   #pt.bg=cols, 
points(foo$text$x, foo$text$y*.55, pch='.', cex=25, col=cols)
dev.off()


#++++++++++++++ more examples using layout
# visualize missing pattern
png(paste('./miss.png',sep=''),units='in', height=4.4,width=6.8, pointsize=12, res=400)

nrows <- 1; ncols <- 5
m <- matrix(c(1:(ncols*nrows),rep(ncols*nrows+1,ncols)),nrow=nrows+1,ncol = ncols,byrow = TRUE)
m0 <- 0.92/nrows; layout(mat=m, heights = c(rep(m0,nrows),1-(m0*nrows)))

par(oma=c(.2,2,.5,.5), mar=c(4.6,1.4,2,0),cex.axis=.8, cex.main=1)  
for(i in 1:5){
  cols <- c('gray70','white')
  x <- is.na(dat2[which(dat2$Stage==lev[i])[1:min(1e7,sum(dat2$Stage==lev[i]))],-1])
  n <- nrow(x); p <- ncol(x)
  par(mgp=c(.7,.2,0), tck=-0.02,xpd=F)
  image(x=1:p, y=1:n, z=t(x), col=cols,xlab='',ylab='',xaxt='n',yaxt='s', main=lev[i])
  par(mgp=c(.7,100,0), tck=-0.02)
  axis(side=1, at=1:p, labels=FALSE)
  fac <- 0.1; if(i==5) fac <- .08
  text(x=1:p,y=rep(-nrow(x)*fac,p),labels=c(names(dat2)[-1]),srt=60,xpd=T,cex=.9)
  abline(v=.5+c(1:p),col='gray50',lty=1)
}
par(mar=c(0,0,0,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend('center',legend=c('Present                ','Missing'), bty='n', pch=22, pt.cex=3, pt.bg=cols, col='black', text.col='black',inset=-0.0,horiz = T,cex=1.1)
mtext(c('Observations'), side=2, line=c(.14), outer=TRUE, cex=.8)

dev.off()


# not run



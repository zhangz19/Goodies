


library(lattice)
library(latticeExtra)   # For useful add-ons to lattice
library(Hmisc)          # For xYplot() and Dotplot()

# lattice.options(default.theme = canonical.theme(color = F))
my.padding <- list(top.padding = 1, main.key.padding = 0, key.axis.padding = 0,
                   axis.ylab.padding = 0, ylab.key.padding  = 0, right.padding = 1,
                   left.padding = 1, key.sub.padding = 0, bottom.padding = 1,
                   axis.right = 0, key.right = 0)

Fig4 <- xyplot(reduction_outlet ~ distance|BMP*Condition, data=mat, pch=16, 
               scales = list(y = list(relation = "free", draw=F), x = list(draw=F)), xlab='Distance (m)', ylab='Outlet Reduction',
               par.settings = list(layout.heights = my.padding,
                                   layout.widths = my.padding),
               panel=function(x,y){
                 panel.xyplot(x,y, cex=.8, pch=16, col='gray40')
                 # panel.abline(h=min(y), col='gray40', lty=3)
                 cols <- c('blue', 'cyan', 'green3', 'red'); aics <- numeric(4); Xs <- Ys <- numeric(4)
                 
                 pa <- coef(fit <- lm(y~x+I(x^2))); 
                 mods <- function(x) return(pa[1]+x*pa[2]+x^2*pa[3]); 
                 panel.curve(mods,from=min(x),to=max(x), add=T, col=cols[1], lty=1); aics[1] <- AIC(fit)
                 tmp <- optimize(mods, interval=range(x), maximum=TRUE)
                 Xs[1] <- tmp$maximum; Ys[1] <- tmp$objective
                 
                 pa <- coef(fit <- lm(y~x+I(x^2)+I(x^3)))
                 mods <- function(x) return(pa[1]+x*pa[2]+x^2*pa[3]+x^3*pa[4]) 
                 panel.curve(mods, from=min(x),to=max(x),add=T, col=cols[2], lty=1); aics[2] <- AIC(fit)
                 tmp <- optimize(mods, interval=range(x), maximum=TRUE)
                 Xs[2] <- tmp$maximum; Ys[2] <- tmp$objective
                 
                 pa <- coef(fit <- lm(y~x+I(x^2)+I(x^3)+I(x^4))); 
                 mods <- function(x) return(pa[1]+x*pa[2]+x^2*pa[3]+x^3*pa[4]+x^4*pa[5]) 
                 panel.curve(mods, from=min(x),to=max(x),add=T, col=cols[3], lty=1); aics[3] <- AIC(fit)
                 tmp <- optimize(mods, interval=range(x), maximum=TRUE)
                 Xs[3] <- tmp$maximum; Ys[3] <- tmp$objective
                 
                 pa <- coef(fit <- lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5))); 
                 mods <- function(x) return(pa[1]+x*pa[2]+x^2*pa[3]+x^3*pa[4]+x^4*pa[5]+x^5*pa[6]) 
                 panel.curve(mods, from=min(x),to=max(x),add=T, col=cols[4], lty=1); aics[4] <- AIC(fit)
                 tmp <- optimize(mods, interval=range(x), maximum=TRUE)
                 Xs[4] <- tmp$maximum; Ys[4] <- tmp$objective
                 
                 indm <- which(aics == min(aics))
                 # panel.text(max(x)-2.3,max(y),round(aics[indm],2),col=cols[indm],cex=.9)
                 if(indm == 4)
                   panel.text(max(x)-0.4,max(y),paste("order =",indm+1),col=cols[indm],cex=.9)
                 if(indm != 4)
                   panel.text(max(x)-2.3,max(y),paste("order =",indm+1),col=cols[indm],cex=.9)
                 panel.lines(x=rep(Xs[indm],2), y=c(min(y),Ys[indm]), lty=2, col=cols[indm])
                 panel.text(Xs[indm]+.7,min(y)-10,round(exp(Xs[indm]),2),adj=1,col=cols[indm],cex=.9)
               })


#jpeg(paste('trellisPlotDistance.jpg',sep=''),quality=100, height=4200,width=5600, pointsize=20, res=600)
tiff(paste('Figure8.tif',sep=''),height=4000,width=6000, pointsize=16, res=600)

Fig8 <- useOuterStrips(Fig4, 
                       strip = strip.custom(strip.names = c(T,T)), 
                       strip.left = strip.custom(horizontal = F, strip.names = c(T,T)))
print(Fig8)

dev.off()




# not run


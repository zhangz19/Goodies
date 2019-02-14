

require('ggplot2')

#+++++++++++++++++++++ example 1: monitor predicted vs observed values
theme_expd <- function(base_size = 18, position="bottom",anglesize=0){
  theme(panel.background = element_rect(fill='white', colour='black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        # axis.ticks=element_blank(), 
        # axis.text.x=element_blank(), 
        # axis.text.y=element_blank(),
        # axis.title.x=element_blank(), 
        # axis.title.y=element_blank(),
        axis.text=element_text(size=base_size*1.25),
        axis.title=element_text(size=base_size*1.5,face="bold"),
        strip.text.x=element_text(size = base_size,colour ="black"),
        strip.text.y=element_text(size = base_size,colour ="black", angle = 0),
        #strip.background = element_blank(),
        strip.background = element_rect(fill="grey95"),
        #axis.title = element_text(size = base_size,colour ="black"),
        plot.title=element_text(size = base_size,colour ="black"),
        #strip.background = element_blank(),
        legend.text=element_text(size = base_size*1.5,colour ="black"),
        legend.title=element_text(size = base_size*1.5,colour ="black"),
        legend.key = element_blank(),
        legend.position= position,                       
        legend.key.width=unit(1,"line"))
}

ggplot(data=dat, aes(x=x_DAFA)) +
  geom_point(size=4,aes(y=y_t,color='observed', shape='observed'))+
  geom_point(size=4,aes(y=y_hat,color='predicted', shape='predicted'))+
  facet_wrap( ~ID, ncol=4, scales="free_x")+ #
  xlab("DAFA")+
  ylab('Residue mass')+
  scale_colour_manual(name="", values = c("observed" = "black", "predicted" = "red")) +
  scale_shape_manual(name="", values = c("observed" = 16, "predicted" = 1))+
  theme_expd()#+





#+++++++++++++++++++++ example 2: randomized block design
ggplot(ft, aes(x=FLAT_ROW, y=FLAT_RANGE, label=rand))+
  geom_tile(aes(fill=rep))+
  facet_wrap(~TABLE_label+FLAT_label, nrow=2)+
  geom_text(size=2.5)+
  labs(fill = "Rep", scale_colour_discrete(name="Diamonds"))+
  theme(axis.ticks=element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  scale_y_continuous(trans = "reverse", breaks = unique(ft$FLAT_RANGE))


library(ggplot2)
library(ggsci)
library(DT)
library(RColorBrewer)

colorcount <- length(unique(dat$col))
getPalette <- colorRampPalette(brewer.pal(9,'Set1'))
ggplot(dat, aes(x=FLAT_ROW, y=FLAT_RANGE, label=sro)) + 
  geom_tile(aes(fill=col, width=.95, height=.98)) + 
  # facet_wrap(~TABLE_label + FLAT_label, nrow = 2) + 
  facet_wrap(~FLAT_label+TRT_label, nrow = 2) + 
  geom_text(size = 6) + 
  labs(fill=input$col, scale_colour_discrete(name="Diamonds")) +   #"Maturity"
  theme_expd() + 
  #scale_fill_jco(alpha = al)+
  scale_fill_manual(values=getPalette(colorcount))+
  scale_y_continuous(trans = "reverse", breaks = unique(dat$FLAT_RANGE))




#+++++++++++++++++++++ example 3: Huron lake white fish movement
require(ggmap)
# baseMap <- get_map(location=c(lon= -82.398304, lat=44.763368), zoom=7, maptype='terrain')
# baseMap <- get_map(location=c(lon= -82.398304, lat=44.763368), zoom=7, maptype='hybrid')
baseMap <- get_googlemap(center=c(lon= -82.398304, lat=44.763368), zoom=7, maptype='hybrid', scale=2, style = 'feature:road|element:labels|visibility:off')
# baseMap <- get_map(location=c(lon= -82.398304, lat=44.763368), zoom=7, maptype='terrain-background')
# baseMap <- get_map(location=c(lon= -82.398304, lat=44.763368), zoom=7, maptype='satellite')
# baseMap <- get_map(location=c(lon= -82.398304, lat=44.763368), zoom=7, maptype='toner', color='bw', source='stamen')

map1 <- ggmap(baseMap, padding=0) + theme_bw() + labs(x='Longitude', y='Latitude') +
  coord_fixed(xlim=range(tmp$tag_long)+c(-.87, .35), ylim=range(tmp$tag_lat)+c(-.12, .2))

print(map1)

labs <- c('Alpena','Burnt Island','Cheboygan', 'Detour','Fishing Islands','Saginaw Bay', 'Sarnia')
colL <- adjustcolor(col, alpha.f=0.1) #lighter version of col for filled background
# I don't know for upper 3 colors the transparency level doesn't look right in the ggmap
colL[2:4] <- adjustcolor(col[2:4], alpha.f=0.009)

# # with separate legend version
# map1 + 
#   geom_point(data=tmp, aes(x=tag_long, y=tag_lat, col=tag_site_nam, fill=tag_site_nam), shape=21, cex=3.5) + 
#   theme(legend.position = "right") + 
#   scale_color_manual(name=element_blank(), labels=labs, values=col) + 
#   scale_fill_manual(name=element_blank(), labels=labs, values=colL) + 
#   theme(text=element_text(size = 10))

# setEPS()
# postscript("LH_new.eps",  width = 4.4, height = 3.4, pointsize=1)

install.packages('ggsn')
require(ggsn) #for scale bar and north


pdf("LH_new.pdf",  width = 4.4, height = 3.4, pointsize=1)

# use text, with no legend version
cdata1 <- cdata
cdata1$labs <- labs
cdata1$long <- cdata$long - c(.12,.8,.15,.057, 1.4, .71, .9) + .05
cdata1$lat <- cdata$lat + c(.08,.12,-.1,0, -.2, -.22,0)
i <- as.integer(tmp$tag_site_nam)
windowsFonts(Times=windowsFont("TT Times New Roman"))

tmp1 <- tmp[,c('tag_long','tag_lat')]; names(tmp1) <- c('long','lat')  #for ggsn package

source('myscalebar.R')
map1 + 
  geom_point(data=tmp, aes(x=tag_long, y=tag_lat), col=col[i], shape=21, bg=colL[i], cex=3.5) + 
  geom_text(data=cdata1, aes(x=long, y=lat, label=labs), size=4, vjust=0, hjust=-0.5, col='white', family = "Times", fontface="bold.italic") +   #, fontface = "bold", italic
  # scale_color_manual(name=element_blank(), labels=labs, values=col) + 
  # scale_fill_manual(name=element_blank(), labels=labs, values=colL) + 
  # scaleBar(lon = -84.5, lat = 43.2, distanceLon = 200, distanceLat = 100, distanceLegend = 200, dist.unit = "km", orientation = FALSE) + 
  myscalebar(tmp1, location="bottomleft", anchor=c(x=-85.1, y=43.1), dist=40, dd2km=T, model='WGS84', st.size=2.5, st.dist=.022) + mynorth(tmp1, location="bottomleft", anchor=c(x=-84.76, y=43.2), scale = 0.1, symbol = 1) + 
  theme(text=element_text(size = 10))

# # verify
# require(geosphere) # for distGeo
# lon1 <- -85; lat1 <- 43.1;  lon2 <- -84.50688; lat2 <- 43.09894
# distGeo(c(lon1, lat1), c(lon2, lat2))/1e3  #10^3 km

dev.off()





#+++++++++++++++++++++ example 4: spatial plot
require(ggplot2)
require(ggmap)
source('myscalebar.R')

# baseMap2 = get_map(location=c(lon=119.4778, lat=32.32867), zoom=6, maptype="satellite")
# baseMap2 = get_map(location=c(lon=119.4778, lat=32.32867), zoom=6, maptype="terrain-background")
# baseMap = get_map(location=c(lon=119.4778, lat=32.32867), zoom=6, maptype="terrain")
# save(file='baseMap', baseMap, baseMap2)
load('baseMap')

map1 = ggmap(baseMap, padding=0) + theme_bw() + 
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle(paste('Non-spatial (Nash-Sutcliffe=', nseNS, ')',sep='')) + 
  coord_fixed(xlim = range(coordsB[,1])+c(0, .6), ylim = range(coordsB[,2])+c(0, .3), ratio = 1/1.2) #xlim = c(-122.55, -122.40),
b <- as.data.frame(coords)
b$`Location effect` <- as.integer(y1)

# Example taken from: 
# http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
# may not work well when saving it
# pos <- (cuts-min(cuts))/(max(cuts)-min(cuts))*(length(labs0)+1)
p0 <- map1+
  geom_point(data=b, aes(x=long, y=lat, colour=`Location effect`), pch=21, bg=cols[y1], alpha = 1, size=2.5) +
  scale_colour_gradientn(colours=cols,breaks=c(1:6)+.4, labels=labs0) +  #c(1:6)+.4 cuts[2:(length(cuts)-1)]
  # scale_colour_gradientn(colours=cols,breaks=pos[2:(length(labs0)+1)], labels=labs0) + 
  theme(legend.position="right") #+ 
#theme(text=element_text(size=4))

source('multiplot.R')
# multiplot(p1, p1, cols=2)

mylegend <- g_legend(p0) 
dev.off()

# pdf(paste('./tex/legend.pdf',sep=''),height=2.6,width=1.6, pointsize=1)
png(paste('./tex/legend.png',sep=''), units='in', height=2.6,width=1.4, pointsize=12, res=600)
grid.draw(mylegend) 
dev.off()

## size is hard to control for this way
# pdf(paste('./test.pdf',sep=''),height=4,width=7, pointsize=1)
# grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
#                                p1 + theme(legend.position="none"),
#                                nrow=1),
#                    mylegend, nrow=1,widths=c(19,19, 1))
# dev.off()
# ggsave('./tex/test.pdf', width=4)

# save separately
# pdf(paste('./tex/p1.pdf',sep=''),height=6.3,width=6, pointsize=1)
png(paste('./tex/p1.png',sep=''),units='in', height=6.3,width=6, pointsize=11, res=600) 
# colsL <- adjustcolor(cols, alpha.f = 0.0)
b <- as.data.frame(coords)
b$`Location effect` <- as.integer(y1)
map1+ 
  geom_point(data=b, aes(x=long, y=lat, colour=`Location effect`), pch=21, bg=cols[y1], alpha = .8, size=6) + scale_colour_gradientn(colours=cols,breaks=c(1:7), labels=labs) + 
  myscalebar(b, location="topright", anchor=c(x=121.82, y=34.2), dist=50, dd2km=T, model='WGS84', st.size=4, st.dist=.022) + 
  mynorth(b, location="topright", anchor=c(x=121.5, y=35), scale=0.1, symbol = 1) +
  theme(legend.position="none", text=element_text(size=18))
dev.off()


# pdf(paste('./tex/p2.pdf',sep=''),height=6.3,width=6, pointsize=1)
png(paste('./tex/p2.png',sep=''),units='in', height=6.3,width=6, pointsize=11, res=600) 
# colsL <- adjustcolor(cols, alpha.f = 0.0)
b <- as.data.frame(coords)
b$`Location effect` <- as.integer(y2)
b1 <- as.data.frame(coords1)
b1$`Location effect` <- as.integer(y3)
map2 <- ggmap(baseMap2, padding=0) + theme_bw()+ 
  labs(x = "Longitude", y = "Latitude") + 
  ggtitle(paste('Spatial (Nash-Sutcliffe=', nseSP, ')',sep='')) + 
  coord_fixed(xlim = range(coordsB[,1])+c(0, .6), ylim = range(coordsB[,2])+c(0, .3), ratio = 1/1.2) 
map2+ 
  geom_point(data=b1, aes(x=long, y=lat, colour=`Location effect`), pch=15, bg=cols[y3], alpha = .4, size=3.5) +
  geom_point(data=b, aes(x=long, y=lat, colour=`Location effect`), pch=21, bg=cols[y2], alpha = .9, size=6) + 
  scale_colour_gradientn(colours=cols,breaks=c(1:7), labels=labs) + theme(legend.position="none") + theme(text=element_text(size=18)) + 
  myscalebar(b, location="topright", anchor=c(x=121.82, y=34.2), dist=50, dd2km=T, model='WGS84', st.size=4, st.dist=.022) + 
  mynorth(b, location="topright", anchor=c(x=121.5, y=35), scale = 0.1, symbol = 1)
dev.off()


# not run



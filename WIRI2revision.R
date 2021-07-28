library(bipartite)
library(tidyverse)
library(ggplot2)
library(viridis)
library(ggalt)

#import overall data, transpose matrix, drop behavioral group
birdsites<-read.csv("WIRI2matrix.csv", row.names=1)
totalmatrix<-t(birdsites[,-1])

birdsites1<-birdsites
birdsites1$code<-row.names(birdsites)
birdsites1

elton<-read.csv("wirielton.csv")

elton1<-full_join(birdsites1, elton)
rownames(elton1)<-elton1$code
elton1

#characterize species-level indices and add to object
totalspeciesoutput<-specieslevel(totalmatrix)
#pull out lower level (bryophyte) data into object
totalspeciesoutput1<-totalspeciesoutput$`lower level`
#look at network-level indices
networklevel(totalmatrix)
#view bryophyte indices
totalspeciesoutput1
#assign table of indices a species variable to manipulate for plotting
totalspeciesoutput1$species<-rownames(totalspeciesoutput1)
#order table by value of d
totalspeciesoutput1<-arrange(totalspeciesoutput1, d)
totalspeciesoutput1$species<-as.character(totalspeciesoutput1$species)
totalspeciesoutput1$species <- factor(totalspeciesoutput1$species, levels=unique(totalspeciesoutput1$species))
#create and add colors to table
totalcolors<-viridis(29)
totalspeciesoutput1$color<-totalcolors




#plot species specific relationships
pdf("figures/web.pdf", paper="a4r")
par(omi=c(1,1,1,1))
plotweb(totalmatrix, adj.low = c(0, 3), labsize = 1.2)
dev.off()
plotweb(totalmatrix, labsize= 1.5, adj.high = text(c(0, 3), par(font = 2)), y.lim = c(0,2))

#plot intteraction matrix heatmap
pdf("figures/heatmap.pdf", paper="a4r")
par(omi=c(1,1,1,1))
visweb(totalmatrix, labsize = 2)
dev.off()
visweb(totalmatrix, labsize = 2)


colororder<-totalspeciesoutput1[,21:22]
colororder

#removed color scale color = totalspeciesoutput1$color
#plot d' values for all bryo species
totalplot<-ggplot(data= totalspeciesoutput1, aes(d, species)) +
  geom_point(cex = 3, shape = 21, fill = "#E69F00" ) +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=8),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    plot.title = element_text(size = 20))+
  ggtitle("")+
  xlab("")+
  ylab("")

totalplot

#####subset by elton foraging traits####

groundmatrix<-subset(elton1, elton1$ForStrat.ground > 20)
groundmatrix<-t(groundmatrix[,c(-1,-30:-39)])
groundmatrix

understorymatrix<-subset(elton1, elton1$ForStrat.understory > 20)
understorymatrix<-t(understorymatrix[,c(-1,-30:-39)])

midhighmatrix<-subset(elton1, elton1$ForStrat.midhigh > 20)
midhighmatrix<-t(midhighmatrix[,c(-1,-30:-39)])

canopymatrix<-subset(elton1, elton1$ForStrat.canopy > 20)
canopymatrix<-t(canopymatrix[,c(-1,-30:-39)])

airmatrix<-subset(elton1, elton1$ForStrat.aerial > 20)
airmatrix<-t(airmatrix[,c(-1,-30:-39)])
airmatrix

#Aerial foraging strategy too depauperate to continue to analyze

####ground####
plotweb(groundmatrix)
visweb(groundmatrix)
#characterize species-level indices and add to object
groundspeciesoutput<-specieslevel(groundmatrix)
#pull out lower level (bryophyte) data into object
groundspeciesoutput1<-groundspeciesoutput$`lower level`
#look at network-level indices
networklevel(groundmatrix)
#view bryophyte indices
groundspeciesoutput1
#assign table of indices a species variable to manipulate for plotting
groundspeciesoutput1$species<-rownames(groundspeciesoutput1)
groundspeciesoutput1<-arrange(groundspeciesoutput1, d)
groundspeciesoutput1$species<-as.character(groundspeciesoutput1$species)
groundspeciesoutput1$species <- factor(groundspeciesoutput1$species, levels=unique(groundspeciesoutput1$species))

groundcolor<-inner_join(groundspeciesoutput1, colororder, by = "species")
groundcolor


#plot d' values for all bryo species
groundplot<-ggplot(data=groundspeciesoutput1, aes(species, d)) +
  geom_point(cex = 3, color = groundcolor$color) +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=8),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank(),
    plot.title = element_text(size = 20))+
  ggtitle("Ground Foragers")+
  xlab("")+
  ylab("d'")

groundplot


####understory####
plotweb(understorymatrix)
visweb(understorymatrix)
#characterize species-level indices and add to object
understoryspeciesoutput<-specieslevel(understorymatrix)
#pull out lower level (bryophyte) data into object
understoryspeciesoutput1<-understoryspeciesoutput$`lower level`
#look at network-level indices
networklevel(understorymatrix)
#view bryophyte indices
understoryspeciesoutput1
#assign table of indices a species variable to manipulate for plotting
understoryspeciesoutput1$species<-rownames(understoryspeciesoutput1)
understoryspeciesoutput1<-arrange(understoryspeciesoutput1, d)
understoryspeciesoutput1$species<-as.character(understoryspeciesoutput1$species)
understoryspeciesoutput1$species <- factor(understoryspeciesoutput1$species, levels=unique(understoryspeciesoutput1$species))

understorycolor<-inner_join(understoryspeciesoutput1, colororder, by = "species")
understorycolor


#plot d' values for all bryo species
understoryplot<-ggplot(data=understoryspeciesoutput1, aes(species, d)) +
  geom_point(cex = 3, color = understorycolor$color) +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=8),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank(),
    plot.title = element_text(size = 20))+
  ggtitle("Understory Foragers")+
  xlab("")+
  ylab("d'")

understoryplot


####midhigh####
plotweb(midhighmatrix)
visweb(midhighmatrix)
#characterize species-level indices and add to object
midhighspeciesoutput<-specieslevel(midhighmatrix)
#pull out lower level (bryophyte) data into object
midhighspeciesoutput1<-midhighspeciesoutput$`lower level`
#look at network-level indices
networklevel(midhighmatrix)
#view bryophyte indices
midhighspeciesoutput1
#assign table of indices a species variable to manipulate for plotting
midhighspeciesoutput1$species<-rownames(midhighspeciesoutput1)
midhighspeciesoutput1<-arrange(midhighspeciesoutput1, d)
midhighspeciesoutput1$species<-as.character(midhighspeciesoutput1$species)
midhighspeciesoutput1$species <- factor(midhighspeciesoutput1$species, levels=unique(midhighspeciesoutput1$species))

midhighcolor<-inner_join(midhighspeciesoutput1, colororder, by = "species")
midhighcolor


#plot d' values for all bryo species
midhighplot<-ggplot(data=midhighspeciesoutput1, aes(species, d)) +
  geom_point(cex = 3, color = midhighcolor$color) +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=8),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank(),
    plot.title = element_text(size = 20))+
  ggtitle("Mid-high Foragers")+
  xlab("")+
  ylab("d'")

midhighplot



####canopy####
plotweb(canopymatrix)

#characterize species-level indices and add to object
canopyspeciesoutput<-specieslevel(canopymatrix)
#pull out lower level (bryophyte) data into object
canopyspeciesoutput1<-canopyspeciesoutput$`lower level`
#look at network-level indices
networklevel(canopymatrix)
#view bryophyte indices
canopyspeciesoutput1
#assign table of indices a species variable to manipulate for plotting
canopyspeciesoutput1$species<-rownames(canopyspeciesoutput1)
canopyspeciesoutput1<-arrange(canopyspeciesoutput1, d)
canopyspeciesoutput1$species<-as.character(canopyspeciesoutput1$species)
canopyspeciesoutput1$species <- factor(canopyspeciesoutput1$species, levels=unique(canopyspeciesoutput1$species))

canopycolor<-inner_join(canopyspeciesoutput1, colororder, by = "species")
canopycolor


#plot d' values for all bryo species
canopyplot<-ggplot(data=canopyspeciesoutput1, aes(species, d)) +
  geom_point(cex = 3, color = canopycolor$color) +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=8),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank(),
    plot.title = element_text(size = 20))+
  ggtitle("Canopy Foragers")+
  xlab("")+
  ylab("d'")

canopyplot

#####create outputs to compare d' between behavioral subsets####
library(dplyr)
multidoutput<-totalspeciesoutput1[,20:21]
names(multidoutput)[1]<-"totald"

multidoutput<-full_join(groundspeciesoutput1[,20:21], multidoutput, by ="species")
names(multidoutput)[1]<-"groundd"

multidoutput<-full_join(understoryspeciesoutput1[,20:21], multidoutput, by ="species")
names(multidoutput)[1]<-"understoryd"

multidoutput<-full_join(midhighspeciesoutput1[,20:21], multidoutput, by ="species")
names(multidoutput)[1]<-"midhighd"

multidoutput<-full_join(canopyspeciesoutput1[,20:21], multidoutput, by ="species")
names(multidoutput)[1]<-"canopyd"


multidoutput$grounddiff<-multidoutput$totald-multidoutput$groundd
multidoutput$understorydiff<-multidoutput$totald-multidoutput$understoryd
multidoutput$midhighdiff<-multidoutput$totald-multidoutput$midhighd
multidoutput$canopydiff<-multidoutput$totald-multidoutput$canopyd


####linear models to assess subset fit to total d'


groundmod<-lm(totald~groundd, data = multidoutput)
summary(groundmod)
understorymod<-lm(totald~understoryd, data=multidoutput)
summary(understorymod)
midhighmod<-lm(totald~midhighd, data = multidoutput)
summary(midhighmod)
canopymod<-lm(totald~canopyd, data=multidoutput)
summary(canopymod)



#######################################base R plots of d' total and d' subset relationships
plot(totald~groundd, xlim = c(0, 0.8), ylim = c(0, 0.8), data=multidoutput)
abline(0, 1)
text(multidoutput$groundd, multidoutput$totald, multidoutput$species, pos =4, cex=0.6, col="red")
#abline(groundmod)
plot(totald~understoryd, xlim = c(0, 0.9), ylim = c(0, 0.9), data=multidoutput)
abline(0,1)
text(multidoutput$understoryd, multidoutput$totald, multidoutput$species, pos =4, cex=0.6, col="red")
#abline(understorymod)
plot(totald~midhighd, xlim = c(0, 1.1), ylim = c(0, 1.1), data=multidoutput)
abline(0,1)
text(multidoutput$midhighd, multidoutput$totald, multidoutput$species, pos =4, cex=0.6, col="red")
plot(totald~canopyd, xlim = c(0, 1.1), ylim = c(0, 1.1), data=multidoutput)
abline(0,1)
text(multidoutput$canopyd, multidoutput$totald, multidoutput$species, pos =4, cex=0.6, col="red")



library(ggrepel)
#########################################ggplot plots of total d' and subset d' relationships
gggroundbytotal<-ggplot(data=subset(multidoutput, groundd != "NA"), aes(groundd, totald, label= species))+
  geom_point(cex = 1)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,0.8)+
  ylim(0,0.8)+
  xlab("")+
  ylab("")+
  theme(axis.text=element_text(size=8) )
  #geom_text_repel()
 
ggunderstorybytotal<-ggplot(data=subset(multidoutput, understoryd != "NA"), aes(understoryd, totald, label= species))+
  geom_point(cex= 1)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,0.9)+
  ylim(0,0.9)+
  xlab("")+
  ylab("")+
  theme(axis.text=element_text(size=8) )
  #geom_text_repel()

ggmidhighbytotal<-ggplot(data=subset(multidoutput, midhighd != "NA"), aes(midhighd, totald, label= species))+
  geom_point(cex = 1)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,1)+
  ylim(0,1)+
  xlab("")+
  ylab("")+
  theme(axis.text=element_text(size=8) )
  #geom_text_repel()

ggcanopybytotal<-ggplot(data=subset(multidoutput, canopyd != "NA"), aes(canopyd, totald, label= species))+
  geom_point(cex = 1)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,1)+
  ylim(0,1)+
  xlab("")+
  ylab("")+
  theme(axis.text=element_text(size=8) )
#geom_text_repel()

gggroundbytotal
ggunderstorybytotal
ggmidhighbytotal
ggcanopybytotal



######################dumbell plots
grounddumbell <- ggplot(subset(multidoutput, groundd != "NA"), aes(x=totald, xend=groundd, y=reorder(species, groundd), group=species)) + 
  geom_dumbbell(colour = "black", size = 1, size_x = 3, size_xend = 3, colour_x = "#E69F00", colour_xend = "#56B4E9") + 
  #scale_x_continuous(label= "d") + 
  labs(x=NULL, 
       y=NULL, 
       title= NULL, 
       subtitle= NULL) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.y = element_text(size = 8),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(grounddumbell)



understorydumbell <- ggplot(subset(multidoutput, understoryd != "NA"), aes(x=totald, xend=understoryd, y=reorder(species, understoryd), group=species)) + 
  geom_dumbbell(colour = "black", size = 1, size_x = 3, size_xend = 3, colour_x = "#E69F00", colour_xend = "#56B4E9") + 
  #scale_x_continuous(label= "d") + 
  labs(x=NULL, 
       y=NULL, 
       title= NULL, 
       subtitle= NULL) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.y = element_text(size = 8),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        #panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(understorydumbell)

 
midhighdumbell <- ggplot(subset(multidoutput, midhighd != "NA"), aes(x=totald, xend=midhighd, y=reorder(species, midhighd), group=species)) + 
  geom_dumbbell(colour = "black", size = 1, size_x = 3, size_xend = 3, colour_x = "#E69F00", colour_xend = "#56B4E9") + 
  #scale_x_continuous(label= "d") + 
  labs(x=NULL, 
       y=NULL, 
       title= NULL, 
       subtitle= NULL) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.y = element_text(size = 8),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        #panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(midhighdumbell)

canopydumbell <- ggplot(subset(multidoutput, canopyd != "NA"), aes(x=totald, xend=canopyd, y=reorder(species, canopyd), group=species)) + 
  geom_dumbbell(colour = "black", size = 1, size_x = 3, size_xend = 3, colour_x = "#E69F00", colour_xend = "#56B4E9") +
  #scale_x_continuous(label= "d") + 
  labs(x=NULL, 
       y=NULL, 
       title= NULL, 
       subtitle= NULL) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.y = element_text(size = 8),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        #panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(canopydumbell)


####################finalize d' figures
library(cowplot)

groundfinald<-ggdraw() +
  draw_plot(groundplot) +
  draw_plot(gggroundbytotal, x = 0.05, y = .55, width = .35, height = .35)

understoryfinald<-ggdraw() +
  draw_plot(understoryplot) +
  draw_plot(ggunderstorybytotal, x = 0.05, y = .55, width = .35, height = .35)

midhighfinald<-ggdraw() +
  draw_plot(midhighplot) +
  draw_plot(ggcanopybytotal, x = 0.05, y = .55, width = .35, height = .35)

canopyfinald<-ggdraw() +
  draw_plot(canopyplot) +
  draw_plot(ggcanopybytotal, x = 0.05, y = .55, width = .35, height = .35)
  
groundfinald
understoryfinald
midhighfinald
canopyfinald

pdf("figures/totald.pdf", width =12, paper="a4r")
par(omi=c(1,1,1,1))
plot(totalplot)
dev.off()

pdf("figures/groundfinald.pdf", width =12, paper="a4r")
par(omi=c(1,1,1,1))
plot(groundfinald)
dev.off()

pdf("figures/understoryfinald.pdf", width =12, paper="a4r")
par(omi=c(1,1,1,1))
plot(understoryfinald)
dev.off()

pdf("figures/airfinald.pdf", width =12, paper="a4r")
par(omi=c(1,1,1,1))
plot(airfinald)
dev.off()



dfourpanel<-plot_grid(groundfinald, understoryfinald, midhighfinald, canopyfinald, labels = c("B", "C", "D", "E"))
dfourpanel

bottomrow<-plot_grid(groundfinald, understoryfinald, midhighfinald, nrow = 1, labels = c("B", "C", "D"))
bottomrow

fulldplot<-plot_grid(totalplot, dffourpanel, ncol = 1, rel_widths = c(1,3), labels = c("A", ""))
fulldplot

ddumbellfourpanel<-plot_grid(grounddumbell, understorydumbell, midhighdumbell, canopydumbell, labels = c("B", "C", "D", "E"))
ddumbellfourpanel

dumbelltotalplot<-plot_grid(totalplot, ddumbellfourpanel, ncol = 2, rel_widths = c(1,3) , labels = c("A", ""))
dumbelltotalplot

pdf("dumbelltotalplot.pdf", width = 12)
par(omi=c(1,1,1,1))
plot(dumbelltotalplot)
dev.off()

pdf("figures/dfourplot.pdf", width =12, paper="a4r")
par(omi=c(1,1,1,1))
plot(dfourpanel)
dev.off()

pdf("figures/ddumbell.pdf", width =12, paper="a4r")
par(omi=c(1,1,1,1))
plot(ddumbellfourpanel)
dev.off()



########################
#Create null webs using null.distr after Dormann, which involves sampling from the marginals to create new marginal distributions
#and generating new webs using abundance crossproducts
obstotal <- unlist(networklevel(totalmatrix, index="H2"))

crossnulls<-null.distr(1000, totalmatrix, dist="negbin")

crossnullH2<- unlist(sapply(crossnulls, networklevel, index="H2")) #takes a while ...

plot(density(crossnullH2), xlim=c(min(obstotal, min(crossnullH2)), max(obstotal, max(crossnullH2))), 
     main="All data comparison of observed with null model crossproduct")
abline(v=obstotal, col="red", lwd=2)   

###examine connectance#####
crossnullconnect<- unlist(sapply(crossnulls, networklevel, index="connectance"))
obstotalconnect <- unlist(networklevel(totalmatrix, index="connectance"))
plot(density(crossnullconnect), xlim=c(min(obstotalconnect, min(crossnullconnect)), max(obstotalconnect, max(crossnullconnect))), 
     main="All data comparison of observed with null model crossproduct")
abline(v=obstotalconnect, col="red", lwd=2)  


#########examine compartments#####
crossnullcompartment<- unlist(sapply(crossnulls, networklevel, index="number of compartments"))
obstotalcompartment <- unlist(networklevel(totalmatrix, index="number of compartments"))
plot(density(crossnullcompartment), xlim=c(min(obstotalcompartment, min(crossnullcompartment)), max(obstotalcompartment, max(crossnullcompartment))), 
     main="All data comparison of observed with null model crossproduct")
abline(v=obstotalcompartment, col="red", lwd=2)   

length(which(crossnullcompartment == 1))
893/1000
length(which(crossnullcompartment == 2))
99/1000
length(which(crossnullcompartment == 3))

which(crossnullcompartment == 3)
plotweb(crossnulls[[486]])



########################



#############Comparing null H2 from null.dist to observed value

nulltotalframe<-as.data.frame(crossnullH2)
colnames(nulltotalframe)<-"H2"
nulltotalframe$treat<-rep("null", nrow(nulltotalframe))
nulltotalframe

obstotalframe<-as.data.frame(obstotal)
colnames(obstotalframe)<-"H2"
obstotalframe$treat<-rep("observed", nrow(obstotalframe))
obstotalframe

h2totalvalues<-bind_rows(nulltotalframe, obstotalframe)

head(h2totalvalues)


totalobsh2<-filter(h2totalvalues, treat == "observed")$H2

length((filter(h2totalvalues, treat == "null" & H2  >= totalobsh2))$H2)/length((filter(h2totalvalues, treat == "null"))$H2) 
#p<0.001

max(h2totalvalues$H2)       

h2totalplot<-ggplot(data=h2totalvalues, aes(treat, H2)) +
  geom_boxplot(fill = "darkgrey") +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=18),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank(),
    plot.title = element_text(size = 20))+
  ggtitle("All Behavioral Types")+
  ylab("H2'")+
  xlab("")

h2totalplot

#####plot ground null vs observed
obsground <- unlist(networklevel(groundmatrix, index="H2"))
nullsground <- null.distr(1000, groundmatrix, distr="negbin")
nullground <- unlist(sapply(nullsground, networklevel, index="H2")) #takes a while ...

plot(density(nullground), xlim=c(min(obsground, min(nullground)), max(obsground, max(nullground))), 
     main="Ground comparison of observed with null model Patefield")
abline(v=obsground, col="red", lwd=2)   

nullground2<-as.data.frame(nullground)
colnames(nullground2)<-"H2"
nullground2$treat<-rep("null", nrow(nullground2))
nullground2

obsground2<-as.data.frame(obsground)
colnames(obsground2)<-"H2"
obsground2$treat<-rep("observed", nrow(obsground2))
obsground2

h2groundvalues<-bind_rows(nullground2, obsground2)

h2groundvalues

groundobsh2<-filter(h2groundvalues, treat == "observed")$H2

length((filter(h2groundvalues, treat == "null" & H2  >= groundobsh2))$H2)/length((filter(h2groundvalues, treat == "null"))$H2) 
#p = 0.005


h2groundplot<-ggplot(data=h2groundvalues, aes(treat, H2)) +
  geom_boxplot(fill = "darkgrey") +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=18),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank(),
    plot.title = element_text(size = 20))+
  ggtitle("Ground Foragers")+
  xlab("")+
  ylab("H2'")

h2groundplot

#####plot understory null vs observed
obsunderstory <- unlist(networklevel(understorymatrix, index="H2"))
nullsunderstory <- null.distr(1000, understorymatrix, distr ="negbin")
nullunderstory <- unlist(sapply(nullsunderstory, networklevel, index="H2")) #takes a while ...

plot(density(nullunderstory), xlim=c(min(obsunderstory, min(nullunderstory)), max(obsunderstory, max(nullunderstory))), 
     main="understory comparison of observed with null model Patefield")
abline(v=obsunderstory, col="red", lwd=2)   

nullunderstory2<-as.data.frame(nullunderstory)
colnames(nullunderstory2)<-"H2"
nullunderstory2$treat<-rep("null", nrow(nullunderstory2))
nullunderstory2

obsunderstory2<-as.data.frame(obsunderstory)
colnames(obsunderstory2)<-"H2"
obsunderstory2$treat<-rep("observed", nrow(obsunderstory2))
obsunderstory2

h2understoryvalues<-bind_rows(nullunderstory2, obsunderstory2)

h2understoryvalues

understoryobsh2<-filter(h2understoryvalues, treat == "observed")$H2

length((filter(h2understoryvalues, treat == "null" & H2  >= understoryobsh2))$H2)/length((filter(h2understoryvalues, treat == "null"))$H2) 
#p<0.001

h2understoryplot<-ggplot(data=h2understoryvalues, aes(treat, H2)) +
  geom_boxplot(fill = "darkgrey") +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=18),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank(),
    plot.title = element_text(size = 20))+
  ggtitle("Understory Foragers")+
  xlab("")+
  ylab("H2'")

h2understoryplot

#####plot midhigh null vs observed
obsmidhigh <- unlist(networklevel(midhighmatrix, index="H2"))
nullsmidhigh <- null.distr(1000, midhighmatrix, distr="negbin")
nullmidhigh <- unlist(sapply(nullsmidhigh, networklevel, index="H2")) #takes a while ...

plot(density(nullmidhigh), xlim=c(min(obsmidhigh, min(nullmidhigh)), max(obsmidhigh, max(nullmidhigh))), 
     main="midhigh comparison of observed with null model Patefield")
abline(v=obsmidhigh, col="red", lwd=2)   

nullmidhigh2<-as.data.frame(nullmidhigh)
colnames(nullmidhigh2)<-"H2"
nullmidhigh2$treat<-rep("null", nrow(nullmidhigh2))
nullmidhigh2

obsmidhigh2<-as.data.frame(obsmidhigh)
colnames(obsmidhigh2)<-"H2"
obsmidhigh2$treat<-rep("observed", nrow(obsmidhigh2))
obsmidhigh2

h2midhighvalues<-bind_rows(nullmidhigh2, obsmidhigh2)

h2midhighvalues
midhighobsh2
midhighobsh2<-filter(h2midhighvalues, treat == "observed")$H2
max(h2midhighvalues$H2)
length((filter(h2midhighvalues, treat == "null" & H2  >= midhighobsh2))$H2)/length((filter(h2midhighvalues, treat == "null"))$H2) 
#p>0.01


h2midhighplot<-ggplot(data=h2midhighvalues, aes(treat, H2)) +
  geom_boxplot(fill = "darkgrey") +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=18),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank(),
    plot.title = element_text(size = 20))+
  ggtitle("Midhigh Foragers")+
  xlab("")+
  ylab("H2'")

h2midhighplot


#####plot canopy null vs observed
obscanopy <- unlist(networklevel(canopymatrix, index="H2"))
nullscanopy <- null.distr(1000, canopymatrix, distr="negbin")
nullcanopy <- unlist(sapply(nullscanopy, networklevel, index="H2")) #takes a while ...

plot(density(nullcanopy), xlim=c(min(obs, min(nullcanopy)), max(obs, max(nullcanopy))), 
     main="comparison of observed with null model Patefield")
abline(v=obs, col="red", lwd=2)   

nullcanopy2<-as.data.frame(nullcanopy)
colnames(nullcanopy2)<-"H2"
nullcanopy2$treat<-rep("null", nrow(nullcanopy2))
nullcanopy2

obscanopy2<-as.data.frame(obscanopy)
colnames(obscanopy2)<-"H2"
obscanopy2$treat<-rep("observed", nrow(obscanopy2))
obscanopy2

h2canopyvalues<-bind_rows(nullcanopy2, obscanopy2)

h2canopyvalues
mean(subset(nullcanopy2$H2, nullcanopy2$H2 != "NA"))

length((filter(h2canopyvalues, treat == "null" & H2  >= obscanopy2$H2))$H2)/length((filter(h2canopyvalues, treat == "null"))$H2) 
#p = 0.216



h2canopyplot<-ggplot(data=h2canopyvalues, aes(treat, H2)) +
  geom_boxplot(fill = "darkgrey") +
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank())+
    ggtitle("Canopy Foragers")+
  xlab("")+
  ylab("")

h2canopyplot

h2fourpanel<-plot_grid(h2totalplot, h2groundplot, h2understoryplot, h2midhighplot, h2canopyplot, labels = c("A", "B", "C", "D", "E"))
h2fourpanel

pdf("figures/h2fourplot.pdf", paper="a4r")
par(omi=c(1,1,1,1))
plot(h2fourpanel)
dev.off()


##################### histogram plot


h2totalhistplot<-ggplot(data=h2totalvalues, aes(treat, H2)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  geom_errorbar(max=upper, min =lower)+
  theme_bw()+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank())+
  xlab("")+
  ylab("")

h2totalhistplot

h2totalvalues1<-nulltotalframe
h2totalvalues1$treat<-"Total"
h2groundvalues1<-subset(h2groundvalues, treat != "observed")
h2groundvalues1$treat<-"Ground"
h2understoryvalues1<-subset(h2understoryvalues, treat != "observed")
h2understoryvalues1$treat<-"Understory"
h2midhighvalues1<-subset(h2midhighvalues, treat != "observed")
h2midhighvalues1$treat<-"Midhigh"
h2canopyvalues1<-subset(h2canopyvalues, treat != "observed")
h2canopyvalues1$treat<-"Canopy"

allh2<-rbind(h2totalvalues1, h2groundvalues1, h2understoryvalues1, h2midhighvalues1, h2canopyvalues1)
allh2$treat<-as.factor(allh2$treat)

allh2$treat<-factor(allh2$treat, levels = c("Total", "Ground", "Understory", "Midhigh", "Canopy"))

allh2obs<-as.data.frame(rbind(obstotal, obsground, obsunderstory, obsmidhigh, obscanopy))
allh2obs$treat<-c("Total", "Ground", "Understory", "Midhigh", "Canopy")
levels(allh2obs$treat)<-c("Total", "Ground", "Understory", "Midhigh", "Canopy")




allh2plot<-ggplot(data=allh2, aes(treat, H2)) +
  geom_boxplot(outlier.shape = NA, fill ="chartreuse4", alpha =0.7) +
  geom_boxplot(data=allh2obs, aes(treat, H2), colour = "darkorange4")+
  #labs(y=expression(H[2]))+
  theme_bw()+
  coord_flip()+
  theme(#axis.title.x = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=12, face = "bold"),
    axis.text.y = element_text(size =12, face = "bold"),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank())+
  ggtitle("")+
  xlab("")+
  ylab(bquote(~H[2]~"'"))
 
allh2plot
pdf("allh2plot.pdf")
allh2plot
dev.off()

library(png)
library(magick)

dougfir<- readPNG( source = "PhyloPic.38c636a7.Michele-M-Tobias.Laricoideae_Pinaceae_Pseudotsuga_Pseudotsuga-menziesii.png")

dougfirplot<-ggdraw() +
  draw_plot(allh2plot) +
  draw_image(dougfir, x = 0.7, y = .34, width = .4, height = 0.58)

png("dougfirplot.png")
dougfirplot
dev.off()

dougfirplot


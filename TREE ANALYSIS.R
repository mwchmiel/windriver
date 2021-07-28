library(ape)
library(phangorn)
library(seqinr)
#load library for manipulating phylogenies, specifically in this case to
#access rename.fasta
library(phylotools)
library(picante)
library(tidyverse)

###load in fasta file
bryos<-read.dna("mossrenamedcopy.fasta", format="fasta")
#next transfer into phyDat format
bryos_phyDat <- phyDat(bryos, type = "DNA", levels = NULL)
#modeltest....might have issues on windows machine
##mt<-modelTest(bryos_phyDat)
##print(mt)
#make distance data from the phyDat data for use in building the tree
bryodistance<-dist.ml(bryos_phyDat)
#perform upgma analysis on distances
bryos_upgma<-upgma(bryodistance)
#perform neighbor joining analysis on distances
##bryosNJ<-NJ(bryodistance)
#plot either one. the labels overlap in a big mess, so changing the text size (cex)
#and the spacing between values on the y axis (oma) helps make it legible
plot(bryos_upgma, oma= 1, cex=.1)
bryos_upgma$tip.label####




birdsite<-read.csv("birdsassites.csv", row.names = 1, header = TRUE)
birdsite1<-as.matrix(birdsite)
names(birdsite)

prunedbryos<-prune.sample(birdsite1, bryos_upgma)
plot(prunedbryos, oma=1, cex=.1)
par(mfrow = c(1, 1))
for (i in row.names(birdsite1)) {
  plot(prunedbryos, show.tip.label = FALSE, main = i)
  tiplabels(tip = which(prunedbryos$tip.label %in% names(which(birdsite1 [i, ] > 0))) ,pch=19) 
}

birdtreebyphylo<-read.dna(file="birdsrenamedbyphylo.fasta", format="fasta")
birdtreebyphylophy<-phyDat(birdtreebyphylo, type = "DNA", levels = NULL)
birdtreephylodistance<-dist.ml(birdtreebyphylophy)
birdtreephylo_upgma<-upgma(birdtreephylodistance)
plot(birdtreephylo_upgma )


birdsassitesbyphylo<-read.csv("birdsassitesbyphylo.csv", row.names = 1, header= TRUE)
birdsassitesbyphylo1<-as.matrix(birdsassitesbyphylo)
row.names(birdsassitesbyphylo1)
prunedphylobryos<-prune.sample(birdsassitesbyphylo1, bryos_upgma)



for (i in row.names(birdsassitesbyphylo1)) {
  plot(birdtreephylo_upgma, show.tip.label = FALSE, main = i)
  tiplabels(tip = which(prunedphylobryos$tip.label %in% names(which(birdsassitesbyphylo1 [i, ] > 0))), pch=19) 
}

birdspeciesanalysis<-ses.pd(birdsite[,-1], bryos_upgma, include.root=FALSE, null.model= "taxa.labels")
write.csv(birdspeciesanalysis, file="phylodistance.csv")
birdspeciesanalysis

birdspeciesanalysis$species<-rownames(birdspeciesanalysis)
birdspeciesanalysis$samplesize<-rowSums(birdsite[,-1])
birdspeciesanalysis$CI<-(birdspeciesanalysis$pd.rand.sd*1.96)/sqrt(birdspeciesanalysis$samplesize)
speciespd<-gather(birdspeciesanalysis, pd.type, pd.val, 2:3)
speciespd



pdplot<-ggplot(data=droplevels(subset(speciespd, pd.val != "NaN")), aes(species, pd.val, fill = pd.type)) +
  geom_point(shape = 21, cex =3, alpha = .6) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("", ""))+
  #scale_color_discrete(name= "", breaks=c("pd.obs", "pd.rand.mean"), labels=c("Observed PD", "Null Distribution"))+
  geom_errorbar(data=subset(speciespd, pd.type =="pd.rand.mean" & pd.val!= "NaN"), aes(ymax=(pd.val+CI), ymin=(pd.val-CI), width=.4), color ="black")+
  theme_bw()+
  labs(fill ="")+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=12, face = "bold"),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    legend.position = "none",
    panel.grid.major.y=element_blank())+
  ggtitle("A")+
  xlab("")+
  ylab("Faith's Phylogenetic Distance")

pdplot



pd.phylo<-pd(birdsassitesbyphylo1, bryos_upgma, include.root = FALSE)
pd.phylo

phylaanalysis<-ses.pd(birdsassitesbyphylo1, bryos_upgma, include.root=FALSE, null.model= "taxa.labels")
phylaanalysis

phylaanalysis$phylum<-rownames(phylaanalysis)
phylaanalysis$samplesize<-rowSums(birdsassitesbyphylo1)
phylaanalysis$CI<-(phylaanalysis$pd.rand.sd*1.96)/sqrt(phylaanalysis$samplesize)
phylapd<-gather(phylaanalysis, pd.type, pd.val, 2:3)
phylapd


phylumpdplot<-ggplot(data=droplevels(subset(phylapd, pd.val != "NaN")), aes(phylum, pd.val, fill = pd.type)) +
  geom_point(shape = 21, cex =3, alpha = .6) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("Observed", "Null"))+
  geom_errorbar(data=subset(phylapd, pd.type =="pd.rand.mean" & pd.val!= "NaN"), aes(ymax=(pd.val+CI), ymin=(pd.val-CI), width=.4), color = "black")+
  theme_bw()+
  labs(fill = "")+
  theme(#axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x  = element_text(angle=90, vjust=0.5, size=12, face = "bold"),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank())+
  ggtitle("B")+
  xlab("")+
  ylab("")

phylumpdplot

library(cowplot)

finalpdplot<-plot_grid(pdplot, phylumpdplot)

pdf("finalpdplot.pdf", paper="a4r", width = 10)
par(omi=c(1,0,1,0))
plot(finalpdplot)
dev.off()



finalpdplot


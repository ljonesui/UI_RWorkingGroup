#Import sp_cov2.xls from Rank Abundance folder
library(readxl)
sp_cov2<-read_excel("sp_cov2.xlsx")
library(tidyr)
library(dplyr)
a1<- gather(sp_cov2, sp, cover, 3:110) #Converts to long format
a2<- a1 %>% group_by(vedu_code, sp) %>% summarize(cover = sum(cover)) #Calculates sum cover based on vedu_code & sp
vedu_rad<- spread(a2, sp, cover) #Converts back to wide format
write.csv(vedu_rad, file="vedu_rad")

##Important: setting row names as first column##
vedu_rad<-as.data.frame(vedu_rad) 
rownames(vedu_rad) <- vedu_rad[,1]
vedu_rad <- vedu_rad[,-1]

library(vegan)
##Use radfit to determine best fit models##
vedu_radfit = radfit(vedu_rad, family=Gamma) #Because using cover data, use Gamma family
vedu_radfit #for each category, lowest number indicates best model
plot(vedu_radfit)

#Plots#
#To make individual plots for the VEDU cover categories, use rad.xxxx function based on the previous plot which tells you which model is the best fit.
vedu_rad_N = rad.zipfbrot(vedu_rad["No",], family=Gamma)
plot_rad_N = plot(vedu_rad_N, plot= TRUE, type = "b", main = "No Ventenata")
identify(plot_rad_N, cex=0.9)

vedu_rad_L = rad.zipfbrot(vedu_rad["Low",], family=Gamma)
plot_rad_L = plot(vedu_rad_L, plot= TRUE, type = "b", main = "Low Ventenata")
identify(plot_rad_L, cex=0.9)

vedu_rad_H = rad.lognormal(vedu_rad["High",], family=Gamma)
plot_rad_H = plot(vedu_rad_H, plot= TRUE, type = "b", main = "High Ventenata")
identify(plot_rad_H, cex=0.9)

#Manuscript quality figures
#THIS IS WRONG--CUTTING OFF TOP POINT OF SYAL @ 925 ABUNDANCE
png(filename="fig5a.png", width=5, height=4, units="in", res=1200)
par(mar=c(2,2,0,0)+0.1)
plot(vedu_rad_N, plot= TRUE, type = "b")
dev.off()

#colors ("#d9f0d3", "#5aae61", "#00441b") light to dark green
png(filename="CGfig5D.png", width=5, height=4, units="in", res=1200)
par(mar=c(2,2,0,0)+0.1)
plot(vedu_rad_N, plot= TRUE, type = "b", pch=21, bg="#d9f0d3", col='black', cex.axis=1.2)
dev.off()


png(filename="fig5b2.png", width=5, height=4, units="in", res=1200)
par(mar=c(4,2,0,0)+0.1)
plot(vedu_rad_L, plot= TRUE, type = "b")
dev.off()

png(filename="fig5c.png", width=5, height=4, units="in", res=1200)
par(mar=c(2,2,0,0)+0.1)
plot(vedu_rad_H, plot= TRUE, type = "b")
dev.off()

#Powerpoint figures
png(filename="rad_No.png", width=5, height=4, units="in", res=600)
par(mar=c(2,2,0,0)+0.1)
plot(vedu_rad_N, xlab="Rank", type = "b", lwd=2)
axis(1, at= 20, labels=20, cex.axis=1.2, cex.lab=1.4)
dev.off()

xvar<-c(1:95)
yvar<-vedu_rad_N$y
yvar2<-as.vector(yvar)
bind<-cbind(xvar, yvar2)
radN<-as.data.frame(bind)

xvarL<-c(1:80)
yvarL<-vedu_rad_L$y
yvarL2<-as.vector(yvarL)
bindL<-cbind(xvarL, yvarL2)
radL<-as.data.frame(bindL)

xvarH<-c(1:56)
yvarH<-vedu_rad_H$y
yvarH2<-as.vector(yvarH)
bindH<-cbind(xvarH, yvarH2)
radH<-as.data.frame(bindH)

library(ggplot2)
ggplot(radN, aes(x=xvar, y=yvar2)) +
  geom_point(shape=1, size=3) +
  geom_smooth(method="auto", se=FALSE, col="black") +
  theme_classic(base_size=20)+
  scale_x_continuous(breaks=0:90*20, expand=c(0,0), limits=c(-1, 90), name="Rank") +
  scale_y_continuous(breaks=0:900*200, expand=c(0,0), limits=c(0, 950), name="Abundance")+
  theme(axis.text=element_text(size=22), axis.title=element_text(size=26),axis.title.x=element_text(vjust=-3), 
        axis.title.y=element_text(vjust=3))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
ggsave("rad_No.png", width = 5.5, height=4.5, units="in", dpi=600)

ggplot(radL, aes(x=xvarL, y=yvarL2)) +
  geom_point(shape=1, size=3) +
  geom_smooth(method="auto", se=FALSE, col="black") +
  theme_classic(base_size=20)+
  scale_x_continuous(breaks=0:90*20, expand=c(0,0), limits=c(-1, 90), name="Rank") +
  scale_y_continuous(breaks=0:600*100, expand=c(0,0), limits=c(0, 500), name="Abundance")+
  theme(axis.text=element_text(size=22), axis.title=element_text(size=26),axis.title.x=element_text(vjust=-3), 
        axis.title.y=element_text(vjust=3))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
ggsave("rad_Low.png", width = 5.5, height=4.5, units="in", dpi=600)


library(scales)
ggplot(radH, aes(x=xvarH, y=yvarH2)) +
  geom_point(shape=1, size=3) +
  geom_smooth(method="auto", se=FALSE, col="black") +
  theme_classic(base_size=20)+
  scale_x_continuous(breaks=0:60*20, expand=c(0,0), limits=c(-1, 65), name="Rank") +
  scale_y_continuous(trans=log10_trans(), name="Abundance")+
  theme(axis.text=element_text(size=22), axis.title=element_text(size=26),axis.title.x=element_text(vjust=-3), 
        axis.title.y=element_text(vjust=3))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
ggsave("rad_High.png", width = 5.5, height=4.5, units="in", dpi=600)



#New color graphs for manuscript
library(ggplot2)
ggplot(radN, aes(x=xvar, y=yvar2)) +
  geom_point(shape=21, size=3, fill="#d9f0d3", show.legend=FALSE) +
  geom_smooth(method="auto", se=FALSE, col="black") +
  theme_classic(base_size=20)+
  scale_x_continuous(breaks=0:90*20, expand=c(0,0), limits=c(-1, 90), name="Rank") +
  scale_y_continuous(breaks=0:900*200, expand=c(0,0), limits=c(0, 950), name="Abundance")+
  theme(axis.title.x = element_text(face="bold"))+
  theme(axis.title.y = element_text(face="bold"))+
  theme(axis.text=element_text(size=21))+
  theme(axis.text.x  = element_text(color="black"))+
  theme(axis.text.y  = element_text(color="black"))+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
ggsave("fig5D.tiff", width = 5.5, height=4.5, units="in", dpi=1200)

ggplot(radL, aes(x=xvarL, y=yvarL2)) +
  geom_point(shape=21, size=3, fill="#5aae61", show.legend=FALSE) +
  geom_smooth(method="auto", se=FALSE, col="black") +
  theme_classic(base_size=20)+
  scale_x_continuous(breaks=0:90*20, expand=c(0,0), limits=c(-1, 90), name="Rank") +
  scale_y_continuous(breaks=0:600*100, expand=c(0,0), limits=c(0, 500))+
  theme(axis.title.x = element_text(face="bold"))+
  theme(axis.text=element_text(size=21), axis.title.y = element_blank())+
  theme(axis.text.x  = element_text(color="black"))+
  theme(axis.text.y  = element_text(color="black"))+
  theme(plot.margin = unit(c(0.4, 0.2, 0.2, 0.2), "cm"))
ggsave("fig5E.tiff", width = 5.5, height=4.5, units="in", dpi=1200)

ggplot(radL, aes(x=xvarL, y=yvarL2)) +
  geom_point(shape=21, size=3, fill="#5aae61", show.legend=FALSE) +
  geom_smooth(method="auto", se=FALSE, col="black") +
  theme_classic(base_size=20)+
  scale_x_continuous(breaks=0:90*20, expand=c(0,0), limits=c(-1, 90), name="Rank") +
  scale_y_continuous(breaks=0:600*100, expand=c(0,0), limits=c(0, 500), name="Abundance")+
  theme(axis.title.x = element_text(face="bold"))+
  theme(axis.title.y = element_text(face="bold"))+
  theme(axis.text=element_text(size=21))+
  theme(axis.text.x  = element_text(color="black"))+
  theme(axis.text.y  = element_text(color="black"))+
  theme(plot.margin = unit(c(0.4, 0.2, 0.2, 0.2), "cm"))
ggsave("fig5E-lab.tiff", width = 5.5, height=4.5, units="in", dpi=1200)


library(scales)
ggplot(radH, aes(x=xvarH, y=yvarH2)) +
  geom_point(shape=21, size=3, fill="#00441b", show.legend=FALSE) +
  geom_smooth(method="auto", se=FALSE, col="black") +
  theme_classic(base_size=20)+
  scale_x_continuous(breaks=0:60*20, expand=c(0,0), limits=c(-1, 65), name="Rank") +
  scale_y_continuous(trans=log10_trans())+
  theme(axis.title.x = element_text(face="bold"))+
  theme(axis.title.y = element_text(face="bold"))+
  theme(axis.text=element_text(size=21), axis.title.y = element_blank())+ 
  theme(axis.text.x  = element_text(color="black"))+
  theme(axis.text.y  = element_text(color="black"))+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
ggsave("fig5F.tiff", width = 5.5, height=4.5, units="in", dpi=1200)

ggplot(radH, aes(x=xvarH, y=yvarH2)) +
  geom_point(shape=21, size=3, fill="#00441b", show.legend=FALSE) +
  geom_smooth(method="auto", se=FALSE, col="black") +
  theme_classic(base_size=20)+
  scale_x_continuous(breaks=0:60*20, expand=c(0,0), limits=c(-1, 65), name="Rank") +
  scale_y_continuous(trans=log10_trans(), name="Abundance")+
  theme(axis.title.x = element_text(face="bold"))+
  theme(axis.title.y = element_text(face="bold"))+
  theme(axis.text=element_text(size=21))+ 
  theme(axis.text.x  = element_text(color="black"))+
  theme(axis.text.y  = element_text(color="black"))+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
ggsave("fig5F-lab.tiff", width = 5.5, height=4.5, units="in", dpi=1200)

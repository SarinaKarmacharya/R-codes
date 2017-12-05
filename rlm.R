
library(ggplot2)
library(reshape2)
library(Hmisc)
library(stats)
library(data.table)
library(ggthemes)
library(fonts)
library(plyr)
library(reshape)
library(TStools)
library(MASS)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

Data<-read.csv("Data.csv", header=TRUE)

DataSubset_TDC<-subset(Data,Subject %in% c("TDC") & Test %in% c("FA") & Hemisphere %in% c("Left") & Region %in% c( "ALIC", "PLIC", "RIC", "ACR", "SCR", "PCR"))
DataSubset_CHD<-subset(Data,Subject %in% c("CHD") & Test %in% c("FA") & Hemisphere %in% c("Left") & Region %in% c("PLIC", "RIC", "ACR", "SCR", "PCR"))

P_FA<-ggplot(data=DataSubset_TDC, aes(x=Age, y=Values))+
  geom_point(aes(color=Region), shape=17, fill="white", size=4)+
  stat_smooth(aes(color=Region, fill=Region), method=rlm, se=FALSE, alpha=0.1, fullrange=TRUE)+
  geom_point(data=DataSubset_CHD, aes(color=Region), size=4, alpha=0.5)+
  scale_x_continuous(name="Age") +
  scale_y_continuous(name="FA, Left Hemisphere")+
  theme(panel.background = element_blank(), axis.text=element_text(size=10, color="black"), axis.title.y=element_text(size=15), axis.title.x=element_text(size=13), panel.border = element_rect(fill = NA, colour = "black"), panel.grid.major = element_line(colour = "gray88", size=0.3), strip.background = element_rect(fill="snow1"))+ theme(legend.title=element_blank(),legend.text=element_text(size=12), legend.key.width = unit(0.3, "cm"), legend.key.height = unit(0.4, "cm"))+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))
P_FA

DataSubset_TDC2<-subset(Data,Subject %in% c("TDC") & Test %in% c("FA") & Hemisphere %in% c("Right") & Region %in% c("PLIC", "RIC", "ACR", "SCR", "PCR",  "SLF"))
DataSubset_CHD2<-subset(Data,Subject %in% c("CHD") & Test %in% c("FA") & Hemisphere %in% c("Right") & Region %in% c("PLIC", "RIC", "ACR", "SCR", "PCR"))

P_FA2<-ggplot(data=DataSubset_TDC2, aes(x=Age, y=Values))+
  geom_point(aes(color=Region), shape=17, fill="white", size=4)+
stat_smooth(aes(color=Region, fill=Region), method=rlm, se=FALSE, alpha=0.1, fullrange=TRUE)+
geom_point(data=DataSubset_CHD2, aes(color=Region), size=4, alpha=0.5)+
scale_x_continuous(name="Age") +
scale_y_continuous(name="FA, Right Hemisphere")+
theme(panel.background = element_blank(), axis.text=element_text(size=10, color="black"), axis.title.y=element_text(size=15), axis.title.x=element_text(size=13), panel.border = element_rect(fill = NA, colour = "black"), panel.grid.major = element_line(colour = "gray88", size=0.3), strip.background = element_rect(fill="snow1"))+ theme(legend.title=element_blank(),legend.text=element_text(size=12), legend.key.width = unit(0.3, "cm"), legend.key.height = unit(0.4, "cm"))+
theme(plot.margin=unit(c(0,0,0,0),"mm"))
P_FA2

pdf("/Users/Karmacharya/Desktop/ALL_LM_FA.pdf", width=18, height=12)
multiplot(P_FA, P_FA2, cols=2)
dev.off()


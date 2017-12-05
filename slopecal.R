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
library(RColorBrewer)
library(extrafont)
loadfonts()

RawData<- read.csv("/Users/Karmacharya/Documents/NEONATE-FINAL_july102016/BARPLOTRESULTS/slope1.csv", header=T)

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

DataSubset_TDC<-subset(RawData, Test %in% c("ODI") & Hemisphere %in% c("Left") & Region %in% c("CC", "ALIC", "PLIC", "RIC", "ACR", "SCR", "PCR", "SLF", "UF", "SFOF","IFOF" ))

#DataCtrl<-subset(RawData,Subject %in% c("TDC") & Hemisphere %in% c("Left"))

#SummaryDataSubset <- summarySE(DataSubset_TDC, measurevar="Values", groupvars=c("Region"))

#SummaryDataCtrl<- summarySE(DataCtrl, measurevar="Values", groupvars=c("Region", "Hemisphere", "Test"))

cl2=c("chocolate4", "yellow1", "#FF7300", "steelblue4", "#B739FF", "#038C0A","ivory", "maroon", "dimgrey", "khaki4", "lightsalmon")


P1<-ggplot(DataSubset_TDC,aes(x= reorder(Region, -Values), y=Values, fill=Region))+
  geom_bar(aes(colour=Region), stat="identity", color="black",  position=position_dodge()) +
  
  geom_errorbar(aes(ymin=Values-se, ymax=Values+se), width=(0.2), position=position_dodge(.9)) + 
  scale_y_continuous(name="")+ labs(x= "")+
  ggtitle("ODI: Left Hemisphere")+scale_color_manual(values = c("black","black"))+ 
  scale_fill_manual(values = cl2)+
  theme(legend.position="none", panel.background = element_blank(),axis.text.x=element_text(size=45, color="black", angle=90),axis.text.y=element_text(size=35, color="black"), axis.title.y=element_text(size=45), axis.title.x=element_text(size=40), plot.title=element_text(size=45), panel.border = element_rect(fill = NA, colour = "black"), panel.grid.major = element_line(colour = "gray88", size=0.3), strip.background = element_rect(fill="snow1"))
P1

DataSubset_TDC_right<-subset(RawData, Test %in% c("ODI") & Hemisphere %in% c("Right") & Region %in% c("CC", "ALIC", "PLIC", "RIC", "ACR", "SCR", "PCR", "SLF", "UF", "SFOF","IFOF" ))





P2<-ggplot(DataSubset_TDC_right,aes(x= reorder(Region, -Values), y=Values, fill=Region))+
  geom_bar(aes(colour=Region), stat="identity", color="black",  position=position_dodge()) +
  
  geom_errorbar(aes(ymin=Values-se, ymax=Values+se), width=(0.2), position=position_dodge(.9)) + 
  scale_y_continuous(name="")+ labs(x= "")+
  ggtitle("ODI: Right Hemisphere")+scale_color_manual(values = c("black","black"))+ 
  scale_fill_manual(values = cl2)+
  theme(legend.position="none", panel.background = element_blank(),axis.text.x=element_text(size=45, color="black", angle=90),axis.text.y=element_text(size=35, color="black"), axis.title.y=element_text(size=45), axis.title.x=element_text(size=40), plot.title=element_text(size=45), panel.border = element_rect(fill = NA, colour = "black"), panel.grid.major = element_line(colour = "gray88", size=0.3), strip.background = element_rect(fill="snow1"))
P2

pdf("/Users/Karmacharya/Documents/NEONATE-FINAL_july102016/BARPLOTRESULTS/Slope/BarPLOT_SLOPE_ODI.pdf",  family="CM Roman", width=26, height=11)
multiplot(P1,P2, cols=2)
dev.off()





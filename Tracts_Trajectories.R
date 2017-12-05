
library(extrafont)
font_install('fontcm')
library(extrafont)
loadfonts()
library(ggplot2)
library(reshape2)
library(Hmisc)
library(stats)
library(plyr)
library(MASS)
library(fontcm)
library(extrafont)
library(dplyr)
library(nls2)
loadfonts()

setwd("~/Documents/Rhesus_monkey_graph/Sarina/Tracts/")
#Bar graph

data <- read.csv("~/Documents/Rhesus_monkey_graph/Sarina/Tracts/Percent_Bar_Graph.csv")
plot(1:7,type='n', yaxt = "n", xlim=c(4,27),ylab = "Tracts")
#axis(2,at =1:7,labels= c("CC","CB","SLF ||","ALIC","PLIC","SLF |||","AC","AF"),las =1)
g1 <- ggplot(data,aes(x=Tracts,y=FA_peakage,fill=FA_Percent_change))+
  geom_bar(stat="identity") +
  theme_classic() + 
  coord_flip() + 
  scale_fill_continuous(low="blue",high="red", name="FA Percent Change", space="lab")+
  geom_errorbar(limits<-aes(ymax = data$FA_peakage + data$FA_se, ymin=data$FA_peakage -   data$FA_se), size=1.5, color="black")+
  scale_y_continuous(name="Age (years)")+
  theme(panel.background = element_blank(),
        axis.text.x=element_text(face="bold",size=65, color="black"),
        axis.text.y=element_text(face="bold", size=55, color="black"),
        axis.title.y=element_text(face="bold",size=80),
        axis.title.x=element_text(face="bold",size=80),
        plot.title=element_text(face="bold",size=80),
        panel.border = element_rect(fill = NA, linetype="solid", size=2, colour = "black"),
        #legend.title=element_blank(),
        legend.title=element_text(face="bold", size=60),
        legend.text=element_text(face="bold", size=45),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(5, "cm"),
        legend.text.align = 1,
        strip.background = element_rect(fill="snow1"),
        #guides(fill=guide_legend(title="FA Percent Change"))+
        plot.margin=unit(c(1,1,1.5,1), "lines"))
  #text=element_text(family="CM Roman"))+
  
pathFA=("~/Documents/Rhesus_monkey_graph/Sarina/Tracts/FA_bar.pdf")
ggsave(filename = pathFA, plot = g1, width = 45, height = 20,family="CM Roman")
g1

g2 <- ggplot(data,aes(x=Tracts,y=FAt_peakage,fill=FAt_Percent_change))+
  geom_bar(stat="identity") +
  theme_classic()+
  coord_flip() + 
  scale_fill_continuous(low="blue",high="red", space="Lab", name= "FAt Percent Change") +
  geom_errorbar(limits<-aes(ymax = data$FAt_peakage + data$FAt_se, ymin=data$FAt_peakage - data$FAt_se), size=1.5, color="black") +
  scale_y_continuous(name="Age (years)")+
  theme(panel.background = element_blank(),
        axis.text.x=element_text(face="bold",size=65, color="black"),
        axis.text.y=element_text(face="bold", size=65, color="black"),
        axis.title.y=element_text(face="bold",size=80),
        axis.title.x=element_text(face="bold",size=80),
        plot.title=element_text(face="bold",size=80),
        panel.border = element_rect(fill = NA, linetype="solid", size=2, colour = "black"),
        #legend.title=element_blank(),
        legend.title=element_text(face="bold", size=60),
        legend.text=element_text(face="bold", size=45),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(5, "cm"),
        legend.text.align = 1,
        strip.background = element_rect(fill="snow1"),
        #guides(fill=guide_legend(title="FAt Percent Change"))+
        plot.margin=unit(c(1,1,1.5,1), "lines"))
#text=element_text(family="CM Roman"))+
pathFAt=("~/Documents/Rhesus_monkey_graph/Sarina/Tracts/FAt_bar.pdf")
ggsave(filename = pathFAt, plot = g2, width = 45, height = 20,family="CM Roman")

g2


g3 <- ggplot(data,aes(x=Tracts,y=FW_peakage,fill=FW_Percent_change))+
  geom_bar(stat="identity") +theme_classic() +
  coord_flip()+
  scale_fill_gradient(low="blue",high="red",  space = "Lab", name="FW Percent Change")+
  geom_errorbar(limits<-aes(ymax = data$FW_peakage + data$FW_se, ymin=data$FW_peakage - data$FW_se), size=1.5, color="black")+
  scale_y_continuous(name="Age (years)")+
  theme(panel.background = element_blank(),
        axis.text.x=element_text(face="bold",size=65, color="black"),
        axis.text.y=element_text(face="bold", size=65, color="black"),
        axis.title.y=element_text(face="bold",size=80),
        axis.title.x=element_text(face="bold",size=80),
        plot.title=element_text(face="bold",size=80),
        panel.border = element_rect(fill = NA, linetype="solid", size=2, colour = "black"),
        #legend.title=element_blank(),
        legend.title=element_text(face="bold", size=60),
        legend.text=element_text(face="bold", size=45),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(5, "cm"),
        legend.text.align = 1,
        strip.background = element_rect(fill="snow1"),
        #guides(fill=guide_legend(title="FW Percent Change"))+
        plot.margin=unit(c(1,1,1.5,1), "lines"))
        #text=element_text(family="CM Roman"))+
pathFW=("~/Documents/Rhesus_monkey_graph/Sarina/Tracts/FW_bar.pdf")
ggsave(pathFW, plot=g3, width = 45, height = 20,family="CM Roman")
g3



# in command line type g1 g2 and g3 to get the respective bar graphs.

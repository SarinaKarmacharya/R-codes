
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
# CC:"firebrick"
# SLF: "darkkhaki"
# UF: "#4682B4"
# SFOF: "#86592d"
# IFOF: "#ffcc00"        

# user-level interMDce 
axis.groups = function(groups) {
  structure(
    list(groups=groups),
    ## inheritance since it should be a element_text
    class = c("element_custom","element_blank")  
  )
}
# returns a gTree with two children: 
# the categories axis
# the groups axis
element_grob.element_custom <- function(element, x,...)  {
  cat <- list(...)[[1]]
  groups <- element$group
  ll <- by(data$Group,data$Category,I)
  tt <- as.numeric(x)
  grbs <- Map(function(z,t){
    labs <- ll[[z]]
    vp = viewport(
      x = unit(t,'native'), 
      height=unit(2,'line'),
      width=unit(diff(tt)[1],'native'),
      xscale=c(0,length(labs)))
    grid.rect(vp=vp)
    textGrob(labs,x= unit(seq_along(labs)-0.5,
                          'native'),
             y=unit(2,'line'),
             vp=vp)
  },cat,tt)
  g.X <- textGrob(cat, x=x)
  gTree(children=gList(do.call(gList,grbs),g.X), cl = "custom_axis")
}

## # gTrees don't know their size 
grobHeight.custom_axis = 
  heightDetails.custom_axis = function(x, ...)
    unit(5, "lines")

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

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=MDLSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=MDLSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

cl2=c( "#FE1E26", "#0D8B0F")
RawData<- read.csv("/Users/Karmacharya/Documents/NEONATE-FINAL_july102016/RESI-COM-DATA_final_data_AD.csv", header=T)
Data_range=subset(RawData, Test %in% c("ODI") & Region %in% c("CC", "IFOF", "SFOF", "UF", "SLF"))
min_val=min(Data_range$Values)
max_val=max(Data_range$Values)

DataSubset_TDC<-subset(RawData, Test %in% c("ODI") & Hemisphere %in% c("Left") & Region %in% c("CC", "IFOF", "SFOF", "UF", "SLF"))



#RawDataN <- summarySE(DataSubset_TDC, measurevar="Values", groupvars=c("Subject", "Test", "Region", "Hemisphere"))

#RawDataN2 <- summarySE(RawData, measurevar="SLF_26", groupvars=c("subject"))
P1<-ggplot(DataSubset_TDC,aes(x=Region, y=Values, fill=Subject))
P1<-P1 + geom_boxplot(aes(colour=Subject), size=0.5) +
  labs(y= "ODI")+
  ggtitle("ODI: Left Hemisphere")+scale_color_manual(values = c("black", "black")) +scale_fill_manual(values = cl2)+
  theme(panel.background = element_blank(), axis.text=element_text(size=50, color="black"), axis.title.y=element_text(size=50), axis.title.x=element_text(size=45), plot.title=element_text(size=55), panel.border = element_rect(fill = NA, colour = "black"), panel.grid.major = element_line(colour = "gray88", size=0.3), strip.background = element_rect(fill="snow1"))+ 
  theme(legend.title=element_blank(),legend.text=element_text(size=45), legend.key.width = unit(1, "cm"), legend.key.height = unit(2, "cm"),plot.margin=unit(c(1,1,1.5,1), "lines"))+coord_cartesian(ylim = c(min_val, max_val))
P1
  
DataSubset_TDC<-subset(RawData, Test %in% c("ODI") & Hemisphere %in% c("Right") & Region %in% c("CC", "IFOF", "SFOF", "UF", "SLF"))


P2<-ggplot(DataSubset_TDC,aes(x=Region, y=Values, fill=Subject))
P2<-P2 + geom_boxplot(aes(color=Subject), size=0.5) +
  labs(y= "ODI")+scale_y_continuous(name="")+ 
  ggtitle("ODI: Right Hemisphere")+scale_color_manual(values = c("black","black"))+ scale_fill_manual(values = cl2)+
  theme(panel.background = element_blank(), axis.text=element_text(size=50, color="black"), axis.title.y=element_text(size=50), axis.title.x=element_text(size=45), plot.title=element_text(size=55), panel.border = element_rect(fill = NA, colour = "black"), panel.grid.major = element_line(colour = "gray88", size=0.3), strip.background = element_rect(fill="snow1"))+ 
  theme(legend.title=element_blank(),legend.text=element_text(size=45), legend.key.width = unit(1, "cm"), legend.key.height = unit(2, "cm"), plot.margin=unit(c(1,0,2.5,1), "lines"))+coord_cartesian(ylim = c(min_val, max_val))

P2
pdf("/Users/Karmacharya/Documents/NEONATE-FINAL_july102016/GC_ODI.pdf",  family="CM Roman", width=32, height=11)
multiplot(P1,P2, cols=2)
dev.off()

#ylim=c(0,ceiling(max(Values))+1))
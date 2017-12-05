remove(list=ls())


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

#Create plots from already generated data
# Tracts plots
#data <- read.csv("~/Desktop/Tracts_BU_MGH.csv")

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

Plots_PeakAge <- function(data_new,column_name){
  FA <- select(data_new, Age, get(paste(column_name[[1]][1],"FA",sep ="_")))
  FW <- select(data_new, Age, get(paste(column_name[[1]][1],"FW",sep ="_")))
  Fat <- select(data_new, Age, get(paste(column_name[[1]][1],"Fat",sep ="_")))
  Result_FA <- Poisson_Quadratic(FA,c("FA"),column_name)
  Result_FW <- Poisson_Quadratic(FW,c("FW"),column_name)
  Result_Fat <- Poisson_Quadratic(Fat,c("Fat"),column_name)
  #Result_final <- data.frame(Result_FA,Result_FW,Result_Fat)
  Result_final <- list("Result_FA" = Result_FA$coeff,"Result_FW" = Result_FW$coeff,"Result_Fat" = Result_Fat$coeff)
  return(Result_final)
  #SavePlotPoiss(Result_FA$coeff, FA,"FA")
  #SavePlotPoiss(Result_FA$coeff, FW,"FW")
  #SavePlotPoiss(Result_FA$coeff, Fat,"Fat")
}

Poisson_Quadratic <- function(measure,type,column_name){
  
  colnames(measure)[2] <- "mean_measure"
  
  poiss <- poissonNLSModel(measure)
  quad <- quadraticNLSModel(measure)
  name <- c(column_name[[1]][1],type,".png")
  name <- paste(name,collapse="")
  #SavePlotPoiss(poiss$coeff_poiss, name, measure,type, column_name[[1]][1])
  #result <- list("poiss_peak_age" = poiss$peak_age_poiss, "se_poiss" = poiss$se_peak_age_poiss, "quad_peak_age" = quad$peak_age_quad, "se_quad" = quad$se_peak_age_poiss)
  result <- list("coeff"=poiss$coeff_poiss)  
  return(result)
}
poissonNLSModel <- function(measure){
  Poisson <- nls2( mean_measure ~ C+A*Age*exp(-B*Age),measure, start=list(A = 0.1, B = 0.1, C = 0.1))
  vb <- vcov(Poisson)[2,2]
  B <- coef(Poisson)[2]
  peak_age <- 1/B
  Df = -(1/B^2)
  vG <- Df %*% vb %*% Df #Delta Method
  se_peak_age <- sqrt(vG)
  coeff_poiss <- coef(Poisson)
  result <- list("peak_age_poiss" = peak_age,"se_peak_age_poiss" = se_peak_age, "coeff_poiss" = coeff_poiss)  
  return (result)
}
quadraticNLSModel <- function(measure){
  Quadratic <- nls2(mean_measure ~ A*Age + B*Age^2 + C,measure, start=list(A = 0.001, B = 0.001, C = 0.001))
  vb1 <- vcov(Quadratic)[2,2]
  va1 <- vcov(Quadratic)[1,1]
  covab <- vcov(Quadratic)[1,2]
  B1 <- coef(Quadratic)[2]
  A1 <- coef(Quadratic)[1]
  peak_age <- -A1/(2*B1)
  DfA <- -(1/(2*B1))
  DfB <- A1/(2*(B1^2))
  vG <- DfA %*% va1 %*% DfA + DfB %*% vb1 %*% DfB + DfA %*% covab %*% DfB #Delta Method
  se_peak_age <- sqrt(vG)
  result <- list("peak_age_quad" = peak_age,"se_peak_age_poiss" = se_peak_age)
}

SavePlotPoiss <- function(coeff_poiss, measure,type){
  age <- c(4:27)
  color = c("blue","green","red","dimgrey","black","orange","purple","steelblue4")
  #color=c("chocolate4", "#FF7300", "steelblue4", "#B739FF", "#038C0A","ivory", "maroon", "dimgrey")
  #color=c("#0D033F", "#2D4BFF", "#FF7300", "#FF002E", "#B739FF", "#038C0A", "#000000", "#B739FF", "#038C0A")
  tracts <- c("CC","CB","SLF ||","AnteriorLimbofInternalCapsule","PosteriorLimbofInternalCapsule","SLF |||","AnteriorCommissure","ArcuateFasciculus")
  
  if (type == c("FAt")){
    predicted_measure =as.data.frame(matrix(NA,  ncol=24, nrow=8))
    j <- 1
    for (i in seq(1,24,by = 3)){
      predicted_measure[1:24,i] <- FAt[i+2] + FAt[i]*age*exp(-(FAt[i+1]*age))
      j <- j+1
    }
    g=predicted_measure[ , colSums(is.na(predicted_measure)) == 0]
    colnames(g)=c("CC","CB","SLF II","ALIC","PLIC","SLF III","AC","AF")
    h=data.frame(age, g)
    attach(h)
    hp=melt(h)
    mdata <- melt(h, id=c("age"))
    require(ggplot2)
    FAt=ggplot() +
      geom_line(data = mdata, aes(x = age, y = value, color=variable),size=2.2,alpha=0.9)+
      scale_color_manual(values = color)+
      scale_x_continuous(name="Age (years)") +
      scale_y_continuous(name="FAt")+
      coord_cartesian(ylim = c(0, 0.3))+
      coord_cartesian(xlim = c(4, 27))+
      ggtitle("FAt with Age") + 
      theme(panel.background = element_blank(),
            axis.text.x=element_text(size=45, color="black"),
            axis.text.y=element_text(size=45, color="black"),
            axis.title.y=element_text(size=60),
            axis.title.x=element_text(size=60),
            plot.title=element_text(size=75),
            panel.border = element_rect(fill = NA, colour = "black"),
            panel.grid.major = element_line(colour = "gray80", size=0.3),
            legend.title=element_blank(),
            legend.text=element_text(size=35),
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(2, "cm"),
            strip.background = element_rect(fill="snow1"),
            plot.margin=unit(c(1,1,1.5,1), "lines"))  
      path=("~/Documents/Rhesus_monkey_graph/Sarina/Tracts/FAt.pdf")
      ggsave(filename = path, plot = FAt, width = 19, height = 15,family="CM Roman")
      
  }
  if (type == c("FA")){
    predicted_measure =as.data.frame(matrix(NA,  ncol=24, nrow=8))
    j <- 1
    for (i in seq(1,24,by = 3)){
      predicted_measure[1:24,i] <- FA[i+2] + FA[i]*age*exp(-(FA[i+1]*age))
      #lines(age,predicted_measure,type='l', col =color[j], lwd=1.5)
      j <- j+1
    }
    g=predicted_measure[ , colSums(is.na(predicted_measure)) == 0]
    colnames(g)=c("CC","CB","SLF II","ALIC","PLIC","SLF III","AC","AF")
    h=data.frame(age, g)
    attach(h)
    hp=melt(h)
    mdata <- melt(h, id=c("age"))
    require(ggplot2)
    FA=ggplot() +
      geom_line(data = mdata, aes(x = age, y = value, color=variable),size=2.2,alpha=0.7)+
      scale_color_manual(values = color)+
      scale_x_continuous(name="Age (years)") +
      scale_y_continuous(name="FA")+
      coord_cartesian(ylim = c(0, 0.3))+
      coord_cartesian(xlim = c(4, 27))+
      ggtitle("FA with Age") + 
      theme(panel.background = element_blank(),
            axis.text.x=element_text(size=45, color="black"),
            axis.text.y=element_text(size=45, color="black"),
            axis.title.y=element_text(size=60),
            axis.title.x=element_text(size=60),
            plot.title=element_text(size=75),
            panel.border = element_rect(fill = NA, colour = "black"),
            panel.grid.major = element_line(colour = "gray80", size=0.3),
            legend.title=element_blank(),
            legend.text=element_text(size=35),
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(2, "cm"),
            strip.background = element_rect(fill="snow1"),
            plot.margin=unit(c(1,1,1.5,1), "lines"))  
    path=("~/Documents/Rhesus_monkey_graph/Sarina/Tracts/FA.pdf")
    ggsave(filename = path, plot = FA, width =19, height = 15,family="CM Roman")
  }
  
  if (type == c("FW")){
    predicted_measure =as.data.frame(matrix(NA,  ncol=24, nrow=8))
    j <- 1
    for (i in seq(1,24,by = 3)){
      predicted_measure[1:24,i] <- FW[i+2] + FW[i]*age*exp(-(FW[i+1]*age))
      #lines(age,predicted_measure,type='l', col =color[j], lwd=1.5)
      j <- j+1
    }
    g=predicted_measure[ , colSums(is.na(predicted_measure)) == 0]
    colnames(g)=c("CC","CB","SLF II","ALIC","PLIC","SLF III","AC","AF")
    h=data.frame(age, g)
    attach(h)
    hp=melt(h)
    mdata <- melt(h, id=c("age"))
    require(ggplot2)
    FW=ggplot() +
      #geom_line(size = 2, color = variable) +
      geom_line(data = mdata, aes(x = age, y = value, color=variable),size=2.2,alpha=0.7)+
      scale_color_manual(values = color)+
      scale_x_continuous(name="Age (years)") +
      scale_y_continuous(name="FW")+
      coord_cartesian(ylim = c(0, 0.3))+
      coord_cartesian(xlim = c(4, 27))+
      ggtitle("FW with Age") + 
      theme(panel.background = element_blank(),
            axis.text.x=element_text(size=45, color="black"),
            axis.text.y=element_text(size=45, color="black"),
            axis.title.y=element_text(size=60),
            axis.title.x=element_text(size=60),
            plot.title=element_text(size=75),
            panel.border = element_rect(fill = NA, colour = "black"),
            panel.grid.major = element_line(colour = "gray80", size=0.3),
            legend.title=element_blank(),
            legend.text=element_text(size=35),
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(2, "cm"),
            strip.background = element_rect(fill="snow1"),
            plot.margin=unit(c(1,1,1.5,1), "lines"))  
    path=("~/Documents/Rhesus_monkey_graph/Sarina/Tracts/FW.pdf")
    ggsave(filename = path, plot = FW, width = 19, height = 15,family="CM Roman")
  }
}

data <- read.csv("~/Documents/Rhesus_monkey_graph/Sarina/Tracts/Tracts_BU_MGH_LRComb.csv")
data_new <- tbl_df(data)
no_of_columns <- ncol(data_new)
All_tracts <- data.frame()
for (i in seq(from=2, to=no_of_columns, by=3)){
  j <- (i+1)/3
  data_name <- paste("data",j,sep="")
  assign(data_name,select(data_new,Age,i:(i+2)))
  data_slot<-get(data_name)
  p <- colnames(data_new)[i]
  rname <- strsplit(p,"_")
  Result_tract<-Plots_PeakAge(data_slot,rname)
  #rownames(Result_tract) = rname[[1]][1]
  All_tracts <- rbind(All_tracts,Result_tract)
}
FA <- All_tracts[1:24,1]
FW <- All_tracts[1:24,2]
FAt <- All_tracts[1:24,3]

SavePlotPoiss(Result_tract$Result_FA, FA,"FA")
SavePlotPoiss(Result_tract$Result_FW, FW,"FW")
SavePlotPoiss(Result_tract$Result_Fat, FAt,"FAt")

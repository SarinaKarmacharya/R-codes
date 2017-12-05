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

data <- read.csv("~/Documents/Rhesus_monkey_graph/Sarina/Whole_Brain/TBSS_BU_MGH.csv")
data_new <- tbl_df(data)

#no_of_columns <- ncol(data_new)
Plots_PeakAge <- function(data_new,column_name){
  FA <- select(data_new, Age, mean_FA_Scaled_To_MGH)
  FW <- select(data_new, Age, mean_FW_Scaled_To_MGH)
  Fat <- select(data_new, Age, mean_Fat_Scaled_To_MGH)
  Result_FA <- Poisson_Quadratic(FA,c("FA"))
  Result_FW <- Poisson_Quadratic(FW,c("FW"))
  Result_Fat <- Poisson_Quadratic(Fat,c("Fat"))
  Result_final <- data.frame(Result_FA,Result_FW,Result_Fat)
  return (Result_final)
  #write.csv(Result_final,"~/Desktop/TBSS_peak_age.csv",row.names = "TBSS")
}

Poisson_Quadratic <- function(measure,type){
  colnames(measure)[2] <- "mean_measure"
  poiss <- poissonNLSModel(measure)
  quad <- quadraticNLSModel(measure)
  name <- c("WholeBrain",type,".pdf")
  name <- paste(name,collapse="")
  SavePlotPoiss(poiss, name, measure,type)
  result <- list("poiss_peak_age" = poiss$peak_age_poiss, "se_poiss" = poiss$se_peak_age_poiss, "quad_peak_age" = quad$peak_age_quad, "se_quad" = quad$se_peak_age_poiss,"coeff_poiss" = poiss$coeff_poiss)
  #result <- list("poiss_peak_age" = poiss$peak_age_poiss, "se_poiss" = poiss$se_peak_age_poiss, "quad_peak_age" = quad$peak_age_quad, "se_quad" = quad$se_peak_age_poiss)
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


SavePlotPoiss <- function(poiss, name, measure,type){
  
  #errorbar
  coeff_poiss <- poiss$coeff_poiss
  peak_age <- poiss$peak_age_poiss
  se <- poiss$se_peak_age_poiss
  xmax <- peak_age + se
  xmin <- peak_age - se
  x <- c(seq(xmin,xmax, by = 0.01))
  y1 <- coeff_poiss[3] + coeff_poiss[1]*peak_age*exp(-(coeff_poiss[2]*peak_age))
  y <- c(rep(y1,length(x)))
  d<- data.frame(x,y)
  
  #peakage point
  m <- data.frame(peak_age, y1)
  
  #plots
  age <- c(4:27)
  predicted_measure <- coeff_poiss[3] + coeff_poiss[1]*age*exp(-(coeff_poiss[2]*age))
  res <- data.frame(age,predicted_measure)
  path <- c("~/Documents/Rhesus_monkey_graph/Sarina/Whole_Brain/",name)
  path <- paste(path,collapse = "")
  if (type == c("FW")){
    p0 <- ggplot(res,aes(age,predicted_measure))+ 
      geom_line(size = 6, color = "blue") + 
      geom_point(data = measure, aes( x= Age, y= mean_measure), colour = "red", size = 9, alpha=0.7) + 
      #geom_line(data = d,aes(x,y), size = 4, alpha=0.8,color = "grey") +
      geom_point(data = m,aes(peak_age,y1), size = 20, color = "black", shape ="|") +
      scale_x_continuous(name="Age (years)") +
      scale_y_continuous(name="FW")+
      coord_cartesian(ylim = c(0, 0.3))+
      coord_cartesian(xlim = c(0, 30))+
      ggtitle("Whole Brain FW with Age") + 
        theme(panel.background = element_blank(),
              axis.text.x=element_text(size=45, color="black"),
              axis.text.y=element_text(size=45, color="black"),
              axis.title.y=element_text(size=60),
              axis.title.x=element_text(size=60),
              plot.title=element_text(size=80),
              panel.border = element_rect(fill = NA, colour = "black"),
              panel.grid.major = element_line(colour = "gray88", size=0.3),
              strip.background = element_rect(fill="snow1"),
              plot.margin=unit(c(1,1,1.5,1), "lines"))   
                                        
      
    ggsave(filename = path, plot = p0, width = 18, height = 15,family="CM Roman")
  }
  else if (type==c("FA")){
    p1 <- ggplot(res,aes(age,predicted_measure))+ 
      geom_line(size = 6, color = "blue") + 
      geom_point(data = measure, aes( x= Age, y= mean_measure), colour = "red", size = 9, alpha=0.7) + 
      #geom_line(data = d,aes(x,y), size = 4, alpha=0.8,color = "grey") +
      geom_point(data = m,aes(peak_age,y1), size = 20, color = "black", shape ="|") +
      scale_x_continuous(name="Age (years)") +
      scale_y_continuous(name="FA")+
      ggtitle("Whole Brain FA with Age") + 
      theme(panel.background = element_blank(),
            axis.text.x=element_text(size=45, color="black"),
            axis.text.y=element_text(size=45, color="black"),
            axis.title.y=element_text(size=60),
            axis.title.x=element_text(size=60),
            plot.title=element_text(size=80),
            panel.border = element_rect(fill = NA, colour = "black"),
            panel.grid.major = element_line(colour = "gray88", size=0.3),
            strip.background = element_rect(fill="snow1"),
            plot.margin=unit(c(1,1,1.5,1), "lines"))+
            coord_cartesian(ylim = c(0.3, .8))
            #coord_cartesian(xlim = c(0., 30))
    
      ggsave(filename = path, plot = p1,width = 18, height = 15,family="CM Roman")
  }
  else if (type==c("Fat")){
    p2 <- ggplot(res,aes(age,predicted_measure))+ 
      geom_line(size = 6, color = "blue") + 
      geom_point(data = measure, aes( x= Age, y= mean_measure), colour = "red", size = 9, alpha=0.7) + 
      #geom_line(data = d,aes(x,y), size = 4, alpha=0.8,color = "grey") +
      geom_point(data = m,aes(peak_age,y1), size = 20, color = "black", shape ="|") +
      scale_x_continuous(name="Age (years)") +
      scale_y_continuous(name="FAt")+
      
      ggtitle("Whole Brain FAt with Age") + 
      theme(panel.background = element_blank(),
            axis.text.x=element_text(size=45, color="black"),
            axis.text.y=element_text(size=45, color="black"),
            axis.title.y=element_text(size=60),
            axis.title.x=element_text(size=60),
            plot.title=element_text(size=80),
            panel.border = element_rect(fill = NA, colour = "black"),
            panel.grid.major = element_line(colour = "gray88", size=0.3),
            strip.background = element_rect(fill="snow1"),
            plot.margin=unit(c(1,1,1.5,1), "lines"))+
      coord_cartesian(ylim = c(0.3, 0.8))
            #coord_cartesian(xlim = c(0., 30))
    
      ggsave(filename = path, plot = p2,width = 18, height = 15,family="CM Roman")
  }
}
Results_Final = Plots_PeakAge(data_new)

#Do not run this with the entire code
#Computing rate change and %rate change
B <- Results_Final$coeff_poiss[2]
A <- Results_Final$coeff_poiss[1]
C <- Results_Final$coeff_poiss[3]
FA_x <- c(Results_Final$poiss_peak_age[1]:27)
FA_pred <- C + A*FA_x*exp(-B*FA_x)
FA_rate <- -A * exp(-B*FA_x)*  (B*FA_x - 1)
# FA_rate <- -B * exp(-C*FA_x) * (C*FA_x - 1) = 

B <- Results_Final$coeff_poiss.1[2]
A <- Results_Final$coeff_poiss.1[1]
C <- Results_Final$coeff_poiss.1[3]
FW_x <- c(Results_Final$poiss_peak_age.1[1]:27)
FW_rate <- -A * exp(-B*FW_x)*  (B*FW_x - 1)
FW_pred <- C + A*FW_x*exp(-B*FW_x)

B <- Results_Final$coeff_poiss.2[2]
A <- Results_Final$coeff_poiss.2[1]
C <- Results_Final$coeff_poiss.2[3]
Fat_x <- c(Results_Final$poiss_peak_age.2[1]:27)
Fat_rate <- -A * exp(-B*Fat_x)*  (B*Fat_x - 1)
Fat_pred <- C + A*Fat_x*exp(-B*Fat_x)

FA_data=data.frame(FA_x, (FA_rate/FA_pred * 100));
FA_graph=ggplot() +
  geom_point(data = FA_data, aes( x= FA_x, y= (FA_rate/FA_pred * 100)), color = "#0D8B0F", size = 9, alpha=1) + 
  scale_color_manual(values = color)+
  scale_x_continuous(name="Age (years)") +
  scale_y_continuous(name="% Rate of Change of FA")+
  #coord_cartesian(ylim = c(0, 0.3))+
  coord_cartesian(xlim = c(9, 27))+
  ggtitle("FA % Rate of Decrease") + 
  theme(panel.background = element_blank(),
        axis.text.x=element_text(size=45, color="black"),
        axis.text.y=element_text(size=45, color="black"),
        axis.title.y=element_text(size=60),
        axis.title.x=element_text(size=60),
        plot.title=element_text(size=60),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_line(colour = "gray80", size=0.3),
        legend.title=element_blank(),
        legend.text=element_text(size=28),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(2, "cm"),
        strip.background = element_rect(fill="snow1"),
        plot.margin=unit(c(1,1,1.5,1), "lines"))  
path=("~/Documents/Rhesus_monkey_graph/Sarina/Whole_Brain/FA_decrease.pdf")
ggsave(filename = path, plot = FA_graph, width = 20, height = 10,family="CM Roman")

##
FAt_data=data.frame(Fat_x, (Fat_rate/Fat_pred *100));
FAt_graph=ggplot() +
  geom_point(data = FAt_data, aes( x= Fat_x, y= (Fat_rate/Fat_pred *100)), color = "#0D8B0F", size = 9, alpha=1) + 
  scale_color_manual(values = color)+
  scale_x_continuous(name="Age (years)") +
  scale_y_continuous(name="% Rate of Change of FAt")+
  #coord_cartesian(ylim = c(0, 0.3))+
  #coord_cartesian(xlim = c(9, 27))+
  ggtitle("FAt % Rate of Decrease") + 
  theme(panel.background = element_blank(),
        axis.text.x=element_text(size=45, color="black"),
        axis.text.y=element_text(size=45, color="black"),
        axis.title.y=element_text(size=60),
        axis.title.x=element_text(size=60),
        plot.title=element_text(size=60),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_line(colour = "gray80", size=0.3),
        legend.title=element_blank(),
        legend.text=element_text(size=28),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(2, "cm"),
        strip.background = element_rect(fill="snow1"),
        plot.margin=unit(c(1,1,1.5,1), "lines"))  
path=("~/Documents/Rhesus_monkey_graph/Sarina/Whole_Brain/FAt_decrease.pdf")
ggsave(filename = path, plot = FAt_graph, width = 20, height = 10,family="CM Roman")
FAt_graph
##

FW_data=data.frame(FW_x, (FW_rate/FW_pred * 100));
FW_graph=ggplot() +
  geom_point(data = FW_data, aes( x= FW_x, y= (FW_rate/FW_pred * 100)), color = "#0D8B0F", size = 9, alpha=1) + 
  scale_color_manual(values = color)+
  scale_x_continuous(name="Age (years)") +
  scale_y_continuous(name="% Rate of Change of FW")+
  #coord_cartesian(ylim = c(0, 0.3))+
  coord_cartesian(xlim = c(9, 27))+
  ggtitle("FW % Rate of Increase") + 
  theme(panel.background = element_blank(),
        axis.text.x=element_text(size=45, color="black"),
        axis.text.y=element_text(size=45, color="black"),
        axis.title.y=element_text(size=60),
        axis.title.x=element_text(size=60),
        plot.title=element_text(size=60),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_line(colour = "gray80", size=0.3),
        legend.title=element_blank(),
        legend.text=element_text(size=28),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(2, "cm"),
        strip.background = element_rect(fill="snow1"),
        plot.margin=unit(c(1,1,1.5,1), "lines"))  
path=("~/Documents/Rhesus_monkey_graph/Sarina/Whole_Brain/FW_increase.pdf")
ggsave(filename = path, plot = FW_graph, width = 20, height = 10,family="CM Roman")


# plot(FA_x, (FA_rate/FA_pred * 100), main = "FA % rate of decrease", xlab = "Age", ylab = "% rate of change of FA" )
# plot(Fat_x, (Fat_rate/Fat_pred *100), main = "FAt %rate of decrease", xlab = "Age", ylab = "% rate of change of FAt" )
# plot(FW_x, (FW_rate/FW_pred * 100), main = "FW % rate of increase", xlab = "Age", ylab = "% rate of change of FW" )

# plot(FA_x, FA_rate, main = "FA rate of decrease", xlab = "Age", ylab = "rate of change of FA" )
# plot(FW_x, FW_rate, main = "FW rate of increase", xlab = "Age", ylab = "rate of change of FW" )
# plot(Fat_x, Fat_rate, main = "FAt rate of decrease", xlab = "Age", ylab = "rate of change of FAt" )

  
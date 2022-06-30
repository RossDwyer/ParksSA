# Script to plot histograms and dispersal kernels generated from tracking data
# Version 1: R Dwyer, 24/05/22 - plots the images ready for publication
# Image used for Supps and some also used for figure 1 

# Loads packages
library(tidyverse)
library(lubridate)
library(VTrack)
library(remora)
library(sf)
library(sp)
library(ggpubr)
library(RColorBrewer)
library(grid)

## Load in the dispersal data files for $Daily, $Weekly and $Monthly subsets
Dispersal_Timescales_EagleRay <- readRDS(file = "Data/Dispersal_Timescales_EagleRay.RDS")
Dispersal_Timescales_Harlequin <- readRDS(file = "Data/Dispersal_Timescales_Harlequin.RDS")
Dispersal_Timescales_Kingfish <- readRDS(file = "Data/Dispersal_Timescales_Kingfish.RDS")
Dispersal_Timescales_Snapper <- readRDS(file = "Data/Dispersal_Timescales_Snapper.RDS")
Dispersal_Timescales_Trevally <- readRDS(file = "Data/Dispersal_Timescales_Trevally.RDS")
Dispersal_Timescales_BTWrasse<- readRDS(file = "Data/Dispersal_Timescales_BTWrasse.RDS")
Dispersal_Timescales_BlueGroper <- readRDS(file = "Data/Dispersal_Timescales_BlueGroper.RDS")
Dispersal_Timescales_BronzeWhaler <- readRDS(file = "Data/Dispersal_Timescales_BronzeWhaler.RDS")
Dispersal_Timescales_DuskyWhaler <- readRDS(file = "Data/Dispersal_Timescales_DuskyWhaler.RDS")
Dispersal_Timescales_WhiteShark <- readRDS(file = "Data/Dispersal_Timescales_WhiteShark.RDS")

# Check first and last date of detection
max(Dispersal_Timescales_EagleRay$Daily$Day)
max(Dispersal_Timescales_Harlequin$Daily$Day)
max(Dispersal_Timescales_Kingfish$Daily$Day)
max(Dispersal_Timescales_Snapper$Daily$Day)
max(Dispersal_Timescales_Trevally$Daily$Day)
max(Dispersal_Timescales_BTWrasse$Daily$Day)
min(Dispersal_Timescales_BlueGroper$Daily$Day) #(min)
max(Dispersal_Timescales_BronzeWhaler$Daily$Day)
max(Dispersal_Timescales_DuskyWhaler$Daily$Day)
max(Dispersal_Timescales_WhiteShark$Daily$Day) # max

# Function to plot histogram of weekly dispersal distances and the fitted dispersal distance kernel (k D (r), solid line); 
##i.e. the probability density function of the distribution of the dispersal distance travelled by tagged individuals
fplothist <- function(sdata,idist=1,icol){
  
  #sdata = Dispersal_Timescales_BTWrasse$Weekly;idist=.5;icol=1
  
  # Filter to remove 0km distances per week
  sdata <- sdata %>%
    filter(maxDistkm!=0) 
  
  # Generate the histogram
  kplot <- sdata %>% 
    ggplot(aes(maxDistkm)) + 
    geom_histogram(binwidth = idist,
                   fill=icol,colour="black",alpha=.3) +
    xlab("") +
    ylab("") +
    ggtitle(paste0(sdata[3,1])) +
    theme_classic(base_size = 14) +
    theme(legend.text=element_text(size=11),
          axis.text.x = element_text(color="black"),
          axis.text.y = element_text(color="black"),
          strip.background = element_blank(),
          panel.background = element_blank(),
          strip.text.y = element_text(angle = 0),
          plot.margin = unit(c(0,0.1,0,0), "lines"),
          plot.title=element_text(#hjust=1, vjust=0.5, 
            #margin=margin(t=40,b=-30),
            size=14))
  
  kplot2 <- kplot + 
    geom_density(kernel="gaussian",bw = "bcv",aes(y=idist * ..count..),size=1.5,adjust=4)
  
  return(kplot2)
  # 
  # #Estimate the Neg Bin distribution
  #   set.seed(1)
  #   distances <- seq(range(sdata$maxDistkm)[1],range(sdata$maxDistkm)[2],length=100)
  #   sd_dist <- sd(sdata$maxDistkm)
  #   mean_dist <- mean(sdata$maxDistkm)    
  #   median_dist <- median(sdata$maxDistkm) 
  #   cv_dist = sd_dist/mean_dist # coefficient of variation
  #   p_dist = 1/cv_dist/sd_dist # probability of sampling success to represent SD
  #   r_dist = mean_dist * p_dist / (1-p_dist) # change of mean to sample in order to maintain actual mean
  #   
  #   #probs_disp <- dnbinom(distances,size=r_dist,prob = p_dist) # calculate probabilities of fish dispersal
  #   #rprobs_disp <- probs_disp/max(probs_disp)
  #   
  #   probs_disp <- dnbinom(0:3,mu=mean_dist,size = p_dist) # calculate probabilities of fish dispersal
  #   rprobs_disp <- probs_disp *50 # For plotting to be on same y axis
  #   
  #   probdf<- data.frame(distance=0:3,
  #                          probs_disp,rprobs_disp)
  #   
  #   # Generate the plot
  #   kplot2 <- kplot + 
  #     geom_line(data=probdf,aes(distance,rprobs_disp),
  #       colour="black",alpha=1,lwd=2) 
  #   
  #   return(kplot2)
  }

# Run the function on all our species to generate the plots
p1 <- fplothist(sdata = Dispersal_Timescales_BTWrasse$Weekly,idist=.5,icol=1)
p2 <- fplothist(sdata = Dispersal_Timescales_Harlequin$Weekly,idist=.5,icol=2)
p3 <- fplothist(sdata = Dispersal_Timescales_Trevally$Weekly,idist=1,icol=3)
p4 <- fplothist(sdata = Dispersal_Timescales_Snapper$Weekly,idist=1,icol=4)
p5 <- fplothist(sdata = Dispersal_Timescales_BlueGroper$Weekly,idist=.5,icol=5)
p6 <- fplothist(sdata = Dispersal_Timescales_Kingfish$Weekly,idist=2,icol=6)
p7 <- fplothist(sdata = Dispersal_Timescales_EagleRay$Weekly,idist=1,icol=7)
p8 <- fplothist(sdata = Dispersal_Timescales_BronzeWhaler$Weekly,idist=1,icol=8)
p9 <- fplothist(sdata = Dispersal_Timescales_DuskyWhaler$Weekly,idist=1,icol=9)
p10 <- fplothist(sdata = Dispersal_Timescales_WhiteShark$Weekly,idist=1,icol=10)

# Arrange all the plots in at 3x4 figure
ddhist_all <- ggarrange(p1,p2,p3,p4,p5,p6,
                         p7,p8,p9,p10,
                        nrow=4,ncol=3,
                        labels = NULL) # For bony fish
# add comon x and y labels
ddhist_all <- annotate_figure(ddhist_all, 
                left = textGrob("Relative frequency", 
                                rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Weekly distance (km)", gp = gpar(cex = 1.3)))
# Save to jpeg
ggsave(plot=ddhist_all,
       filename="Images/fitted kernel fish.png",
       width = 23,height = 26, units = "cm")


#################

# For the Main manuscript plot Western Blue Groper, Silver trevally, Dusky shark, white shark
# Arrange all the plots in at 3x4 figure
ddhist_sub <- ggarrange(p5,p3,p9,p10,
                        nrow=4,ncol=1,
                        labels = NULL) # For bony fish
# add comon x and y labels
ddhist_sub <- annotate_figure(ddhist_sub, 
                              left = textGrob("Relative frequency", 
                                              rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                              bottom = textGrob("Weekly distance (km)", gp = gpar(cex = 1.3)))
# Save to jpeg
ggsave(plot=ddhist_sub,
       filename="Images/fitted kernel subset.png",
       width = 8,height = 26, units = "cm")

install.packages("ggthemes")
theme_base(base_size = 16, base_family = "")


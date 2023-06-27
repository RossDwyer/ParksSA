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

## For each species, generate summary table of weekly maximum distance
## Need thjis for predictive lines and Table 1
ftable1 <- function(sdata){
   #sdata = Dispersal_Timescales_BTWrasse$Weekly
  
  # Filter to remove 0km distances per week
  sdata_sum <- sdata %>%
    group_by(transmitter_id) %>% 
    summarize(maxmaxdist=max(maxDistkm),
              Count=n()) %>% 
    summarize(meanmaxdist=round(mean(maxmaxdist),1),
              sdmaxdist=round(sd(maxmaxdist),1),
              medianmaxdist=round(median(maxmaxdist),1),
              maxmaxdist=round(max(maxmaxdist),1),
              weeklyest=round(sum(Count),1)) %>% data.frame()
  
  return(sdata_sum)
}
  
# Run  function all our species
ftable1(sdata = Dispersal_Timescales_BTWrasse$Weekly)
ftable1(sdata = Dispersal_Timescales_Harlequin$Weekly)
ftable1(sdata = Dispersal_Timescales_Trevally$Weekly)
ftable1(sdata = Dispersal_Timescales_Snapper$Weekly)
ftable1(sdata = Dispersal_Timescales_BlueGroper$Weekly)
ftable1(sdata = Dispersal_Timescales_Kingfish$Weekly)
ftable1(sdata = Dispersal_Timescales_EagleRay$Weekly)
ftable1(sdata = Dispersal_Timescales_BronzeWhaler$Weekly)
ftable1(sdata = Dispersal_Timescales_DuskyWhaler$Weekly)
ftable1(sdata = Dispersal_Timescales_WhiteShark$Weekly) # looks good


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot Figure 2 histograms with Neg Bin dist #
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set common colour pallette
nspecies = 10
icols <- RColorBrewer::brewer.pal(nspecies,"Paired")

# Function to plot movement profile
plotMovehistogram <- function(sdata,species.to.plot,icol){
  
  # set metrics
  SampleSize = length(unique(sdata$transmitter_id)) # no tags in plot title
  hist.resolution <- 1
  alpha.col <- 0.55
  
  # Filter to remove 0km distances per week
  sdata <- sdata %>%
    filter(maxDistkm!=0) 
  
  # Draw the histogram
  hist(sdata$maxDistkm,
       col = alpha(icol,alpha.col),
       border=alpha(icol,alpha.col),
       breaks = seq(0, max(sdata$maxDistkm), length.out = 11),
       freq = FALSE, # FALSE = Relative freq, TRUE = actual
       ylab = '',
       xlab='',
       main ='',
       yaxt="n",
       cex.main=1, 
       cex.sub=1,
       xlim=c(0,round(max(sdata$maxDistkm))))
  axis(side = 2,las = 2) ## Rotate the y labels
  title(main=paste0(str_replace_all(species.to.plot,"_"," ")," = ",SampleSize),adj = 0,font.main = 1) # adds the title
  
  # Now add the lines based on mean, median, sd, max
  mean_dist <- ftable1(sdata)$meanmaxdist
  nsd_dist <- ftable1(sdata)$sdmaxdist
  sd_dist <- ftable1(sdata)$sdmaxdist # not sure the difference between sddist and nsddist but sd dist used for the normal dist and nsddidt used for the nb
  median_dist <- ftable1(sdata)$medianmaxdist
  max_dist <- ftable1(sdata)$maxmaxdist
  distances <- seq(0,max_dist,length=100)
  
  # Add the vertical lines to the plot
  abline(v=mean_dist,col='blue',lty=1,lwd=2)
  abline(v=median_dist,col='red',lty=2,lwd=2)
  
  # Add the predictive line by sampling from negative binomial
  cv_dist = sd_dist/mean_dist # coefficient of variation
  p_dist = 1/cv_dist/sd_dist # probability of sampling success to represent SD
  r_dist = mean_dist * p_dist / (1-p_dist); # change of mean to sample in order to maintain actual mean
  
  ## NILS -- These 3 lines of code generate NaN vals instead of numbers. I'm not sure what's going on here as I'm not familiar with these functions
  probs_disp <- pnbinom(distances,size=r_dist,prob = p_dist) # calculate probabilities of larval dispersal
  probs_disp <- probs_disp/max(probs_disp)
  dprobs_disp <- dnbinom(distances,size=r_dist,prob = p_dist) ## THis is what we need to visualise !
  
  # Add the predicted smoothed lines (Doesnt work as NaNs produced)
  #lines(distances,dprobs_disp,lwd=2)

} # end species specific function

########################

# Draw the movement histograms for all species
jpeg(file ="Images/Figure 2 Movement histogram_all_V1.jpg",
     quality=100, bg="white", res=200, 
     height=2300, width=2300,pointsize=18) 

# For high res PDF
# pdf(file = "Images/Movement profiles_fish.pdf",
#     width = 8.74, height = 5.91)
#width = 220, height = 150, units = "mm",
#res=600)

# Generate the plot by running function for each species
par(mfrow=c(4,3),las=1,
    oma = c(3, 3, 0, 0), # make room (i.e. the 4's) for the overall x and y axis titles
    mar = c(2, 2, 2, 2), # make the plots be closer together
    xpd = FALSE)  # Prevent abline from plotting outside margins

plotMovehistogram(sdata=Dispersal_Timescales_BTWrasse$Weekly,"Bluethroat_Wrasse",icol=icols[1])
plotMovehistogram(sdata=Dispersal_Timescales_Harlequin$Weekly,"Harlequin",icol=icols[2])
plotMovehistogram(sdata=Dispersal_Timescales_Trevally$Weekly,"Silver_Trevally",icol=icols[3])
plotMovehistogram(sdata=Dispersal_Timescales_Snapper$Weekly,"Snapper",icol=icols[4])
plotMovehistogram(sdata=Dispersal_Timescales_BlueGroper$Weekly,"Western_Blue_Groper",icol=icols[5])
plotMovehistogram(sdata=Dispersal_Timescales_Kingfish$Weekly,"Yellowtail_Kingfish",icol=icols[6])
plotMovehistogram(sdata=Dispersal_Timescales_EagleRay$Weekly,"Southern_Eagle_Ray",icol=icols[7])
plotMovehistogram(sdata=Dispersal_Timescales_BronzeWhaler$Weekly,"Bronze_Whaler",icol=icols[8])
plotMovehistogram(sdata=Dispersal_Timescales_DuskyWhaler$Weekly,"Dusky_Whaler",icol=icols[9])
plotMovehistogram(sdata=Dispersal_Timescales_WhiteShark$Weekly,"White_Shark",icol=icols[10])

# add the common x and y label
xlabel = "Movement distance (km)"
ylabel = "Relative frequency"

mtext(xlabel, side = 1, outer = TRUE, line = 1, cex=1)
par(las=0)
mtext(ylabel, side = 2, outer = TRUE, line = 1, cex=1)

dev.off()


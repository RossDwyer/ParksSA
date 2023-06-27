# Script to plot individual movement profiles generated from tracking data
# Version 1: Nils Krueck, 31/10/21
# Version 2: R Dwyer, 24/05/22 - plots the images ready for publication
# Image used for Supps and some also used for figure 1 

library(stringr)

rm(list = ls()) # remove all parameters from workspace
data.folder <- paste0(getwd(),'/Data/Movement_Profiles/') #'C:/Users/nkrueck/Documents/Github/ParksSA/Data/'
#setwd(data.folder)
resolution <- 100 # in m
type <- 'Measured' # 'Measured' or 'Idealized'

# get data on movement distances
if (resolution > 1){
  probsi <- readRDS(paste0(data.folder,'IndividualMovementProbabilities_',type,'_',resolution,'m.RDS'))
  probsm <- readRDS(paste0(data.folder,'MeanMovementProbabilities_',type,'_',resolution,'m.RDS'))
} else {
  probsi <- readRDS(paste0(data.folder,'IndividualMovementProbabilities_',type,'.RDS'))
  probsm <- readRDS(paste0(data.folder,'MeanMovementProbabilities_',type,'.RDS'))
}
  
save.folder <- paste0(getwd(),'/Dispersal_kernels/') #'C:/Users/nkrueck/Documents/Github/SA_MPA_model/Figures/'
species.names <- names(probsm)


# Load in the Weekly movements for all species
mean_max_distance_m_weekly <- read.csv("R Code/Generate_movement_profiles/mean_max_distance_m_weekly.csv")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot movement profiles ####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Function to plot movement profile
plotMoveprofile <- function(species.to.plot="Western_Blue_Groper",icol){
    
  #species.to.plot="Snapper"; icol=2
  
  #species.to.plot="Western_Blue_Groper"
  probsi_s <- probsi[[which(!is.na(match(species.names,species.to.plot)))]]
  probsm_s <- probsm[[which(!is.na(match(species.names,species.to.plot)))]]

  p = 2 # only plot "weekly data

    period <- names(probsm_s)[p]
      
    for (i in 1:length(probsi_s[[p]])){
      SampleSize= length(probsi_s[[p]])
      
      mdistances <- round(probsm_s[[p]]$Distance_Metres/resolution)*resolution
      idistances <- round(probsi_s[[p]][[i]]$Distance_Metres/resolution)*resolution #probsi_s[[p]][[i]]$Distance_Metres
      iprobs <- probsi_s[[p]][[i]]$Probability_Occurrence
      iprobs.locs <- which(!is.na(match(mdistances,idistances)))
      #uneven.locs <- which(round(idistances)/resolution!=round(idistances/resolution))
      plot.probs <- rep(0,length(probsm_s[[p]]$Distance_Metres))
      plot.probs[iprobs.locs] <- iprobs[]/max(iprobs,na.rm=TRUE)
      
      if(i == 1) {
        plot(mdistances/1000,plot.probs,
             type='l',
             col=icol,
             xlab = "Distance from origin (km)",
             ylab = "Relative likelihood of occurrence",
             main = "",
             las=1,
             ylim=c(0,1),
             #xaxt = "n", 
            # yaxt = "n", 
             lwd=0.7,
             bty='l',
             cex.main=1, 
             cex.sub=1)
        title(main=paste0(str_replace_all(species.to.plot,"_"," ")," = ",SampleSize),adj = 0,font.main = 1) # adds the title
        
      } else {
        lines(probsm_s[[p]]$Distance_Metres/1000,plot.probs,col=icol,lwd=0.7)}
        #lines(idistances,iprobs,col='grey50')}
      
    } # individual loop
    # PLots the means
    # lines(mdistances/1000,probsm_s[[p]]$Probability_Occurrence/
    #         max(probsm_s[[p]]$Probability_Occurrence,na.rm=TRUE),
    #       col="black",lwd=2)
    # 
    # plots the max mean distance
    mean_max_distance_m_weekly.sp <- mean_max_distance_m_weekly %>% 
      filter(common.name==species.to.plot) 
    mean.max.dist <- mean_max_distance_m_weekly.sp$mean.max.dist
    mean.max.range <- c(-mean.max.dist/2,mean.max.dist/2)
    mean.max.01 <- ifelse(mdistances < mean.max.range[1],0,
           ifelse(mdistances > mean.max.range[2],0,
                  1))
    
    lines(mdistances/1000,mean.max.01,
          col="black",lwd=2)
    
    #text(4, 0.9, paste0("n = ",length(probsi_s[[p]]), cex=1.5))
    # end plot
    # dev.off()
    
} # species loop

# Set colour pallette
nspecies = 10
cols <- RColorBrewer::brewer.pal(nspecies,"Paired")

# Draw the movement profiles for all species
jpeg(file ="Images/Figure 3 Movement profiles_all_V2.jpg",
     quality=100, bg="white", res=200, 
     height=2300, width=2000,pointsize=18) 
# pdf(file = "Images/Movement profiles_fish.pdf",
#     width = 8.74, height = 5.91)
#width = 220, height = 150, units = "mm",
#res=600)

par(mfrow=c(4,3),las=1,
    oma = c(3, 3, 0, 0), # make room (i.e. the 4's) for the overall x and y axis titles
    mar = c(2, 2, 2, 2), # make the plots be closer together
    xpd = TRUE)  # Allow legend plotting in margins

plotMoveprofile("Bluethroat_Wrasse",icol=cols[1])
plotMoveprofile("Harlequin",icol=cols[2])
plotMoveprofile("Silver_Trevally",icol=cols[3])
plotMoveprofile("Snapper",icol=cols[4])
plotMoveprofile("Western_Blue_Groper",icol=cols[5])
plotMoveprofile("Yellowtail_Kingfish",icol=cols[6])
plotMoveprofile("Southern_Eagle_Ray",icol=cols[7])
plotMoveprofile("Bronze_Whaler",icol=cols[8])
plotMoveprofile("Dusky_Whaler",icol=cols[9])
plotMoveprofile("White_Shark",icol=cols[10])

xlabel = "Distance from origin (km)"
ylabel = "Relative likelihood of occurrence"

mtext(xlabel, side = 1, outer = TRUE, line = 1, cex=1)
par(las=0)
mtext(ylabel, side = 2, outer = TRUE, line = 1, cex=1)

dev.off()

#

# Draw a subset of movement profiles for the paper
png(file ="Images/Movement profiles_subset.jpg",
    width = 8,height = 26, units = "cm",res=300) 
# pdf(file = "Images/Movement profiles_fish.pdf",
#     width = 8.74, height = 5.91)
#width = 220, height = 150, units = "mm",
#res=600)

par(mfrow=c(4,1),las=1,
    oma = c(3, 3, 0, 0), # make room (i.e. the 4's) for the overall x and y axis titles
    mar = c(2, 2, 2, 2), # make the plots be closer together
    xpd = TRUE)  # Allow legend plotting in margins

plotMoveprofile("Western_Blue_Groper",icol=cols[5])
plotMoveprofile("Silver_Trevally",icol=cols[3])
plotMoveprofile("Dusky_Whaler",icol=cols[9])
plotMoveprofile("White_Shark",icol=cols[10])

xlabel = "Distance from origin (km)"
ylabel = "Relative likelihood of occurrence"

mtext(xlabel, side = 1, outer = TRUE, line = 1, cex=1)
par(las=0)
mtext(ylabel, side = 2, outer = TRUE, line = 1, cex=1)

dev.off()

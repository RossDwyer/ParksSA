# Script to plot concept diagram for paper
# Version 1: R Dwyer, 26/05/22 - plots the images ready for publication
gaussplot <- function(mean_dist,sd_dist){
  
  set.seed(1)
  #mean_dist = 50 ; sd_dist=15
  distances <- 0:100
  probs_disp <- dnorm(distances, mean = mean_dist, sd = sd_dist) # calculate probabilities of fish dispersal
  rprobs_disp <- probs_disp/max(probs_disp)
  gaus_dist <- data.frame(Shape="gaussian", distances,probs_disp,rprobs_disp)
  
  saveRDS(gaus_dist,file="R Code/MS Figures/gaus_dist.RDS")
  
  # Generate the plot
  plot(distances,rprobs_disp,type='l',bty="l",xlab="",ylab="")
  abline(v=mean_dist,lty=2, lwd=1.5,col="grey50")
  
  #mydf <- data.frame(distances,rprobs_disp,rprobs_disp100=round(rprobs_disp*100))
}
negbinplot <- function(mean_dist,sd_dist){
  
  mean_dist = 50 ; sd_dist=45
  set.seed(1)
  distances <- 0:100
  cv_dist = sd_dist/mean_dist # coefficient of variation
  p_dist = 1/cv_dist/sd_dist # probability of sampling success to represent SD
  r_dist = mean_dist * p_dist / (1-p_dist) # change of mean to sample in order to maintain actual mean
  
  probs_disp <- dnbinom(distances,
                        size=r_dist,
                        prob = p_dist) # calculate probabilities of fish dispersal
  rprobs_disp <- probs_disp/max(probs_disp)
  
  negbinR_dist <- data.frame(Shape="negbinR", distances,probs_disp,rprobs_disp)
  saveRDS(negbinR_dist,file="R Code/MS Figures/negbinR_dist.RDS")
  
  # Generate the plot
  plot(distances,rprobs_disp,type='l',bty="l",xlab="",ylab="")
  abline(v=mean_dist,lty=2, lwd=1.5,col="grey50")
  
  ## added to calculate the median
  n_disp0 <- rnbinom(1000,size=r_dist,prob = p_dist) # calculate probabilities of larval dispersal
  med_dist<- median(n_disp0)
  abline(v=med_dist,lty=2, lwd=1.5,col="blue")
}
negbinplot_rev <- function(mean_dist,sd_dist){
  set.seed(1)
  distances <- 0:100
  cv_dist = sd_dist/mean_dist # coefficient of variation
  p_dist = 1/cv_dist/sd_dist # probability of sampling success to represent SD
  r_dist = mean_dist * p_dist / (1-p_dist) # change of mean to sample in order to maintain actual mean
  probs_disp <- dnbinom(distances,size=r_dist,prob = p_dist) # calculate probabilities of larval dispersal
  rprobs_disp <- probs_disp/max(probs_disp)
  distances <- -distances+100
  
  negbinL_dist <- data.frame(Shape="negbinL", distances,probs_disp,rprobs_disp)
  saveRDS(negbinL_dist,file="R Code/MS Figures/negbinL_dist.RDS")
  
  # Generate the plot
  plot(distances,rprobs_disp,type='l',bty="l",xlab="",ylab="")
  abline(v=mean_dist,lty=2, lwd=1.5,col="grey50")

  ## added to calculate the median
  n_disp0 <- rnbinom(1000,size=r_dist,prob = p_dist) # calculate probabilities of larval dispersal
  med_dist<- 100+(-1*(median(n_disp0)))
  abline(v=med_dist,lty=2, lwd=1.5,col="blue")
  
  
  }

# Draw the image
png(file ="Images/Fig 1a conceptual.png",
    width = 6,height = 13, units = "cm",res=300) 
par(mfrow=c(3,1),mar = c(2, 2, 1, 1),oma = c(4, 4, 0, 0),las=1)
#negbinplot(50,7.1)
gaussplot(50,15)
negbinplot(50,45)
negbinplot_rev(50,45)
mtext('Distance (km)', side = 1, outer = TRUE, line = 2)
par(las=0)
mtext('Frequency', side = 2, outer = TRUE, line = 2)
dev.off()

############################

Nplot <- function(){
  
  #mean_dist = 50 ; sd_dist=15
  distances <- 0:100
  nprot <- distances
  
  # Generate the plot
  plot(distances,nprot,type='l',bty="l",xlab="",ylab="")
  abline(v=mean_dist,lty=2, lwd=1.5,col="grey50")
  
  #mydf <- data.frame(distances,rprobs_disp,rprobs_disp100=round(rprobs_disp*100))
}
Nplot_pos <- function(x){
  #mean_dist = 50 ; sd_dist=15
  distances <- 1:100
  nprot <- pexp(1:100, rate = x)
  # Generate the plot
  plot(distances,nprot,type='l',bty="l",xlab="",ylab="")
  abline(v=mean_dist,lty=2, lwd=1.5,col="grey50")
}
Nplot_exp<- function(){
  #mean_dist = 50 ; sd_dist=15
  distances <- 1:100
  nprot <- (distances)^2.5
  # Generate the plot
  plot(distances,nprot,type='l',bty="l",xlab="",ylab="")
  abline(v=mean_dist,lty=2, lwd=1.5,col="grey50")
}


# Draw the image
png(file ="Images/Fig 1b conceptual.png",
    width = 6,height = 13, units = "cm",res=300) 

par(mfrow=c(3,1),mar = c(2, 2, 1, 1),oma = c(4, 4, 0, 0),las=1)
#negbinplot(50,7.1)
Nplot()
Nplot_pos(x=.13)
Nplot_exp()
mtext('Distance (km)', side = 1, outer = TRUE, line = 2)
par(las=0)
mtext('% individuals protected', side = 2, outer = TRUE, line = 2)

dev.off()


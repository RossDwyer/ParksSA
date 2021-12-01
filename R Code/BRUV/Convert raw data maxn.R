## MaxN standardization for BRUV observation frequencies
## Code will take Max Ns and plug in correct zeros for each site for our species of interest

library(ggplot2)
library(scales)
library(dplyr)
library(broom)

source('R Code/summarySE.R') # Loads bespoke functions for subsetting and plotting data

# Location where the raw acoustic telemetry and Bruv data are stored
datafolder <- "/Users/uqrdwye2/Dropbox/shark_mpa_model_v400/SA/DEW Marine Parks project/"

# load the BRUVS data
maxndata <- read.csv(paste0(datafolder,"BRUVS data/SA.Synthesis.MaxN.csv"))

unique(maxndata$genus.species)
# Function to generate species specific bruv data with correct 'absences
fextract_sp <- function(data,singlesp){
  
  #data = maxndata
  #singlesp = "Myliobatis tenuicaudatus"
  
  maxndata_spec <- data %>% filter(genus.species ==singlesp)  # Extract data for single species
  all_drops <- unique(as.character(data$sample))   # What are the names of the BRUV drops?
  
  # Which drops were in a site with one of our named species, but didnt see any of this species on a survey? 
  # I.e. generates the 'absences'
  maxndata_nospec <- data %>% filter(genus.species != singlesp)  # Extract data for single species
  nosspec_setids <- sort(unique(as.character(maxndata_nospec$sample))) # what are the set ids where species other than our fish speces were spotted  
  nosinglesp_setids <- nosspec_setids[nosspec_setids %!in% maxndata_spec$sample] # Extract set_ids not present in the single species data (i.e. absences)
  maxndata_spec_nospecdups <- data %>% filter(sample %in% nosinglesp_setids) # filter absences dataset for these set codes
  maxndata_spec_nospec<- maxndata_spec_nospecdups[!duplicated(maxndata_spec_nospecdups$sample),] # remove the duplicated set ids where more than one species was spotted
  maxndata_spec_nospec <- maxndata_spec_nospec %>% mutate(family = "", # Add the missing columns and the Zeros for these BRUV surveys
                                                          genus = "",
                                                          species = "",
                                                          maxn = 0,
                                                          genus.species = singlesp)
  
  #Assign the correct zeros to the shark bruv data dataframe (bind our shark species, other animals and no animals dataframes)
  maxndata_spec <- rbind(maxndata_spec,maxndata_spec_nospec) %>% arrange(sample) # Arrange by BRUV deployment id
  
  #are there any duplicates?
  
  
  return(maxndata_spec)
}

#From singlesp
genus.sp <- c("Myliobatis tenuicaudatus", #Southern eagle ray
             "Othos dentex", #Harlequin Fish
             "Seriola lalandi", #Yellowtail Kingfish (Missing from BRUV dataset)
             "Chrysophrys auratus", #Snapper 
             "Achoerodus gouldii", #Western Blue Groper 
             "Notolabrus tetricus", #Bluethroat Wrasse 
             "Carcharhinus brachyurus", #Bronze Whaler 
             "Carcharhinus obscurus", #Dusky Whaler  (Missing from BRUV dataset)
             "Pseudocaranx spp", #Silver Trevally (Coded as this in the BRUVS dataset)
             "Carcharodon carcharias") # White Shark



# Run function to generate species specific bruv data with correct 'absences'
maxndata_spec1 <- fextract_sp(maxndata,genus.sp[1]) # Southern eagle ray
maxndata_spec2 <- fextract_sp(maxndata,genus.sp[2]) # Harlequin Fish
maxndata_spec3 <- fextract_sp(maxndata,genus.sp[3]) # Yellowtail Kingfish
maxndata_spec4 <- fextract_sp(maxndata,genus.sp[4]) # Snapper
maxndata_spec5 <- fextract_sp(maxndata,genus.sp[5]) # Western Blue Groper 
maxndata_spec6 <- fextract_sp(maxndata,genus.sp[6]) # Bluethroat Wrasse 
maxndata_spec7 <- fextract_sp(maxndata,genus.sp[7]) # Bronze Whaler
maxndata_spec8 <- fextract_sp(maxndata,genus.sp[8]) # Dusky Whaler 
maxndata_spec9 <- fextract_sp(maxndata,genus.sp[9]) # Silver Trevally
maxndata_spec10 <- fextract_sp(maxndata,genus.sp[10]) # White Shark

# Combine into single dataset
maxndata_spec_all <- rbind(maxndata_spec1,
                           maxndata_spec2,
                           maxndata_spec3,
                           maxndata_spec4,
                           maxndata_spec5,
                           maxndata_spec6,
                           maxndata_spec7,
                           maxndata_spec8,
                           maxndata_spec9,
                           maxndata_spec10)

# Generate summarySE table for all 5 species combined
sumndata_site_all <- maxndata_spec_all %>%
  summarySE(measurevar="maxn", groupvars=c("genus.species")) %>%
  rename(c('maxn' = 'meanmaxn')) 

  ## plot
  ggplot(sumndata_site_all, 
         aes(x = genus.species, y = meanmaxn, alpha = 0.5,legend=FALSE)) + 
    geom_errorbar(width=.1, aes(ymin=meanmaxn-se, ymax=meanmaxn+se), colour="black")+
    labs(title = "Max N with absences", x = "Species", y = "Max N") +
    theme_bw() + 
    geom_bar(stat = "identity")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          legend.position = c(0.8, 0.8))+
    scale_alpha(guide = 'none')




## This code selects those MPAs that intersect with coral reefs 
## (following the request of Current Biology Reviewers)


# For information on coral reef dataset see here- https://www.unep-wcmc.org/policies/general-data-license-excluding-wdpa#data_policy
# MPA dataset provided by Russell Moffitt <Russell.Moffitt@marine-conservation.org> from MPA Atlas

# 1. Load the packages -----------
library(rgdal)
library(rgeos)
library(sp)
library(sf)
library(rmapshaper)
library(magrittr)
library(ggplot2)
library(plotly)
library(rmapshaper)
library(tmap)

## For SA project,  Dont need reef specific info
## But we should filter for only SA Parks

# 2. Load the shapefile -----------
Aus.CAPAD_sf <- read_sf(dsn='GIS/CAPAD2020_marine',layer='CAPAD2020_marine')
#Aus.MarineParks_sf <- read_sf(dsn='GIS',layer='Marine Parks') How do 

### Extract polygon attributes
names(Aus.CAPAD_sf)
unique(Aus.CAPAD_sf$NAME)
unique(Aus.CAPAD_sf$STATE)

# 3. Simplify the shapefile for quick plotting ---------
# Use mapshaper rather than 
Aus.CAPAD_sf_simpl2 <- ms_simplify(Aus.CAPAD_sf, 
                                keep = 0.05, # proportion of points to retain (0-1; default 0.05)
                                keep_shapes = FALSE) # Prevent small polygon features from disappearing at high simplification (default FALSE)

st_crs(Aus.CAPAD_sf_simpl2)

#Transform into WGS84
Aus.CAPAD_sf_simpl2 <- st_transform(Aus.CAPAD_sf_simpl2,crs=4326)

## Plot the AU Wide Polygon - takes ages
# plot(st_geometry(AusCAPAD_sf_simpl2))
# plot(AusCAPAD_sf_simpl2["STATE"])

# 4. Subset for SA only ---------------
## the simplified version
SA.CAPAD_sf_simpl2 <- Aus.CAPAD_sf_simpl2 %>% 
  filter(STATE =="SA") 

# 5. Plot the SA Polygons --------------
#plot(SA.CAPAD_sf_simpl2["TYPE"])

tmap_mode("view")
tm_shape(SA.CAPAD_sf_simpl2) + 
  tm_fill("IUCN", 
          id="NAME", 
          palette = sf.colors(7))

# 6. filter the original resolution for SA MNPs only ----------
SA.MPA_sf_simpl2 <- Aus.CAPAD_sf_simpl2 %>% 
  filter(STATE =="SA")  %>%
  filter(IUCN=="II") %>%
  filter(NAME!="Great Australian Bight")

tmap_mode("view")
tm_shape(SA.MPA_sf_simpl2) + 
  tm_fill("IUCN", 
          id="NAME", 
          palette = sf.colors(7))

# 6. Extract the max width of the SA polygons (taken from the Reefshark code)
MaxDist <- rep(0,nrow(SA.MPA_sf_simpl2))

for(i in 1:nrow(SA.MPA_sf_simpl2)){
  tryCatch(
    expr = {
      MaxDist[i] <- max(st_distance(st_cast(st_geometry(SA.MPA_sf_simpl2[i,]),"POINT"))) # loop from 1 to N or otherwise apply over the features, and you're done.
      message("Iteration ", i, " successful.")
    },
    error = function(e){
      MaxDist[i] <- NA
      message("* Caught an error on itertion ", i)
      print(e)
    })
}

# Combine the files
SAMPA_CatII_width <- data.frame(SA.MPA_sf_simpl2,MaxDist_km=MaxDist/1000)

########################
# 7. What is the minimum distance BETWEEN MPAs -------------------
MinDist.i <- rep(0,nrow(SA.MPA_sf_simpl2)) #83 MPAs
NearestDist <- MinDist.i

for(j in 1:nrow(SA.MPA_sf_simpl2)){
  for(i in 1:nrow(SA.MPA_sf_simpl2)){
    MinDist.i[i] <- st_distance(SA.MPA_sf_simpl2[j,],SA.MPA_sf_simpl2[i,])
  }
  NearestDist[j] <- min(ifelse(MinDist.i==0,NA,MinDist.i),na.rm=T)/1000
  print(j)
}

# Combine the Polygons file with the distance column
SAMPA_CatII_nearest<- data.frame(SAMPA_CatII_width,Nearest_km=NearestDist)
SAMPA_CatII_nearest_sf <- st_as_sf(SAMPA_CatII_nearest) # Save s an sf object for plotting

saveRDS(SAMPA_CatII_nearest_sf,file="Data/SAMPA_CatII_nearest.RDS")

########################
# 8. Plotting the data 

SAMPA_CatII_nearest_sf <- readRDS(file="Data/SAMPA_CatII_nearest.RDS")

tmap_mode("view")
tm_shape(SAMPA_CatII_nearest_sf) + 
  tm_fill("MaxDist_km", 
          title="Maximum width of MPA (km)",
          id="NAME", 
          breaks = c(0,5,10,20,30,50,100,200),
          palette = rev(sf.colors(7)),
          popup.vars=c("Type"="TYPE",
                       "RES_NUMBER"="RES_NUMBER",
                       "Distance to nearest MPA (km)"="Nearest_km", 
                       "Max width (km)"="MaxDist_km", 
                       "Area"="GAZ_AREA"))

tm_shape(SAMPA_CatII_nearest_sf) + 
  tm_fill("Nearest_km", 
          title="Minimum distance between MPAs (km)",
          id="NAME", 
          breaks = c(0,5,10,20,40,60,80),
          palette = sf.colors(7),
          popup.vars=c("Type"="TYPE",
                       "RES_NUMBER"="RES_NUMBER",
                       "Distance to nearest MPA (km)"="Nearest_km", 
                       "Max width (km)"="MaxDist_km", 
                       "Area"="GAZ_AREA"))
#Save the files
write.csv(SAMPA_CatII_nearest,"GIS/CAPAD2020_marine_SA_CatII_nearestkm.csv")
save(SAMPA_CatII_nearest_sf,file="Data/SA Parks_Distances.Rdata")

# Now extract  and plot some summary statistics ---------
###for the max distances of these MPAs 
MPA_maxwidth_med <- round(median(SAMPA_CatII_nearest$MaxDist_km),2)
MPA_maxwidth_mea <- round(mean(SAMPA_CatII_nearest$MaxDist_km),2)

library(ggplot2)
ggplot() + 
  geom_histogram(aes(SAMPA_CatII_nearest$MaxDist_km))+ 
  labs(title="Histogram for SA MPA width", x="Max MPA width in km", y="Count")+
  scale_x_continuous(trans='log2')+ 
  geom_vline(xintercept = c(MPAmed,MPAmea,MPAmod), linetype='dashed',colour='white')+
  geom_text(aes(x=MPA_maxwidth_med, label=paste0("median = ",MPAmed), y=5), size=5, colour="hotpink", angle=90)+
  geom_text(aes(x=MPA_maxwidth_mea, label=paste0("mean = ",MPAmea), y=5), size=5, colour="hotpink", angle=90)

length(which(SAMPA_CatII_nearest$MaxDist_km <= 10))/length(SAMPA_CatII_nearest$MaxDist_km) *100
length(which(SAMPA_CatII_nearest$MaxDist_km <= 20))/length(SAMPA_CatII_nearest$MaxDist_km) *100
length(which(SAMPA_CatII_nearest$MaxDist_km  <= 30))/length(SAMPA_CatII_nearest$MaxDist_km) *100

###

###for the max distances of these MPAs 
MPA_Nearest_km_med <- round(median(SAMPA_CatII_nearest$Nearest_km),2)
MPA_Nearest_km_mea <- round(mean(SAMPA_CatII_nearest$Nearest_km),2)
round(range(SAMPA_CatII_nearest$Nearest_km),2)

?st_distance
## the simplified version
NArch <- SAMPA_CatII_nearest_sf %>% 
  filter(NAME =="Nuyts Archipelago") 
tm_shape(NArch) + 
  tm_fill("IUCN", 
          id="NAME", 
          palette = sf.colors(7),
          popup.vars=c("Type"="TYPE",
                       "Description"="ZONE_TYPE",
                       "Distance to nearest MPA (km)"="MinDist_km", 
                       "Max width (km)"="MaxDist_km", 
                       "Area"="GAZ_AREA"))

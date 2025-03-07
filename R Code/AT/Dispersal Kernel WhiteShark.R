## Dispersal Kernel comparisons
## Code will take various detection datasets for each species and combine with the tag release metadata
## Updated function to only work with SA fish

#install_github("RossDwyer/VTrack", configure.args = "--with-proj-lib=/usr/local/lib/")

library(tidyverse)
library(lubridate)
library(VTrack)
library(remora)
library(sf)
library(sp)
library(ggpubr)

## Get the tag files ready and run the QC -------------

# Location where the raw acoustic telemetry and Bruv data are stored
datafolder <- "/Users/rdwyer2/Dropbox/shark_mpa_model_v400/SA/DEW Marine Parks project/"

#White Sharks
sp_det <- paste0(datafolder,"White sharks/Total_detections.csv")
sp_tagmet <- paste0(datafolder,"White sharks/whiteshark_transmitter_codes.csv") # duplicate sensor tags (some) need to combine. Filter to Gulf stvincent - ignore NSW DPI and Vic

sp_files <- list(det = sp_det,
                 tmeta = sp_tagmet)

## Get the data ready for generating temporal dispersal metrics --------------

# Read in the detection dataset
sp_det_dat <- read.csv(sp_files$det) %>% 
  mutate(Date.and.Time..UTC.=as.POSIXct(Timestamp,tz="UTC")) %>%
  dplyr::select(Transmitter,Date.and.Time..UTC.,Latitude,Longitude,Station.Name)

# Extract only the variables we are interested renaming them to remora format
d.dplyr <- sp_det_dat %>% 
  rename(transmitter_id=Transmitter,
         detection_datetime=Date.and.Time..UTC.) %>%
  mutate(station_name = Station.Name,
         receiver_deployment_longitude = Longitude,
         receiver_deployment_latitude = Latitude) %>%
  mutate(species_common_name = "White Shark",  # Attach species and common name
         species_scientific_name = "Carcharodon carcharias") %>%
  select(species_common_name,species_scientific_name,
         transmitter_id,detection_datetime,station_name,
         receiver_deployment_longitude,receiver_deployment_latitude)

# Remove the SARDI tags
'%!in%' <- function(x,y)!('%in%'(x,y)) # Operator to negate vector of tag ids
sp_tag_dat <- read.csv(sp_files$tmeta)
d.dplyr_id <- d.dplyr %>% filter(transmitter_id %!in% sp_tag_dat$transmitter_id)

# Add time catagories to location summary
location_summary <-  d.dplyr_id %>%
  mutate(species_scientific_name = as.factor(species_scientific_name),
         species_common_name = as.factor(species_common_name),
         Day = date(detection_datetime),
         Week = format(Day, "%Y-%W"),
         Month = format(Day, "%Y-%m")) %>%
  dplyr::select(transmitter_id,
                species_common_name,species_scientific_name,
                detection_datetime,
                station_name,
                receiver_deployment_longitude, 
                receiver_deployment_latitude,
                Day,Week,Month)

# How many stations per shark?
location_summary %>% 
  group_by(transmitter_id) %>% 
  distinct(station_name) %>% count() %>% arrange(desc(n))
  
# Generate kml of a shark
GenerateAnimationKMLFile_Track <- function (sInputFile, sid, sOutputFile, sTrackColour) 
{
  TransmitterList <- unique(sInputFile$transmitter_id)
  sInputFile2 <- sInputFile %>% filter(transmitter_id == sid)
  #sInputFile2$DATETIME <- as.character(sInputFile2$DATETIME)
  outfile <- file(sOutputFile, "wt")
  writeLines("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
             outfile)
  writeLines("<kml xmlns=\"http://www.opengis.net/kml/2.2\"", 
             outfile)
  writeLines("xmlns:gx=\"http://www.google.com/kml/ext/2.2\"", 
             outfile)
  writeLines("xmlns:kml=\"http://www.opengis.net/kml/2.2\"", 
             outfile)
  writeLines("xmlns:atom=\"http://www.w3.org/2005/Atom\">", 
             outfile)
  writeLines("<Document>", outfile)
  writeLines(paste("<name>", sOutputFile, "</name>", sep = ""), 
             outfile)
  writeLines("<StyleMap id=\"msn_track\">", outfile)
  writeLines("<Pair>", outfile)
  writeLines("<key>normal</key>", outfile)
  writeLines("<styleUrl>#sn_track</styleUrl>", outfile)
  writeLines("</Pair>", outfile)
  writeLines("<Pair>", outfile)
  writeLines("<key>highlight</key>", outfile)
  writeLines("<styleUrl>#sh_track</styleUrl>", outfile)
  writeLines("</Pair>", outfile)
  writeLines("</StyleMap>", outfile)
  writeLines("<Style id=\"sn_track\">", outfile)
  writeLines("<IconStyle>", outfile)
  writeLines(paste("<color>", sTrackColour, "</color>", sep = ""), 
             outfile)
  writeLines("<Icon>", outfile)
  writeLines("<href>http://maps.google.com/mapfiles/kml/shapes/track.png</href>", 
             outfile)
  writeLines("</Icon>", outfile)
  writeLines("</IconStyle>", outfile)
  writeLines("<LabelStyle>", outfile)
  writeLines("<scale>0.6</scale>", outfile)
  writeLines(paste("<color>", sTrackColour, "</color>", sep = ""), 
             outfile)
  writeLines("</LabelStyle>", outfile)
  writeLines("<LineStyle>", outfile)
  writeLines(paste("<color>", sTrackColour, "</color>", sep = ""), 
             outfile)
  writeLines("<width>3</width>", outfile)
  writeLines("</LineStyle>", outfile)
  writeLines("</Style>", outfile)
  writeLines("<Style id=\"sh_track\">", outfile)
  writeLines("<IconStyle>", outfile)
  writeLines("<color>ff69deb3</color>", outfile)
  writeLines("<scale>1.33</scale>", outfile)
  writeLines("<Icon>", outfile)
  writeLines("<href>http://maps.google.com/mapfiles/kml/shapes/track.png</href>", 
             outfile)
  writeLines("</Icon>", outfile)
  writeLines("</IconStyle>", outfile)
  writeLines("<LineStyle>", outfile)
  writeLines("<color>ff69deb3</color>", outfile)
  writeLines("<width>3</width>", outfile)
  writeLines("</LineStyle>", outfile)
  writeLines("</Style>", outfile)
  writeLines("<Placemark>", outfile)
  writeLines(paste("<name>", sid, "</name>", sep = ""), outfile)
  writeLines("<styleUrl>#msn_track</styleUrl>", outfile)
  writeLines("<gx:Track>", outfile)
  for (i in 1:nrow(sInputFile2)) {
    writeLines(paste("<when>", substr(sInputFile2$detection_datetime[i], 
                                      1, 10), "T", substr(sInputFile2$detection_datetime[i], 12, 
                                                          19), "Z</when>", sep = ""), outfile)
  }
  for (i in 1:nrow(sInputFile2)) {
    writeLines(paste("<gx:coord>", sInputFile2$receiver_deployment_longitude[i], 
                     " ", sInputFile2$receiver_deployment_latitude[i], " 0</gx:coord>", sep = ""), 
               outfile)
  }
  writeLines("</gx:Track>", outfile)
  writeLines("</Placemark>", outfile)
  writeLines("</Document>", outfile)
  writeLines("</kml>", outfile)
  close(outfile)
}

GenerateAnimationKMLFile_Track(sInputFile=location_summary,
                               sid="A69-9001-17325",
                               sOutputFile="Images/shark1.kml", "cc69deb3")

GenerateAnimationKMLFile_Track(sInputFile=location_summary,
                              sid="A69-9001-17331",
                              sOutputFile="Images/shark2.kml", "cc69deb3")

                              
### Days ------------
# Unite columns to get unique detections at receiver stations within a specified time interval
location_summary_day <- 
  location_summary %>%
  unite("z", species_common_name, transmitter_id, Day, remove = FALSE) %>%
  distinct(z,receiver_deployment_longitude,receiver_deployment_latitude) %>%
  arrange(z)

# Function to calculate great circle distances
fGCdist <- function(x) {
  tempsf <- SpatialPoints(location_summary_day[x, c("receiver_deployment_longitude","receiver_deployment_latitude")], 
                          proj4string = CRS("+proj=longlat +ellps=WGS84"))
  return(max(spDists(tempsf,longlat = TRUE)))
}
out2 <- tapply(1:nrow(location_summary_day), location_summary_day$z, fGCdist) # Run the function on out dayly dataset

dispersal_summary_day <- location_summary_day %>%
  group_by(z) %>%
  dplyr::summarize(n_stations=n()) %>%
  mutate(maxDistkm = round(as.numeric(as.vector(out2)),2)) %>%
  ungroup() %>%
  separate(z, c("species_common_name", "transmitter_id", "Day"), sep = "([._:])")

### Weeks ------------
# Unite columns to get unique detections at receiver stations within a specified time interval
location_summary_week <- 
  location_summary %>%
  unite("z", species_common_name, transmitter_id, Week, remove = FALSE) %>%
  distinct(z,receiver_deployment_longitude,receiver_deployment_latitude) %>%
  arrange(z)

# Function to calculate great circle distances
fGCdist <- function(x) {
  tempsf <- SpatialPoints(location_summary_week[x, c("receiver_deployment_longitude","receiver_deployment_latitude")], 
                          proj4string = CRS("+proj=longlat +ellps=WGS84"))
  return(max(spDists(tempsf,longlat = TRUE)))
}
out2 <- tapply(1:nrow(location_summary_week), location_summary_week$z, fGCdist) # Run the function on out weekly dataset

dispersal_summary_week <- location_summary_week %>%
  group_by(z) %>%
  dplyr::summarize(n_stations=n()) %>%
  mutate(maxDistkm = round(as.numeric(as.vector(out2)),2)) %>%
  ungroup() %>%
  separate(z, c("species_common_name", "transmitter_id", "Week"), sep = "([._:])")

### Months ------------
# Unite columns to get unique detections at receiver stations within a specified time interval
location_summary_month <- 
  location_summary %>%
  unite("z", species_common_name, transmitter_id, Month, remove = FALSE) %>%
  distinct(z,receiver_deployment_longitude,receiver_deployment_latitude) %>%
  arrange(z)

# Function to calculate great circle distances
fGCdist <- function(x) {
  tempsf <- SpatialPoints(location_summary_month[x, c("receiver_deployment_longitude","receiver_deployment_latitude")], 
                          proj4string = CRS("+proj=longlat +ellps=WGS84"))
  return(max(spDists(tempsf,longlat = TRUE)))
}
out2 <- tapply(1:nrow(location_summary_month), location_summary_month$z, fGCdist) # Run the function on out monthly dataset

dispersal_summary_month <- location_summary_month %>%
  group_by(z) %>%
  dplyr::summarize(n_stations=n()) %>%
  mutate(maxDistkm = round(as.numeric(as.vector(out2)),2)) %>%
  ungroup() %>%
  separate(z, c("species_common_name", "transmitter_id", "Month"), sep = "([._:])")

# Save the file as an RDS object
Dispersal_Timescales_WhiteShark <- list(Daily=dispersal_summary_day,
                                     Weekly=dispersal_summary_week,
                                     Monthly=dispersal_summary_month)
saveRDS(Dispersal_Timescales_WhiteShark, file = "Data/Dispersal_Timescales_WhiteShark.RDS") # Save to github

##############################################


# Compute a histogram of distance per month
disp.hist <- Dispersal_Timescales_WhiteShark$Weekly %>%
  ggplot(aes(maxDistkm)) + geom_histogram() +theme_bw()
# Compute a histogram of num receiver stations
stat.hist <- Dispersal_Timescales_WhiteShark$Weekly %>%
  ggplot(aes(n_stations)) + geom_histogram() +theme_bw()
ggarrange(disp.hist,stat.hist)
# 
# 
# lonlat <- "+proj=longlat +datum=WGS84 +no_defs"
# dispersal_summary.xy <- st_as_sf(dispersal_summary, 
#                                 coords = c("receiver_deployment_longitude", 
#                                            "receiver_deployment_latitude")) # Convert pts from a data frame into an sf object, using x and y as coordinates
# st_crs(dispersal_summary.xy) <- lonlat # Make sure that we assign the lon-lat CRS so that R knows how this object is projected
# 
# dispersal_summary.xy %>%
#   group_by(species_common_name, transmitter_id, Month) %>%
#   st_distance()
# 
# 
# ## Time of each detection
# dispersal_summary %>%
#   group_by(species_common_name, transmitter_id) %>%
#   summarise(
#     # num.years = year(max(Date.Time)) - year(min(Date.Time)),
#     num.days = difftime(date(max(detection_datetime)), 
#                         date(min(detection_datetime)), 
#                         units = "days")) %>%
#   arrange(desc(num.days))
# 
# ## Max distance between of daily detections
# dispersal_summary %>%
#   group_by(species_common_name, transmitter_id,date) %>%
#   summarise(
#     # num.years = year(max(Date.Time)) - year(min(Date.Time)),
#     num.days = difftime(date(max(detection_datetime)), 
#                         date(min(detection_datetime)), 
#                         units = "days")) %>%
#   arrange(desc(num.days))
# 
# 
# metadata <-
#   dispersal_summary %>%
#   filter(!is.na(species_common_name)) %>% 
#   mutate(transmitter_id = transmitter_id,
#          species_common_name=species_common_name,
#          species_scientific_name = species_scientific_name) %>%#,
#          #sex = Sex,
#          #length_mm = Bio) %>%
#   group_by(transmitter_id, species_common_name, 
#            species_scientific_name)%>% #, sex, length_mm) %>%
#   summarise(receiver_deployment_longitude = mean(receiver_deployment_longitude, na.rm=T),
#             receiver_deployment_latitude = mean(receiver_deployment_latitude, na.rm=T)) #%>% 
#   # mutate(region = 
#   #          case_when(Longitude < 0 ~ "Caribbean/Atlantic",
#   #                    TRUE ~ "Pacific"),
#          # installation =
#          #   case_when(region %in% "Caribbean/Atlantic" & Latitude > 20 ~ "Florida",
#          #             region %in% "Caribbean/Atlantic" & Latitude < 20 & Latitude > 0 ~ "Belize",
#          #             region %in% "Caribbean/Atlantic" & Latitude < 0 ~ "Brazil",
#          #             region %in% "Pacific" & Longitude < 115 ~ "Ningaloo",
#          #             region %in% "Pacific" & Longitude > 115 & Longitude < 120 ~ "Rowley",
#          #             region %in% "Pacific" & Longitude > 120 & Longitude < 130 ~ "Scott",
#          #             region %in% "Pacific" & Longitude > 130 & Longitude < 149 ~ "FNQ",
#          #             region %in% "Pacific" & Longitude > 149 ~ "Heron"),
#          # array_area_m2 = 
#          #   case_when(installation %in% "Belize" ~ 30790279,
#          #             installation %in% "Brazil" ~ 14074567,
#          #             installation %in% "Florida" ~ 110859213,
#          #             installation %in% "FNQ" ~ 104883087,
#          #             installation %in% "Heron" ~ 74363127,
#          #             installation %in% "Ningaloo" ~ 117306872,
#          #             installation %in% "Rowley" ~ 32621716,
#          #             installation %in% "Scott" ~ 24530935)
#          # ) %>% 
#   dplyr::select(-c(Longitude, Latitude)) %>% 
#   ungroup()
# 
# write_csv(metadata, "~/Desktop/2019-09-18_TagMetadata_installations.csv")
# 
# # array_area <-
# #   dispersal_summary %>%
# #   distinct(Longitude, Latitude) %>% 
# #   mutate(region = 
# #            case_when(Longitude < 0 ~ "Caribbean/Atlantic",
# #                      TRUE ~ "Pacific"),
# #          installation =
# #            case_when(region %in% "Caribbean/Atlantic" & Latitude > 20 ~ "Florida",
# #                      region %in% "Caribbean/Atlantic" & Latitude < 20 & Latitude > 0 ~ "Belize",
# #                      region %in% "Caribbean/Atlantic" & Latitude < 0 ~ "Brazil",
# #                      region %in% "Pacific" & Longitude < 115 ~ "Ningaloo",
# #                      region %in% "Pacific" & Longitude > 115 & Longitude < 120 ~ "Rowley",
# #                      region %in% "Pacific" & Longitude > 120 & Longitude < 130 ~ "Scott",
# #                      region %in% "Pacific" & Longitude > 130 & Longitude < 149 ~ "FNQ",
# #                      region %in% "Pacific" & Longitude > 149 ~ "Heron")) %>% 
# #   st_as_sf(coords=c("Longitude", "Latitude"), crs=4326) %>% 
# #   st_buffer(dist = 0.007) %>% 
# #   group_by(installation) %>% 
# #   summarise(geometry = st_union(geometry)) %>% 
# #   ungroup()
# # array_area %>% 
# #   mutate(area = st_area(.))
# 
# mapview(array_area, zcol = "installation", burst = T)@map %>% 
#   leaflet::addMeasure(primaryLengthUnit = "meters")
# 
# 
# 
# metadata
# 
# # ## Mapview of detection data
# # library(raster)
# # library(mapview)
# # dplot<- car_disp
# # coordinates(dplot)<-c("Longitude","Latitude")
# # projection(dplot)<-CRS("+init=epsg:4326")
# # m<-mapview(dplot, zcol="species", burst=T)
# # mapshot(m, "Raw_Detections.html")
#   
# daily<-
#   dispersal_summary %>%
#   mutate(date = date(detection_datetime),
#          transmitter_id = transmitter_id,
#          species_common_name=species_common_name,
#          species_scientific_name = species_scientific_name#,
#          # sex = Sex,
#          # length_mm = Bio
#          ) %>%
#   group_by(date, transmitter_id, species_common_name, species_scientific_name) %>%#, sex, length_mm) %>%
#   #summarise(max.dispersal_km = max(Consecutive.Dispersal, na.rm=T))
#   summarise(max.dispersal_km = max(Distance_QC, na.rm=T),
#             consecutive.dispersal_km = sum(Distance_QC, na.rm=T))
#   
# daily.un <- head(data.frame(daily),2)
# 
# weekly<-
#   dispersal_summary %>%
#   mutate(week = format(Date.Time, "%Y-%W"),
#          tag_id = Tag.ID,
#          common_name=Common.Name,
#          species = Sci.Name,
#          sex = Sex,
#          length_mm = Bio
#   ) %>%
#   group_by(week, tag_id, common_name, species, sex, length_mm) %>%
#   summarise(max.dispersal_km = max(Consecutive.Dispersal, na.rm=T))
# 
# 
# monthly<-
#   dispersal_summary %>%
#   mutate(month = format(Date.Time, "%Y-%m"),
#          tag_id = Tag.ID,
#          common_name=Common.Name,
#          species = Sci.Name,
#          sex = Sex,
#          length_mm = Bio
#   ) %>%
#   group_by(month, tag_id, common_name, species, sex, length_mm) %>%
#   summarise(max.dispersal_km = max(Consecutive.Dispersal, na.rm=T))
# 
# yearly<-
#   dispersal_summary %>%
#   mutate(year = year(Date.Time),
#          tag_id = Tag.ID,
#          common_name=Common.Name,
#          species = Sci.Name,
#          sex = Sex,
#          length_mm = Bio
#   ) %>%
#   group_by(year, tag_id, common_name, species, sex, length_mm) %>%
#   summarise(max.dispersal_km = max(Consecutive.Dispersal, na.rm=T))
# 
# Dispersal_ouput<-list(Daily=daily, Weekly=weekly, Monthly=monthly, Yearly=yearly)
# 
# saveRDS(Dispersal_ouput, "~/Desktop/Dispersal_Timescales.RDS")


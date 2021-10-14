## Dispersal Kernel comparisons
## Code will take varoius detection datasets for each species and combine with the tag release metadata

install_github("RossDwyer/VTrack", configure.args = "--with-proj-lib=/usr/local/lib/")

library(tidyverse)
library(lubridate)
library(VTrack)
library(remora)
#source("R/2018-10-15_displot.R")

datafolder <- "/Users/uqrdwye2/Dropbox/shark_mpa_model_v400/SA/DEW Marine Parks project/"

#Snapper
snap_det <- paste0(datafolder,"Snapper/IMOS_detections.csv")
snap_receivermet <- paste0(datafolder,"Snapper/IMOS_receiver_deployment_metadata.csv")
snap_tagmet <- paste0(datafolder,"Snapper/IMOS_transmitter_deployment_metadata.csv")
snap_meas <- paste0(datafolder,"Snapper/IMOS_animal_measurements.csv")

## specify files to QC - use supplied example .csv data
snap_files <- list(det = snap_det,
              rmeta = snap_receivermet,
              tmeta = snap_tagmet,
              meas = snap_meas)
qc.out <- runQC(snap_files)

plotQC(qc.out)

d.qc <- grabQC(qc.out, what = "dQC")

# snap_VT <- VTrack::setupData(
#   Tag.Detections=snap_det,
#   Tag.Metadata=snap_tagmet,
#   Station.Information=snap_tagmet,
#   source = "IMOS",
#   tzone = "UTC",
#   crs = NULL
# )


# Read in the detection dataset
#pac_disp<-readRDS("data/Pacific/Dispersal Summaries/Pacific_dispersalSummary.RDS")
#car_disp<-readRDS("data/Pacific/Dispersal Summaries/Caribbean_disperalSummary.RDS")

library(sf)
dispersal_summary <-  d.qc %>%
  select(transmitter_id,tagging_project_name,
         species_common_name,species_scientific_name,
         detection_datetime,
         installation_name,station_name,
         receiver_deployment_longitude, 
         receiver_deployment_latitude) %>%
  # bind_rows(
  # pac_disp %>%
  #   mutate(Release.Date = date(ReleaseDate_QC)), 
  # car_disp) %>%
  # filter(Consecutive.Dispersal > 0) %>%
  #filter(Distance_QC > 0) %>%
  mutate(species_scientific_name = as.factor(species_scientific_name),
         species_common_name = as.factor(species_common_name),
         Day = date(detection_datetime),
         Week = format(Day, "%Y-%W"),
         Month = format(Day, "%Y-%m"))




lonlat <- "+proj=longlat +datum=WGS84 +no_defs"
dispersal_summary.xy <- st_as_sf(dispersal_summary, 
                                coords = c("receiver_deployment_longitude", 
                                           "receiver_deployment_latitude")) # Convert pts from a data frame into an sf object, using x and y as coordinates
st_crs(dispersal_summary.xy) <- lonlat # Make sure that we assign the lon-lat CRS so that R knows how this object is projected

dispersal_summary.xy %>%
  group_by(species_common_name, transmitter_id, Month) %>%
  st_distance()


## Time of each detection
dispersal_summary %>%
  group_by(species_common_name, transmitter_id) %>%
  summarise(
    # num.years = year(max(Date.Time)) - year(min(Date.Time)),
    num.days = difftime(date(max(detection_datetime)), 
                        date(min(detection_datetime)), 
                        units = "days")) %>%
  arrange(desc(num.days))

## Max distance between of daily detections
dispersal_summary %>%
  group_by(species_common_name, transmitter_id,date) %>%
  summarise(
    # num.years = year(max(Date.Time)) - year(min(Date.Time)),
    num.days = difftime(date(max(detection_datetime)), 
                        date(min(detection_datetime)), 
                        units = "days")) %>%
  arrange(desc(num.days))


metadata <-
  dispersal_summary %>%
  filter(!is.na(species_common_name)) %>% 
  mutate(transmitter_id = transmitter_id,
         species_common_name=species_common_name,
         species_scientific_name = species_scientific_name) %>%#,
         #sex = Sex,
         #length_mm = Bio) %>%
  group_by(transmitter_id, species_common_name, 
           species_scientific_name)%>% #, sex, length_mm) %>%
  summarise(receiver_deployment_longitude = mean(receiver_deployment_longitude, na.rm=T),
            receiver_deployment_latitude = mean(receiver_deployment_latitude, na.rm=T)) #%>% 
  # mutate(region = 
  #          case_when(Longitude < 0 ~ "Caribbean/Atlantic",
  #                    TRUE ~ "Pacific"),
         # installation =
         #   case_when(region %in% "Caribbean/Atlantic" & Latitude > 20 ~ "Florida",
         #             region %in% "Caribbean/Atlantic" & Latitude < 20 & Latitude > 0 ~ "Belize",
         #             region %in% "Caribbean/Atlantic" & Latitude < 0 ~ "Brazil",
         #             region %in% "Pacific" & Longitude < 115 ~ "Ningaloo",
         #             region %in% "Pacific" & Longitude > 115 & Longitude < 120 ~ "Rowley",
         #             region %in% "Pacific" & Longitude > 120 & Longitude < 130 ~ "Scott",
         #             region %in% "Pacific" & Longitude > 130 & Longitude < 149 ~ "FNQ",
         #             region %in% "Pacific" & Longitude > 149 ~ "Heron"),
         # array_area_m2 = 
         #   case_when(installation %in% "Belize" ~ 30790279,
         #             installation %in% "Brazil" ~ 14074567,
         #             installation %in% "Florida" ~ 110859213,
         #             installation %in% "FNQ" ~ 104883087,
         #             installation %in% "Heron" ~ 74363127,
         #             installation %in% "Ningaloo" ~ 117306872,
         #             installation %in% "Rowley" ~ 32621716,
         #             installation %in% "Scott" ~ 24530935)
         # ) %>% 
  dplyr::select(-c(Longitude, Latitude)) %>% 
  ungroup()

write_csv(metadata, "~/Desktop/2019-09-18_TagMetadata_installations.csv")

# array_area <-
#   dispersal_summary %>%
#   distinct(Longitude, Latitude) %>% 
#   mutate(region = 
#            case_when(Longitude < 0 ~ "Caribbean/Atlantic",
#                      TRUE ~ "Pacific"),
#          installation =
#            case_when(region %in% "Caribbean/Atlantic" & Latitude > 20 ~ "Florida",
#                      region %in% "Caribbean/Atlantic" & Latitude < 20 & Latitude > 0 ~ "Belize",
#                      region %in% "Caribbean/Atlantic" & Latitude < 0 ~ "Brazil",
#                      region %in% "Pacific" & Longitude < 115 ~ "Ningaloo",
#                      region %in% "Pacific" & Longitude > 115 & Longitude < 120 ~ "Rowley",
#                      region %in% "Pacific" & Longitude > 120 & Longitude < 130 ~ "Scott",
#                      region %in% "Pacific" & Longitude > 130 & Longitude < 149 ~ "FNQ",
#                      region %in% "Pacific" & Longitude > 149 ~ "Heron")) %>% 
#   st_as_sf(coords=c("Longitude", "Latitude"), crs=4326) %>% 
#   st_buffer(dist = 0.007) %>% 
#   group_by(installation) %>% 
#   summarise(geometry = st_union(geometry)) %>% 
#   ungroup()
# array_area %>% 
#   mutate(area = st_area(.))

mapview(array_area, zcol = "installation", burst = T)@map %>% 
  leaflet::addMeasure(primaryLengthUnit = "meters")



metadata

# ## Mapview of detection data
# library(raster)
# library(mapview)
# dplot<- car_disp
# coordinates(dplot)<-c("Longitude","Latitude")
# projection(dplot)<-CRS("+init=epsg:4326")
# m<-mapview(dplot, zcol="species", burst=T)
# mapshot(m, "Raw_Detections.html")
  
daily<-
  dispersal_summary %>%
  mutate(date = date(detection_datetime),
         transmitter_id = transmitter_id,
         species_common_name=species_common_name,
         species_scientific_name = species_scientific_name#,
         # sex = Sex,
         # length_mm = Bio
         ) %>%
  group_by(date, transmitter_id, species_common_name, species_scientific_name) %>%#, sex, length_mm) %>%
  #summarise(max.dispersal_km = max(Consecutive.Dispersal, na.rm=T))
  summarise(max.dispersal_km = max(Distance_QC, na.rm=T),
            consecutive.dispersal_km = sum(Distance_QC, na.rm=T))
  
daily.un <- head(data.frame(daily),2)

weekly<-
  dispersal_summary %>%
  mutate(week = format(Date.Time, "%Y-%W"),
         tag_id = Tag.ID,
         common_name=Common.Name,
         species = Sci.Name,
         sex = Sex,
         length_mm = Bio
  ) %>%
  group_by(week, tag_id, common_name, species, sex, length_mm) %>%
  summarise(max.dispersal_km = max(Consecutive.Dispersal, na.rm=T))


monthly<-
  dispersal_summary %>%
  mutate(month = format(Date.Time, "%Y-%m"),
         tag_id = Tag.ID,
         common_name=Common.Name,
         species = Sci.Name,
         sex = Sex,
         length_mm = Bio
  ) %>%
  group_by(month, tag_id, common_name, species, sex, length_mm) %>%
  summarise(max.dispersal_km = max(Consecutive.Dispersal, na.rm=T))

yearly<-
  dispersal_summary %>%
  mutate(year = year(Date.Time),
         tag_id = Tag.ID,
         common_name=Common.Name,
         species = Sci.Name,
         sex = Sex,
         length_mm = Bio
  ) %>%
  group_by(year, tag_id, common_name, species, sex, length_mm) %>%
  summarise(max.dispersal_km = max(Consecutive.Dispersal, na.rm=T))

Dispersal_ouput<-list(Daily=daily, Weekly=weekly, Monthly=monthly, Yearly=yearly)

saveRDS(Dispersal_ouput, "~/Desktop/Dispersal_Timescales.RDS")


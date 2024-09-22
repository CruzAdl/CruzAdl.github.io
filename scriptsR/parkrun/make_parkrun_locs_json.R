# PROCESSES JSON OF EVENT LOCATIONS FILE FOUND AT HIDDDEN LINK
# https://images.parkrun.com/events.json

librarian::shelf(
  here,jsonlite,sf,dplyr,tidyr,purrr,stringr
  )

file_path<-"rawdata/events.json"

json_data <- fromJSON(file_path)

events_df <- as.data.frame(json_data$events$features)

events_df_flat <- jsonlite::flatten(events_df)

names(events_df_flat)

cols <- c("geometry.coordinates",
          "properties.EventShortName",
          "properties.countrycode",
          "properties.EventLocation")

events_proc <- events_df_flat[,cols] %>%
  mutate(x = map_dbl(geometry.coordinates, 1),
         y = map_dbl(geometry.coordinates, 2),
         geometry.coordinates=NULL) %>%
  rename(Event=1,Country=2,Location=3) %>% 
  filter(!str_detect(Event,"junior")) %>% 
  st_as_sf(., coords=c("x","y"), crs=4326,remove=FALSE) 

saveRDS(events_proc,"rawdata/event_locations.RDS")

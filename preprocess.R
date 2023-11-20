library(here)
library(sf)
library(tidyr)
library(dplyr)

#load in the detailed forest data
detail_forest_data <- read_sf(paste0(here::here(), "/data-raw/union_join_with_fields/union_join.shp")) %>%
  #clean up extra fields
  select(OBJECTID, Shape_Leng, Shape_Area, '1937':geometry) %>%
  mutate(`1937`= case_when(`1937` == "Forest" ~ "Original Forest",
                           TRUE ~ "Not Forest"))

#reorganize the data to use for mapping
cols <- colnames(detail_forest_data)

for (i in 5:11) {
  cur_col <- cols[i]
  last_col <- cols[i-1]
  detail_forest_data <- detail_forest_data %>%
    mutate({{cur_col}} := case_when(.data[[cur_col]] == "NotForest" & .data[[last_col]] == "Original Forest" ~ "Deforested",
                                    .data[[cur_col]] == "Forest" & .data[[last_col]] == "Original Forest" ~ "Original Forest",
                                    .data[[cur_col]] == "NotForest" & .data[[last_col]] == "Not Forest" ~ "Not Forest",
                                    .data[[cur_col]] == "Forest" & .data[[last_col]] == "Not Forest" ~ "New Forest",
                                    .data[[cur_col]] == "NotForest" & .data[[last_col]] == "New Forest" ~ "New Forest Lost",
                                    .data[[cur_col]] == "Forest" & .data[[last_col]] == "New Forest" ~ "New Forest",
                                    .data[[cur_col]] == "NotForest" & .data[[last_col]] == "Deforested" ~ "Deforested",
                                    .data[[cur_col]] == "Forest" & .data[[last_col]] == "Deforested" ~ "New Forest",
                                    .data[[cur_col]] == "NotForest" & .data[[last_col]] == "New Forest Lost" ~ "New Forest Lost",
                                    .data[[cur_col]] == "Forest" & .data[[last_col]] == "New Forest Lost" ~ "New Forest",
                                    TRUE ~ "error"))
}

#save the data
#write_sf(detail_forest_data, paste0(here::here(), "/data-raw/mapping_data/mendota_yahara_forest.shp"))

forest_data <- read_sf(paste0(here::here(), "/data-raw/mapping_data/mendota_yahara_forest.shp")) %>%
  st_transform(4326)

#process data for the cumulative graph
cumulative_summary <- pivot_longer(st_drop_geometry(forest_data), 4:11) %>%
  mutate(value = case_when(
    value == "Original Forest" ~ "Old Growth Forest",
    value == "New Forest" ~ "Early Successional Forest",
    value == "Deforested" ~ "Old Growth Forest Lost",
    value == "New Forest Lost" ~ "Early Successional Forest Lost",
    TRUE ~ value
  )) %>%
  group_by(name, value) %>%
  summarise(total = sum(Acres))

#save
#write.csv(cumulative_summary, paste0(here::here(), "/data-raw/cumulative_summary.csv"), row.names=FALSE)
cumulative_summary <- read.csv(paste0(here::here(), "/data-raw/cumulative_summary.csv"))

#process data for change graph
change_old_growth <- ungroup(cumulative_summary) %>%
  filter(value == "Old Growth Forest") %>%
  mutate(prev = dplyr::lag(total)) %>%
  mutate(prev = replace_na(prev, 0)) %>%
  mutate(change = total-prev) %>%
  filter(name != "1937") %>%
  select(-prev, -total) %>%


change_early_successional <- ungroup(cumulative_summary) %>%
  filter(value == "Early Successional Forest") %>%
  mutate(prev = dplyr::lag(total)) %>%
  mutate(prev = replace_na(prev, 0)) %>%
  mutate(change = total-prev) %>%
  select(-prev, -total)

#save
#write.csv(bind_rows(change_old_growth, change_early_successional), paste0(here::here(), "/data-raw/change_summary.csv"), row.names=FALSE)
change_summary <- read.csv(paste0(here::here(), "/data-raw/change_summary.csv"))


#process parcel data
parcels_2023_short <- select(parcels_2023, SITEADRESS, PLACENAME, ZIPCODE) %>%
  filter(!is.na(SITEADRESS)) %>%
  mutate(address = case_when(
          is.na(PLACENAME) ~ paste0(SITEADRESS, ", ", ZIPCODE),
          is.na(ZIPCODE) ~ paste0(SITEADRESS, ", ", PLACENAME),
          is.na(ZIPCODE) & is.na(PLACENAME) ~ paste0(SITEADRESS),
         TRUE ~ paste0(SITEADRESS, ", ", PLACENAME, ", ", ZIPCODE))) %>%
  select(address) %>%
  st_transform(st_crs(forest_data))

parcels_2023_short <- read_sf(paste0(here::here(), "/data-raw/project_parcels/parcels_2023_short.shp"))

library(sf)
library(dplyr)
library(tidyverse)
library(readr)
library(measurements)

#####################################
## SPRC ##
#####################################

#Author: Mary Ellen Klukow
#email: mklukow@kent.edu

## SPRC Hydrology ##
#inlet SPRC_01 to outlet SPRC_02; then inlet SPRC_03 to outlet SPRC_04
## UPDATE CSVs TO MOST RECENT RELEASE BEFORE RUNNING

#merge HOBO and Solinst data to find water level (m) for SPRC 02 and SPRC 04

#water level from HOBO data
SPRC_hobo_file <- "SPRC_hobo_WY25-v1.0_20250722.csv"
SPRC_HOBO_volume <- read.csv(SPRC_hobo_file) %>%
  select("location",
         "datetime",
         "water_level_m")
SPRC_HOBO_volume$datetime <- 
  as.POSIXct(SPRC_HOBO_volume$datetime, format = "%m/%d/%Y")

#SPRC_02 water level from Solinst data (Solinsts 8 and 26)
SPRC_wll8 <- read.csv("WS_SPRC_02_h2ohio_wll08_proc-comb.csv")
SPRC_wll26 <- read.csv("WS_SPRC_02_h2ohio_wll26_proc-comb.csv")
SPRC_02_sol <- rbind(SPRC_wll8, SPRC_wll26) %>%
  select("datetime",
         "LEVEL") %>%
  rename_at("LEVEL", ~"water_level_m") %>%
  mutate(location = "SPRC_02")
SPRC_02_sol$datetime <- 
  as.POSIXct(SPRC_02_sol$datetime, format = "%m/%d/%Y")

#set unusable dates and remove
unusable1 <- seq(as.Date("2023-12-11"), as.Date("2023-12-16"), by = "day")
unusable2 <- seq(as.Date("2024-04-03"), as.Date("2024-04-17"), by = "day")
unusable3 <- seq(as.Date("2024-11-29"), as.Date("2025-03-10"), by = "day")
unusable_dates <- c(unusable1, unusable2, unusable3)
SPRC_02_sol <- SPRC_02_sol %>%
  filter(!as.Date(datetime) %in% unusable_dates)

#combine SPRC_02 HOBO and Solinst data
SPRC_02_volume <- SPRC_HOBO_volume %>%
  #pull out HOBO from North Pool
  filter(location == "SPRC_02") %>%
  #combine two dfs
  rbind(SPRC_02_sol)
SPRC_02_volume$datetime <- SPRC_02_volume$datetime %>% as.Date()
#set sensor elevations from data from Morgan
el_change1 <- seq(as.Date("2024-04-17"), as.Date("2024-06-18"), by = "day")
el_change2 <- seq(as.Date("2025-05-28"), as.Date("2025-06-16"), by = "day")

#combine HOBO and Solinst data for North Pool
SPRC_02_volume <- SPRC_02_volume %>%
  #group by date and find average daily water level
  group_by(datetime) %>%
  summarise(water_level_m = mean(water_level_m)) %>%
  #set sensor elevations
  mutate(sensor_el = case_when(datetime %in% as.Date(el_change1) ~ as.numeric(824.15),
                               datetime %in% as.Date(el_change2) ~ as.numeric(823.96),
                               .default = as.numeric(822))) %>%
  #calc water elevation in feet by converting units and adding sensor elevation
  mutate(elevation_ft = conv_unit(water_level_m, "m", "ft") + sensor_el,
         #calc average daily volumes in gallons with WS equation
         volume_gal = (3866.66 * (elevation_ft ^ 3)) 
         - (9492338.86 * (elevation_ft ^ 2)) 
         + (7767632310.25 * elevation_ft) 
         - 2118762457458.17, 
         #convert daily volumes in gallons to L
         volume_L = conv_unit(volume_gal, "us_gal", "L")) %>%
  #calc daily change in volume by subtracting current day volume by previous day
  arrange(datetime) %>%
  mutate(vol_diff = volume_L - lag(volume_L, default = first(volume_L))) %>%
  #sum the positive volume changes per month
  mutate(date = as.factor(format(as.Date(datetime, format = "%m/%d/%Y"), "%m/%Y"))) %>%
  filter(vol_diff > 0) %>%
  group_by(date) %>%
  summarise(inflow_vol_L = sum(vol_diff, na.rm = TRUE))

#SPRC_04 water level from Solinst data (Solinst 6)
SPRC_04_sol_file <- "WS_SPRC_04_h2ohio_wll06_proc-comb.csv"
SPRC_04_sol <- read.csv(SPRC_04_sol_file) %>%
  select("datetime",
         "LEVEL") %>%
  rename_at("LEVEL", ~"water_level_m") %>%
  mutate(location = "SPRC_04")
SPRC_04_sol$datetime <- 
  as.POSIXct(SPRC_04_sol$datetime, format = "%m/%d/%Y")

#combine HOBO and Solinst data for South Pool
SPRC_04_volume <- SPRC_HOBO_volume %>%
  #pull out HOBO from South Pool
  filter(location == "SPRC_04") %>%
  #combine two dfs
  rbind(SPRC_04_sol) %>%
  #group by date and find average daily water level
  group_by(datetime) %>%
  summarise(water_level_m = mean(water_level_m)) %>%
  #calc water elevation in feet by converting units and adding sensor elevation
  mutate(elevation_ft = conv_unit(water_level_m, "m", "ft") + 822.5,
      #calc average daily volumes in gallons with WS equation
       volume_gal = (9317.5701086139 * (elevation_ft ^ 3))
       - (22816397.8919351 * (elevation_ft ^ 2))
       + (18623801908.7201 * elevation_ft)
       - 5067181357823.19,
      #convert daily volumes in gallons to L
       volume_L = conv_unit(volume_gal, "us_gal", "L")) %>%
  #calc daily change in volume by subtracting current day volume by previous day
  arrange(datetime) %>%
  mutate(vol_diff = volume_L - lag(volume_L, default = first(volume_L))) %>%
  #sum the positive volume changes per month
  mutate(date = as.factor(format(as.Date(datetime, format = "%m/%d/%Y"), "%m/%Y"))) %>%
  filter(vol_diff > 0) %>%
  group_by(date) %>%
  summarise(inflow_vol_L = sum(vol_diff, na.rm = TRUE))


## SPRC Nutrients ##
SPRC_nutrient_file <- "SPRC_surface_water_nutrient_WY25-v1.0_20250721.csv"
SPRC_conc_df <- read.csv(SPRC_nutrient_file)
head(SPRC_conc_df)

#match up LOIs with coord -- Raissa's code
lois <- read_csv("./locations_of_interest.csv")

# Convert coordinates to sf, ensure same CRS
data.coords.sf <- st_as_sf(SPRC_conc_df %>% 
                             filter(!is.na(coord_x) & !is.na(coord_y)), 
                           coords = c('coord_x', 'coord_y'), 
                           crs = 'EPSG: 4326')
lois.centroids.sf <- st_as_sf(lois %>% 
                                filter(!is.na(loi_lon)|!is.na(loi_lat)), 
                              coords = c('loi_lon', 'loi_lat'), 
                              crs = 'EPSG: 4326')

# Get nearest LOI for each coordinate pair
nearest_id <- st_nearest_feature(data.coords.sf, lois.centroids.sf)

# Get distance between each coordinate pair and its nearest LOI
nearest_dist <- st_distance(data.coords.sf, 
                            lois.centroids.sf[nearest_id,], 
                            by_element = TRUE)

# Combine results into a new dataframe
matched_loc <- 
  bind_cols(SPRC_conc_df %>% filter(!is.na(coord_x) & !is.na(coord_y)), 
            nearest_dist = nearest_dist) %>%
  mutate(loc_coords = lois.centroids.sf[nearest_id, 'loi_id']$loi_id, 
         loc_coords_name = lois.centroids.sf[nearest_id, 'loi_name']$loi_name)

#create monthly concentrations by LOI
SPRC_nutrients <- matched_loc %>%
  separate(datetime, c("date", "time"), sep = " ") %>%
  mutate(date = as.factor(format(as.Date(date, format = "%m/%d/%Y"), "%m/%Y"))) %>%
  select(loc_coords,
         date,
         tn_mgL,
         tp_mgL,
         nitrate_nitrite_mgL,
         total_ammonia_mgL,
         urea_mgL,
         drp_mgL) %>%
  #average monthly concentrations
  aggregate(. ~ date + loc_coords,
            FUN = mean)
#this is taking out all rows with even one NA, so not sure what to do here


## SPRC Load Calculation ##

#use Olivia's for-loop to make a df of each LOI
loc <- c("SPRC_01", "SPRC_02", "SPRC_03", "SPRC_04")
for(i in 1:length(loc)){
  assign(paste0(loc[i],"_nut"), 
         subset(SPRC_nutrients,loc_coords == loc[i]))
}


#calculate North Pool (NP) load
#calculate load at the inlet
SPRC_01_nut <- merge(SPRC_01_nut, SPRC_02_volume, by = "date")
SPRC_01_nut <- SPRC_01_nut %>%
  #calculate load in mg and pounds
  mutate(across(tn_mgL:drp_mgL, ~. * (inflow_vol_L/453592.37), .names = "lbsIN_{.col}")) %>%
  select(date,
         lbsIN_tn_mgL:lbsIN_drp_mgL)

#calculate the load at the outlet
SPRC_02_nut <- merge(SPRC_02_nut, SPRC_02_volume, by = "date")
SPRC_02_nut <- SPRC_02_nut %>%
  #calculate load in mg and pounds
  mutate(across(tn_mgL:drp_mgL, ~. * (inflow_vol_L/453592.37), .names = "lbsOUT_{.col}")) %>%
  select(date,
         lbsOUT_tn_mgL:lbsOUT_drp_mgL)

#combine and find retained
SPRC_NP_load <- SPRC_01_nut %>%
  merge(SPRC_02_nut, by = "date") %>%
  mutate(NP_Nfiltered_lbs = lbsIN_tn_mgL - lbsOUT_tn_mgL,
         NP_Pfiltered_lbs = lbsIN_tp_mgL - lbsOUT_tp_mgL,
         NP_NOxfiltered_lbs = lbsIN_nitrate_nitrite_mgL - lbsOUT_nitrate_nitrite_mgL,
         NP_NH4filtered_lbs = lbsIN_total_ammonia_mgL - lbsOUT_total_ammonia_mgL,
         NP_DRPfiltered_lbs = lbsIN_drp_mgL - lbsOUT_drp_mgL) %>%
  select(date,
         NP_Nfiltered_lbs:NP_DRPfiltered_lbs)

#calculate South Pool (SP) load
#calculate load at the inlet
SPRC_03_nut <- merge(SPRC_03_nut, SPRC_04_volume, by = "date")
SPRC_03_nut <- SPRC_03_nut %>%
  #calculate load in mg and pounds
  mutate(across(tn_mgL:drp_mgL, ~. * (inflow_vol_L/453592.37), .names = "lbsIN_{.col}")) %>%
  select(date,
         lbsIN_tn_mgL:lbsIN_drp_mgL)

#calculate the load at the outlet
SPRC_04_nut <- merge(SPRC_04_nut, SPRC_04_volume, by = "date")
SPRC_04_nut <- SPRC_04_nut %>%
  #calculate load in mg and pounds
  mutate(across(tn_mgL:drp_mgL, ~. * (inflow_vol_L/453592.37), .names = "lbsOUT_{.col}")) %>%
  select(date,
         lbsOUT_tn_mgL:lbsOUT_drp_mgL)

#combine and find retained
SPRC_SP_load <- SPRC_03_nut %>%
  merge(SPRC_04_nut, by = "date") %>%
  mutate(SP_Nfiltered_lbs = lbsIN_tn_mgL - lbsOUT_tn_mgL,
         SP_Pfiltered_lbs = lbsIN_tp_mgL - lbsOUT_tp_mgL,
         SP_NOxfiltered_lbs = lbsIN_nitrate_nitrite_mgL - lbsOUT_nitrate_nitrite_mgL,
         SP_NH4filtered_lbs = lbsIN_total_ammonia_mgL - lbsOUT_total_ammonia_mgL,
         SP_DRPfiltered_lbs = lbsIN_drp_mgL - lbsOUT_drp_mgL) %>%
  select(date,
         SP_Nfiltered_lbs:SP_DRPfiltered_lbs)

#combine both pools' lbs filtered
SPRC_load <- merge(SPRC_NP_load, SPRC_SP_load, by = "date", all = TRUE)
SPRC_load <- SPRC_load %>%
  replace(is.na(.), 0) %>%
  #add lbs filtered from both pools
  mutate(Nfiltered_lbs = NP_Nfiltered_lbs + SP_Nfiltered_lbs,
         Pfiltered_lbs = NP_Pfiltered_lbs + SP_Pfiltered_lbs,
         NOxfiltered_lbs = NP_NOxfiltered_lbs + SP_NOxfiltered_lbs,
         NH4filtered_lbs = NP_NH4filtered_lbs + SP_NH4filtered_lbs,
         DRPfiltered_lbs = NP_DRPfiltered_lbs + SP_DRPfiltered_lbs) %>%
  select(date,
         Nfiltered_lbs:DRPfiltered_lbs)

WY23 <- c("10/2022",
          "11/2022",
          "12/2022",
          "01/2023",
          "02/2023",
          "03/2023",
          "04/2023",
          "04/2023",
          "04/2023",
          "07/2023",
          "08/2023",
          "09/2023")
SPRC_WY23 <- SPRC_load %>%
  filter(date %in% WY23)

WY24 <- c("10/2023",
          "11/2023",
          "12/2023",
          "01/2024",
          "02/2024",
          "03/2024",
          "04/2024",
          "04/2024",
          "04/2024",
          "07/2024",
          "08/2024",
          "09/2024")
SPRC_WY24 <- SPRC_load %>%
  filter(date %in% WY24)




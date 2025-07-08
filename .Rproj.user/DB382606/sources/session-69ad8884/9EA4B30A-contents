setwd("C:/Users/kande/OneDrive - Kent State University/Current Projects/Manuscript Ideas/CONNECTIVITY PAPER")
library(dplyr)
library(lubridate)
library(purrr)
library(rlang)

## Inlets and outlet site codes for each budget site: 

# Inlets
c(BWLK_01,BWLK_05,FORB_09,FORB_15,FORB_19,BROP_01,BROP_02,BROP_03,BROP_05,BROP_06, TIPC_01,TIPC_02,TIPC_03, SPRC_01,SPRC_02, SPRC_03,SPRC_04, MAGM_01,OAKw_01,REDB_01,REDB_02,REDB_03,REDB_04,REDB_05,REDB_06,REDB_07,REDB_13)
# BWLK: 01 inlet, 05 outlet
# FORB: 09/15 inlet, 19 outlet
# BROP: 01 inlet during + flows, lake as inlet during - flows. No outflow duing - flows. Outlet varies by lake elevation: 03<889.5, 06<891, and 05<891.5 (02 was used in place of 05 /06 during Aug 2024 due to dry conditions)
# TIPC: 01 inlet, avg of 02/03 outlet
# SPRC: 01 inlet, 02 oulet, THEN 03 inlet, 04 outlet and combine retention for both chunks to get total
# MAGM: 01 inlet (highest concentration measured used), no outlet: water was not released in 2023 or 2024. Full retention
# OAKW/E: OAKW_01 as inlet and outlet. But not connected in 2024. Check with Bob for how he calculated 2023 
# REDB: Complicated uses all pools: 01-7, 13
# SJRE: Not using. Data is not up to snuff.
# Montepelier: not in our database
# Williamsburg: not in our database


REDBnut = subset(nutrient, project_code == "REDB" & samp_type == "sample")

#####################################
## BWLK ##
#####################################
## BWLK Hydrology
setwd("C:/Users/kande120/OneDrive - Kent State University/Documents/GitHub/data-upload/manual_sensors/hobo")
file_list <- list.files()
filtered_files <- grep("WS_BWLK_05", file_list, value = TRUE) # only files from BWLK_05
print(filtered_files)


read_clean_csv <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE, encoding = "latin1")
  header <- strsplit(lines[1], ",")[[1]]
  num_cols <- length(header)
  
  valid_lines <- lines[sapply(strsplit(lines, ","), length) == num_cols]
  
  temp_file <- tempfile(fileext = ".csv")
  writeLines(valid_lines, temp_file)
  
  read.csv(temp_file, header = TRUE, stringsAsFactors = FALSE)
}

tables <- lapply(filtered_files, read_clean_csv)
combined.df <- do.call(bind_rows , tables)

#### COME BACK TO THIS NEXT WEEK ONCE ISHFAQ IS DONE WITH QA/QC OF THE FILES because they're not behaving ### 


## BWLK Nutrients
setwd("C:/Users/kande/OneDrive - Kent State University/Documents/GitHub/data-releases")
nutrient = read.csv("surface_water_nutrient_qc_primflag_v1.5_20250317.csv") # MAKE SURE You update this to the most recent release

BWLKnut = subset(nutrient, project_code == "BWLK" & samp_type == "sample")
# add isoweeks for the date
BWLKnut$datetime = as.POSIXct(BWLKnut$datetime)
BWLKnut$week = isoweek(BWLKnut$datetime)
BWLKnut$month = month(BWLKnut$datetime)
BWLKnut$year = year(BWLKnut$datetime)
# subset inlet and outlet values
BWLKnutin = subset(BWLKnut, location == "BWLK_01") 
BWLKnutout = subset(BWLKnut, location == "BWLK_05") 
# Strip out just the data we want and make them into their own data frames
BWLKnutin = with(BWLKnutin, data.frame(datetime,location, year, month, tp_mgL, tn_mgL, drp_mgL,nitrate_nitrite_mgL,total_ammonia_mgL))
BWLKnutout = with(BWLKnutout, data.frame(datetime,location, year, month, tp_mgL, tn_mgL, drp_mgL,nitrate_nitrite_mgL,total_ammonia_mgL))


###############################
# FIGURE OUT WHAT YOU ACTUALLY WANT TO DO WITH THESE HERE

# take the monthly average of inlets
BWLKmonthin = BWLKnutin %>% 
  group_by(year, month)%>%
  summarize(avgPin = mean(tp_mgL, na.rm=T))

# take the monthly average at the outlets
BWLKmonthout = BWLKnutout %>%
  group_by(month)%>%
  summarize(avgPout = mean(tp_mgL, na.rm=T))


BWLK23 = BWLKmonthin %>%
  left_join(y=BWLKmonthout, by=c("month")) # combine inlet and outlet concentrations into one dataframe

print(BWLK23)




bwlk = read.csv("BWLK daily loads.csv")
bwlk$Date<-mdy(bwlk$Date) # tell R that the date is a date 
bwlk$month<-month(bwlk$Date) # tell R that the date is a date 

# Now we can calculate the monthly retention for each year (note we're not specifying water year here, but it doesn't really matter though because neither year had flow from october-december)
bwlk_monthlyload = bwlk %>% 
  group_by(Calendar.Year,month) %>%
  summarise_at(c("Tpinlbs", "SRPinlbs","Tninlbs","Noxinlbs","NH4inlbs","dischargeinGDP", "Tpoutlbs", "SRPoutlbs","Tnoutlbs","Noxoutlbs","Nh4outlbs"), sum, na.rm = TRUE)
bwlk_monthlyload

# now we'll do annual retention (note we're using water year here)
bwlk_annualload = bwlk %>% 
  group_by(Water.Year) %>%
  summarise_at(c("Tpinlbs", "SRPinlbs","Tninlbs","Noxinlbs","NH4inlbs","dischargeinGDP", "Tpoutlbs", "SRPoutlbs","Tnoutlbs","Noxoutlbs","Nh4outlbs"), sum, na.rm = TRUE)
bwlk_annualload

#####################################
## Williamsburg ##
#####################################
will = read.csv("Williamsburg.csv")

# Now we can calculate the monthly retention for each year 
will_monthlyload = will %>% 
  group_by(Month) %>%
  summarise_at(c("Tpinlbs", "DRPinlbs","Tninlbs","Noxinlbs","NH3inlbs","Discharge_cf", "Tpoutlbs", "DRPoutlbs","Tnoutlbs","Noxoutlbs","NH3outlbs"), sum, na.rm = TRUE)
will_monthlyload


#######################################################################################################################################################################################################################################################################################################################################################################################################################
#######################################################################################################################################################################################################################################################################################################################################################################################################################
#######################################################################################################################################################################################################################################################################################################################################################################################################################
#######################################################################################################################################################################################################################################################################################################################################################################################################################
## FORB ##
#######################################################################################################################################################################################################################################################################################################################################################################################################################
#######################################################################################################################################################################################################################################################################################################################################################################################################################
#######################################################################################################################################################################################################################################################################################################################################################################################################################
# Drainage Area delineated by expert opinion (small one)
# total drainage area value of 16.3 acres from the Paulding County District Administrator (though our rough polygon shows closer to 26 acres for the area indicated) 
tDalow = 16.3 
# subsurface drainage area of 7.3 acres (the area of the drainage that has tile drains; this comes from the Paulding County District Administrator)
tDasublow = 7.3 
tDAlow_m2=tDalow*4046.86 #convert acres to m2
tDAlowsub_m2=tDasublow*4046.86 #convert acres to m2

# load in rain data
setwd("C:/Users/kande120/OneDrive - Kent State University/Current Projects/Manuscript Ideas/CONNECTIVITY PAPER/FORB_NutrientBudget_ForRedTeamReview")
FORBrain<-read.csv("FORB_ntwk_WY24-v1.2_20250203.csv") #data from sensor network
FORBrain$Rdatetime<-as.POSIXct(FORBrain$datetime, format = "%m/%d/%y %H:%M") # tell R that the date is a date 
FORBrain$date<-as.Date(FORBrain$Rdatetime) #creates a new column with just the date, separating the time stamp out from the date.
FORBrain.daily<-FORBrain %>%  group_by(date) %>%  summarise(PRCP.mm_sensor = sum(precipaccum, na.rm=TRUE)) # turn 10 minute data to daily data
Paulding_GHCND_WY24<-read.csv("PAULDING, OH US (GHCND-USC00336465) weather WY24.csv") # data from NOAA
Paulding_GHCND_WY23<-read.csv("PAULDING, OH US (GHCND-USC00336465) weather WY23.csv") # data from NOAA
Paulding_GHCND_WY23$date <- as.Date(Paulding_GHCND_WY23$DATE, format = "%Y-%m-%d")
Paulding_GHCND_WY24$date <- as.Date(Paulding_GHCND_WY24$DATE, format = "%m/%d/%y")
NOAA_rain <- bind_rows(Paulding_GHCND_WY23, Paulding_GHCND_WY24) # bind the two NOAA years together
NOAA_rain = NOAA_rain %>% mutate(PRCP.mm = PRCP)##remame
NOAA_rain = NOAA_rain %>% select(date, PRCP.mm)##taking out unnecessary columns
# merge our rain datasets replacing all values with sensor data once the sensors are online
dailyrain <- NOAA_rain %>%
  left_join(FORBrain.daily, by = "date") %>%
  mutate(PRCP.mm = coalesce(PRCP.mm_sensor, PRCP.mm)) %>%  # replace if new value exists
  select(-PRCP.mm_sensor)  # drop the temporary column
dailyrain <- dailyrain %>%  mutate(PRCP.in = PRCP.mm / 25.4)

# Calculate runoff
CN=91 # NRCS Curve number method
S = (1000/CN)-10 
Ia = 0.2*S # Calculate I (inches)
dailyrain$Q = ifelse(dailyrain$PRCP.in<=Ia, 0, ((dailyrain$PRCP.in-Ia)^2)/(dailyrain$PRCP.in-Ia+S)) # calculate runoff
dailyrain$Q.m <-dailyrain$Q * 0.0254 #converting the amount of rain that runs off from inches to meters
dailyrain$Q.m3=tDAlow_m2*dailyrain$Q.m # calculating volume by scaling to entire drainage area
dailyrain$QL = dailyrain$Q.m3 * 1000 # Convert volume to liters

# Estimating subsurface runoff
dailyrain$rainlowsub_m3 = tDAlowsub_m2*dailyrain$PRCP.mm/1000 # calculate total volume that fell on the tile drained subset of the drainage area (7.3 acres)
dailyrain$inflow_m3 = (dailyrain$Q.m3) + (dailyrain$rainlowsub_m3 * 0.3) # Sum surface and subsurface: We expect that subsurface drainage will be ~ 31% of total rainfall (Pease et al. 2018)

# Converting to monthly measurements
dailyrain <- dailyrain %>%
  mutate(month.lab = format(as.Date(date, format = "%d/%m/%Y"), "%b")) %>%
  mutate(month = as.factor(format(as.Date(date, format = "%d/%m/%Y"), "%m"))) %>%
  mutate(year = as.factor(format(as.Date(date, format = "%d/%m/%Y"), "%Y")))

monthlyrain<-dailyrain %>% # this just sorts our data into monthly sum of rainfall and inflow volume
  group_by(month.lab, month, year)%>%
  summarise(monthlyPrecip_mm = sum(PRCP.mm),Inflow.m3 =sum(inflow_m3))

########
# Add nutrient data
setwd("C:/Users/kande120/OneDrive - Kent State University/Documents/GitHub/data-releases")
nutrient = read.csv("surface_water_nutrient_qc_primflag_v1.6_20250617.csv") # MAKE SURE You update this to the most recent release

FORBnut = subset(nutrient, project_code == "FORB" & samp_type == "sample")
# add isoweeks for the date
FORBnut$datetime = as.POSIXct(FORBnut$datetime)
FORBnut$week = isoweek(FORBnut$datetime)
FORBnut$month = month(FORBnut$datetime)
FORBnut$year = year(FORBnut$datetime)
# Quick function to calculate monthly averages for a specific nutrient (WE'RE DOING THIS ACROSS YEARS because this site has limited data)
monthavg <- function(data, variable_names) {
  in_sites <- c("FORB_09", "FORB_15") # select just the inlet
  out_site <- "FORB_17" # select just the outlet
  
  data %>%
    filter(location %in% c(in_sites, out_site)) %>%
    group_by(month) %>%
    summarise(
      !!!setNames(lapply(variable_names, function(var) {expr(mean(.data[[var]][location %in% !!in_sites], na.rm = TRUE))}),paste0(variable_names, "_in")), # mean inflow concentration
      !!!setNames(lapply(variable_names, function(var) {expr(mean(.data[[var]][location == !!out_site], na.rm = TRUE))}), paste0(variable_names, "_out")), # mean outflow concentration
      .groups = "drop")}

FORB_nuts = monthavg(FORBnut,c("tp_mgL", "tn_mgL")) # run the function to calculate monthly avg concentration
FORB_nuts$month = as.factor(FORB_nuts$month)

monthload <- function(flow_data, conc_data, variable_name, new_column_name) {
  var_sym <- sym(variable_name)
  new_col_sym <- sym(new_column_name)
  
  conc_clean <- conc_data %>% # take the monthly concentration!
    mutate(month = as.integer(month)) %>%
    select(month, !!var_sym)
  
  flow_data %>% # multiply the monthly concentration by the flow data
    mutate(month = as.integer(as.character(month))) %>%
    left_join(conc_clean, by = "month") %>%
    mutate(!!new_col_sym := .data[[variable_name]] * Inflow.m3 * 1000 / 453600)
}

# calculate budgets 
monthlyrain = monthload(monthlyrain, FORB_nuts, "tp_mgL_in","TPin") # TP in 
monthlyrain = monthload(monthlyrain, FORB_nuts, "tp_mgL_out","TPout") # TP out

monthlyrain <- monthlyrain %>% # add in the wateryear
  mutate(year_num = as.numeric(as.character(year)),  # make the years numeric
    wateryear = case_when(month %in% c(10, 11, 12) ~ year_num + 1,TRUE ~ year_num)) %>%  select(-year_num)  # shifts to the next year for october, november and december

# Calculate retention
monthlyrain$TPreten = monthlyrain$TPin - monthlyrain$TPout
TPretention = monthlyrain %>%
  group_by(wateryear)%>%
  summarize(annual_TP = sum(TPreten))
TPretention
#######################################################################################################################################################################################################################################################################################################################################################################################################################
#######################################################################################################################################################################################################################################################################################################################################################################################################################
#######################################################################################################################################################################################################################################################################################################################################################################################################################
#######################################################################################################################################################################################################################################################################################################################################################################################################################

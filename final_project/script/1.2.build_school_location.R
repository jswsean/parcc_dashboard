# Script to build school-level location data

# -----------------------------------------------------------------------------#
# Preliminaries ----
# -----------------------------------------------------------------------------#

# Loading the required libraries
library(dplyr)
library(tidyr)
library(here)
library(stringr)
library(readxl)
library(readr)
library(tidygeocoder)
library(tidycensus)
library(sf)

# -----------------------------------------------------------------------------#
# Load data ----
# -----------------------------------------------------------------------------#

# Load school-level ela/math scores data
school_level_ela_math <- readRDS(
  here('data', 'build', 'school_level_ela_math.rds')
)

# Load school directory report data 
school_directory <- read_excel(
  here('data', 'osse', '2022 DC School Report Card Aggregate Public Data.xlsx'), 
  sheet = "School Directory"
)

# Load DC ward shape file data, transform it to NAD83 to ensure consistency with 
# tidycensus geom
dc_ward <- st_read(
  here('final_project', 'script', 'parcc_score_dashboard', 'DC_Wards')
) %>% 
  st_transform(crs = "NAD83")


# -----------------------------------------------------------------------------#
# Wrangle ----
# -----------------------------------------------------------------------------#

# Create a unique school-code, school-name data 
unique_schools <- school_level_ela_math %>% 
  distinct(`School Code`, `School Name`) %>% 
  mutate(
    `School Code` = as.numeric(`School Code`)
  )

# Merge with school_directory data
matched_schools <- unique_schools %>% 
  left_join(
    (school_directory %>% 
       mutate(`School Code` = as.numeric(`School Code`)) %>% 
       select(`School Code`, Address)),
    by = "School Code"
  ) %>% 
  filter(!is.na(Address))

# Get pool of unmatched observations 
unmatched_schools <- unique_schools %>% 
  anti_join(matched_schools, by = "School Code")

# Manually enter address for these 15 unmatched schools
# To revise later, if want to automate
unmatched_schools <- unmatched_schools %>% 
  mutate(
    Address = case_when(
      `School Code` == 102 ~ "3701 HAYES ST NE, Washington, DC 20019", 
      `School Code` == 1100  ~ "908 WAHLER PL SE, Washington, DC 20032", 
      `School Code` == 1113 ~ "2501 MARTIN LUTHER KING JR AVE SE, Washington, DC 20020", 
      `School Code` == 1120 ~ "4600 LIVINGSTON RD SE, Washington, DC 20032", 
      `School Code` == 1124 ~ "2705 MARTIN LUTHER KING AVE SE, Washington, DC 20032", 
      `School Code` == 117 ~ "4401 8TH ST NE Washington, DC 20017", 
      `School Code` == 127 ~ "3701 HAYES ST NE, Washington, DC 20019", 
      `School Code` == 131 ~ "6200 KANSAS AVE NE, Washington, DC, 20011", 
      `School Code` == 134 ~ "6130 North Capitol St NW, Washington, DC 20011", 
      `School Code` == 153 ~ "709 12th St SE Washington, DC 20003", 
      `School Code` == 178 ~ "1920 Bladensburg Rd NE, Washington, DC 20002", 
      `School Code` == 187 ~ "3301 Wheeler Rd SE Washington, DC 20032", 
      `School Code` == 210 ~ "705 Edgewood St NE, Washington, DC 20017", 
      `School Code` == 234 ~ "3100 Martin Luther King Jr Ave SE, Washington, DC 20032", 
      `School Code` == 474 ~ "300 Bryant St NW, Washington, DC 20001"
    )
  )


# Build school location data 
schools_location <- bind_rows(matched_schools, unmatched_schools)


# -----------------------------------------------------------------------------#
# Geolocate ----
# -----------------------------------------------------------------------------#

# Geolocate schools location data using OSM services
schools_location <- schools_location %>% 
  geocode(address = Address, lat = latitude, long = longitude, 
          method = "osm", full_results = TRUE)

# Manually fill out lat long for 8 unmatched schools 
schools_location <- schools_location %>% 
  mutate(
    
    # Fill latitude
    latitude = case_when(
      `School Code` == 1125 ~ 38.866848, 
      `School Code` == 115 ~ 38.923026357563685,
      `School Code` == 116 ~ 38.856786266369944,
      `School Code` == 266 ~ 38.82827360351912,
      `School Code` == 292 ~ 38.923633581023736,
      `School Code` == 3071 ~ 38.85666380824152,
      `School Code` == 318 ~ 38.8600656499531,
      `School Code` == 1124 ~ 38.84847885304582,
      .default = latitude
    ),

    # Fill longitude
    longitude = case_when(
      `School Code` == 1125 ~ -77.010279,
      `School Code` == 115 ~ -77.01890788927655, 
      `School Code` == 116 ~ -76.98926463345977, 
      `School Code` == 266 ~ -77.01345094603334, 
      `School Code` == 292 ~ -77.05731208742377, 
      `School Code` == 3071 ~ -76.9892292112898, 
      `School Code` == 318 ~ -76.99538074509846, 
      `School Code` == 1124 ~ -76.99653127208221, 
      .default = longitude
    )
  )


# -----------------------------------------------------------------------------#
# Determine census tracts, block groups, and census blocks ----
# -----------------------------------------------------------------------------#


# Get census tract income
hh_med_income_tract_2019 <- get_acs(
  geography = "tract", 
  variables = "B19013_001", 
  year = 2019,
  state = "DC", 
  geometry = TRUE
) %>% 
  mutate(
    tract_inc_class_4 = ntile(estimate, 4), 
    tract_inc_class_5 = ntile(estimate, 5)
  )

# Get block group med income
hh_med_income_blockgroup_2019 <- get_acs(
  geography = "block group", 
  variables = "B19013_001", 
  year = 2019,
  state = "DC", 
  geometry = TRUE
) %>% 
  mutate(
    blockgroup_inc_class_4 = ntile(estimate, 4), 
    blockgroup_inc_class_5 = ntile(estimate, 5)
  )

# Converting the school directory data to CRS
schools_location <- schools_location %>% 
  select(`School Code`, `School Name`, latitude, longitude) %>% 
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = st_crs(hh_med_income_tract_2019)
  )

# Determining the tract 
schools_location$tract <- as.numeric(
  st_within(schools_location, hh_med_income_tract_2019)
)

schools_location$blockgroup <- as.numeric(
  st_within(schools_location, hh_med_income_blockgroup_2019)
)

# Determining the wards 
schools_location$ward <- as.numeric(
  st_within(schools_location, dc_ward)
)

# Merging the household income data from the corresponding tract+block group df
schools_location <- schools_location %>% 
  st_join(
    hh_med_income_tract_2019,
    left = F
  ) %>% 
  rename(tract_hhinc_2019_est = estimate,
         tract_hhinc_2019_moe = moe,
         tract_GEOID = GEOID, 
         tract_NAME = NAME) %>% 
  select(-variable) %>% 
  st_join(
    hh_med_income_blockgroup_2019, 
    left = F
  ) %>% 
  rename(blockgroup_hhinc_2019_est = estimate,
         blockgroup_hhinc_2019_moe = moe,
         blockgroup_GEOID = GEOID, 
         blockgroup_NAME = NAME) %>% 
  select(-variable)  %>% 
  st_join(
    (dc_ward %>% select(-REP_NAME, -REP_PHONE, 
                        -REP_EMAIL, -REP_OFFICE,
                        -WEB_URL, -AREASQMI, -POPDENSITY, 
                        -POP2000, -MEDAGE)), 
    left = F
  )
  


# -----------------------------------------------------------------------------#
# Export results ----
# -----------------------------------------------------------------------------#

# Exporting the data (save into R)
write_rds(
  schools_location,
  here('data', 'build', 'school_location.rds')
)


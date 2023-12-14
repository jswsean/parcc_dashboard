# Build average scores by race groups.

# -----------------------------------------------------------------------------#
# Preliminaries ----
# -----------------------------------------------------------------------------#

# Loading the required libraries
library(dplyr)
library(tidyr)
library(here)
library(readxl)
library(stringr)
library(readr)


# -----------------------------------------------------------------------------#
# Load raw data ----
# -----------------------------------------------------------------------------#

# Raw 2023 
raw_2023 <- read_excel(
  here('data', 'osse', '2022-23 School Level PARCC and MSAA Data_9_5.xlsx'),
  sheet = "Meeting, Exceeding"
)

# Raw 2022 
raw_2022 <- read_excel(
  here('data', 'osse', '2021-22 School Level PARCC and MSAA Data.xlsx'),
  sheet = "prof"
)

# Raw 2019 
raw_2019 <- read_excel(
  here('data', 'osse', 'Detailed 2018-19 PARCC and MSAA Performance 2.19.20.Xlsx'),
  sheet = "School Performance"
)

# Raw 2018
raw_2018 <- read_excel(
  here('data', 'osse', 'Detailed 2018 PARCC and MSAA Performance.xlsx'), 
  sheet = "School Performance"
)

# Raw 2017
raw_2017 <- read_excel(
  here('data', 'osse', 'Detailed 2017 PARCC and MSAA Performance_0.xlsx'), 
  sheet = "School Performance"
)


# -----------------------------------------------------------------------------#
# Cleaning ----
# -----------------------------------------------------------------------------#


# Clean 2023 data ----

percent_race_2023 <- raw_2023 %>% 
  filter(`Assessment Name` == "All",
         `Student Group` == "Race/Ethnicity",
         `School Framework` == "All", 
         `Tested Grade/Subject` == "All",
         `Grade of Enrollment` == "All") %>% 
  mutate(
    percent = as.numeric(str_replace(Percent, "%", "")),
    `Total Count` = as.numeric(`Total Count`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Student Group Value`)
  ) %>% 
  arrange(
    `Student Group Value`, Subject
  ) %>% 
  mutate(
    year = 2023
  )



# Clean 2022 data ----

percent_race_2022 <- raw_2022 %>% 
  filter(`Assessment Name` == "All", 
         `Student group` == "Race/Ethnicity", 
         `Tested Grade/Subject` == "All", 
         `Grade of Enrollment` == "All") %>% 
  mutate(
    percent = as.numeric(str_replace(Percent, "%", "")),
    `Total Count` = as.numeric(`Total Count`),
    `Subgroup Value` = case_when(
      `Subgroup Value` == "Hispanic/Latino" ~ "Hispanic/Latino of any race",
      `Subgroup Value` == "Pacific Islander/Native Hawaiian" ~ "Native Hawaiian/Other Pacific Islander",
      `Subgroup Value` == "Two or More Races" ~ "Two or more races",
      `Subgroup Value` == "White/Caucasian" ~ "White",
      .default = `Subgroup Value`
    )
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Subgroup Value`)
  ) %>% 
  arrange(
    `Subgroup Value`, Subject
  ) %>% 
  mutate(
    year = 2022
  )
  


# Clean 2019 data ----

percent_race_2019 <- raw_2019 %>% 
  filter(
    `Assessment Type` == "All", 
    Subgroup == "Race/Ethnicity", 
    `Tested Grade/Subject` == "All",
    `Grade of Enrollment` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")),
    `Total Count` = as.numeric(`Total Number Valid Test Takers`),
    `Subgroup Value` = case_when(
      `Subgroup Value` == "American Indian/Alaskan Native" ~ "American Indian/Alaska Native",
      `Subgroup Value` == "Hispanic/Latino" ~ "Hispanic/Latino of any race",
      `Subgroup Value` == "Pacific Islander/Native Hawaiian" ~ "Native Hawaiian/Other Pacific Islander",
      `Subgroup Value` == "Two or More Races" ~ "Two or more races",
      `Subgroup Value` == "White/Caucasian" ~ "White",
      .default = `Subgroup Value`
    )
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Subgroup Value`)
  ) %>% 
  arrange(
    `Subgroup Value`, Subject
  ) %>% 
  mutate(
    year = 2019
  )



# Clean 2018 data ----

percent_race_2018 <- raw_2018 %>% 
  filter(
    `Assessment Type` == "All",
    Subgroup == "Race/Ethnicity",
    `Tested Grade/Subject` == "All", 
    `Grade of Enrollment` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")),
    `Total Count` = as.numeric(`Total Number Valid Test Takers`),
    `Subgroup Value` = case_when(
      `Subgroup Value` == "American Indian/Alaskan Native" ~ "American Indian/Alaska Native",
      `Subgroup Value` == "Hispanic/Latino" ~ "Hispanic/Latino of any race",
      `Subgroup Value` == "Pacific Islander/Native Hawaiian" ~ "Native Hawaiian/Other Pacific Islander",
      `Subgroup Value` == "Two or More Races" ~ "Two or more races",
      `Subgroup Value` == "White/Caucasian" ~ "White",
      .default = `Subgroup Value`
    )
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Subgroup Value`)
  ) %>% 
  arrange(
    `Subgroup Value`, Subject
  ) %>% 
  mutate(
    year = 2018
  )



# Clean 2017 data ----

percent_race_2017 <- raw_2017 %>% 
  filter(
    `Assessment Type` == "All", 
    Subgroup == "Race/Ethnicity", 
    `Tested Grade/Subject` == "All", 
    `Grade of Enrollment` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")),
    `Total Count` = as.numeric(`Total Number Valid Test Takers`),
    `Subgroup Value` = case_when(
      `Subgroup Value` == "American Indian/Alaskan Native" ~ "American Indian/Alaska Native",
      `Subgroup Value` == "Hispanic/Latino" ~ "Hispanic/Latino of any race",
      `Subgroup Value` == "Pacific Islander/Native Hawaiian" ~ "Native Hawaiian/Other Pacific Islander",
      `Subgroup Value` == "Two or More Races" ~ "Two or more races",
      `Subgroup Value` == "White/Caucasian" ~ "White",
      .default = `Subgroup Value`
    )
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Subgroup Value`)
  ) %>% 
  arrange(
    `Subgroup Value`, Subject
  ) %>% 
  mutate(
    year = 2017
  )


# Appending all data ----

percent_race <- bind_rows(
  (percent_race_2023 %>% rename(Race = `Student Group Value`)), 
  (percent_race_2022 %>% rename(Race = `Subgroup Value`)),
  (percent_race_2019 %>% rename(Race = `Subgroup Value`)),
  (percent_race_2018 %>% rename(Race = `Subgroup Value`)),
  (percent_race_2017 %>% rename(Race = `Subgroup Value`))
) %>% 
  arrange(Race, Subject, year) %>%
  filter(!is.na(percent)) %>% 
  mutate(
    Race = case_when(
      Race == "Black/African American" ~ "Black",
      Race == "Hispanic/Latino of any race" ~ "Hispanic", 
      .default = Race
    )
  ) 



# -----------------------------------------------------------------------------#
# Export ----
# -----------------------------------------------------------------------------#

write_rds(
  percent_race,
  file = here('data', 'build', 'race_level_ela_math.rds')
)

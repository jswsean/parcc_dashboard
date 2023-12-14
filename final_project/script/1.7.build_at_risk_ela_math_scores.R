# Build average ELA-Math scores by gender. 

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
# Wrangle ----
# -----------------------------------------------------------------------------#


# Clean 2023 data ----

percent_at_risk_2023 <- raw_2023 %>% 
  filter(
    `Assessment Name` == "All", 
    `Student Group Value` == "Econ Dis",
    `Tested Grade/Subject` == "All", 
    `Grade of Enrollment` == "All",
    `School Framework` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(Percent, "%", "")), 
    `Total Count` = as.numeric(`Total Count`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject)
  ) %>% 
  mutate(year = 2023)


# Clean 2022 data ----

percent_at_risk_2022 <- raw_2022 %>% 
  filter(
    `Assessment Name` == "All", 
    `Subgroup Value` == "At-Risk", 
    `Tested Grade/Subject` == "All", 
    `Grade of Enrollment` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(Percent, "%", "")), 
    `Total Count` = as.numeric(`Total Count`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject)
  ) %>% 
  mutate(year = 2022)


# Clean 2019 data ----

percent_at_risk_2019 <- raw_2019 %>% 
  filter(
    `Assessment Type` == "All", 
    `Tested Grade/Subject` == "All", 
    `Subgroup Value` == "At-Risk", 
    `Grade of Enrollment` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")), 
    `Total Count` = as.numeric(`Total Number Valid Test Takers`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject)
  ) %>% 
  mutate(year = 2019)



# Clean 2018 data ----

percent_at_risk_2018 <- raw_2018 %>% 
  filter(
    `Assessment Type` == "All", 
    `Subgroup Value` == "At-Risk", 
    `Tested Grade/Subject` == "All", 
    `Grade of Enrollment` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")), 
    `Total Count` = as.numeric(`Total Number Valid Test Takers`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject)
  ) %>% 
  mutate(year = 2018)



# Clean 2017 data ----

percent_at_risk_2017 <- raw_2017 %>% 
  filter(
    `Assessment Type` == "All", 
    `Subgroup Value` == "At-Risk", 
    `Tested Grade/Subject` == "All", 
    `Grade of Enrollment` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")), 
    `Total Count` = as.numeric(`Total Number Valid Test Takers`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject)
  ) %>% 
  mutate(year = 2017)


# Append all data ----

percent_at_risk <- bind_rows(
  percent_at_risk_2017, 
  percent_at_risk_2018, 
  percent_at_risk_2019, 
  percent_at_risk_2022, 
  percent_at_risk_2023
) %>% 
  arrange(year, Subject)



# -----------------------------------------------------------------------------#
# Export ----
# -----------------------------------------------------------------------------#

# Export the results
write_rds(
  percent_at_risk, 
  file = here('data', 'build', 'risk_level_ela_math.rds')
)


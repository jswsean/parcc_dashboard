# Script to clean and build individual years' school-level
# PARCC ELA and math test data, from 2015-2023.

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

# Raw 2016 ELA
raw_2016_ela <- read_excel(
  here('data', 'osse', 'PARCC ELA school level 2015-16_0.xlsx'), 
  sheet = "hidden - pct_at_level"
)

# Raw 2016 Math
raw_2016_math <- read_excel(
  here('data', 'osse', 'PARCC Math school level 2015-16.xlsx'), 
  sheet = "hidden - pct_at_level"
)



# -----------------------------------------------------------------------------#
# Parse and clean data ----
# -----------------------------------------------------------------------------#



# Clean 2023 data ----

# Store variables to keep in this year
keep_vars <- c("LEA Code", "LEA Name", "School Code", "School Name", "Subject", "Count", "Percent")

# Getting overall ELA and Math scores by School level: 
school_level_2023 <- raw_2023 %>% 
  filter(
    Subject %in% c("ELA", "Math"),
    `Assessment Name` == "All",
    `Student Group` == "All Students", 
    `Tested Grade/Subject` == "All",
    `Grade of Enrollment` == "All",
    `School Framework` == "All"
  ) %>% 
  select(-Count) %>% 
  rename(Count = `Total Count`) %>% 
  select(all_of(keep_vars)) %>% 
  mutate(year = 2023)


# Clean 2022 data ----

# Store variables to keep in this year
keep_vars <- c("LEA Code", "lea_name", "School Code", "School Name", "Subject", "Count", "Percent")

# Getting overall ELA and Math scores by School level.
school_level_2022 <- raw_2022 %>% 
  filter(
    Subject %in% c("ELA", "Math"), 
    `Assessment Name` == "All", 
    `Student group` == "All",
    `Tested Grade/Subject` == "All",
    `Grade of Enrollment` == "All"
  ) %>% 
  select(-Count) %>% 
  rename(Count = `Total Count`) %>% 
  select(all_of(keep_vars)) %>% 
  rename(`LEA Name` = lea_name) %>% 
  mutate(year = 2022)


# Clean 2019 data ----

# Store variables to keep in this year
keep_vars <- c("LEA Code", "LEA Name", "School Code", "School Name", "Subject", 
               "Total Number Valid Test Takers", "Percent Meeting or Exceeding Expectations")

# Getting overall ELA and Math scores by School level.
school_level_2019 <- raw_2019 %>% 
  filter(
    Subject %in% c("ELA", "Math"), 
    `Assessment Type` == "All", 
    `Tested Grade/Subject` == "All", 
    `Grade of Enrollment` == "All", 
    Subgroup == "All",
    `Subgroup Value` == "All"
  ) %>% 
  select(all_of(keep_vars)) %>% 
  rename(
    Count = `Total Number Valid Test Takers`, 
    Percent = `Percent Meeting or Exceeding Expectations`
  ) %>% 
  mutate(year = 2019)


# Clean 2018 data ----

# Store variables to keep in this year
keep_vars <- c("LEA Code", "LEA Name", "School Code", "School Name", "Subject", 
               "Total Number Valid Test Takers", "Percent Meeting or Exceeding Expectations")


# Getting overall ELA and Math scores by School level.
school_level_2018 <- raw_2018 %>%
  filter(
    Subject %in% c("ELA", "Math"), 
    `Assessment Type` == "All", 
    `Tested Grade/Subject` == "All", 
    `Grade of Enrollment` == "All", 
    Subgroup == "All",
    `Subgroup Value` == "All"
  ) %>% 
  select(all_of(keep_vars)) %>% 
  rename(
    Count = `Total Number Valid Test Takers`, 
    Percent = `Percent Meeting or Exceeding Expectations`
  ) %>% 
  mutate(year = 2018)



# Clean 2017 data ----

# Store variables to keep in this year
keep_vars <- c("LEA Code", "LEA Name", "School Code", "School Name", "Subject", 
               "Total Number Valid Test Takers", "Percent Meeting or Exceeding Expectations")


# Getting overall ELA and Math scores by School level.
school_level_2017 <- raw_2017 %>%
  filter(
    Subject %in% c("ELA", "Math"), 
    `Assessment Type` == "All", 
    `Tested Grade/Subject` == "All", 
    `Grade of Enrollment` == "All", 
    Subgroup == "All",
    `Subgroup Value` == "All"
  ) %>% 
  select(all_of(keep_vars)) %>% 
  rename(
    Count = `Total Number Valid Test Takers`, 
    Percent = `Percent Meeting or Exceeding Expectations`
  ) %>% 
  mutate(year = 2017)



# Clean 2016 data ----

# Store variables to keep in this year
keep_vars <- c("LEA Code", "LEA Name", "School Code", "School Name", "Subject",
               "Count", "Percent")


# Getting overall ELA and Math scores by School level.
school_level_2016_ela <- raw_2016_ela %>%
  filter(
    `Tested Grade/Subject` == "All ELA",
    `School Code` != "Statewide"
  ) %>% 
  mutate(
    across(c(`% level 1`: `% level 5`), ~ as.numeric(.x)), 
    Percent = as.character((`% level 4` + `% level 5`)*100),
    Count = `Total valid test takers`,
    Subject = "ELA"
  ) %>% 
  select(all_of(keep_vars))

school_level_2016_math <- raw_2016_math %>%
  filter(
    `Tested Grade/Subject` == "All Math",
    `School Code` != "Statewide"
  ) %>% 
  mutate(
    across(c(`% level 1`: `% level 5`), ~ as.numeric(.x)), 
    Percent = as.character((`% level 4` + `% level 5`)*100),
    Count = `Total valid test takers`,
    Subject = "Math"
  ) %>% 
  select(all_of(keep_vars))

# Binding the two subject dfs
school_level_2016 <- bind_rows(
  school_level_2016_ela, 
  school_level_2016_math
) %>% 
  mutate(year = 2016)



# -----------------------------------------------------------------------------#
# Wrangle appended data ----
# -----------------------------------------------------------------------------#

# Bind all individual years' data 
school_level_ela_math <- bind_rows(
  school_level_2016, 
  school_level_2017, 
  school_level_2018, 
  school_level_2019, 
  school_level_2022, 
  school_level_2023
) %>% 
  relocate(year, .after = Subject) %>% 
  arrange(`School Code`, Subject, year)

# Clean the appended data 
school_level_ela_math <- school_level_ela_math %>% 
  mutate(
    
    # Due to non-standardized formatting, some values contain "%" 
    # while others do not. Remove all %
    Percent = str_replace_all(Percent, "%", ""),
    
    # Replace percent as NA if contains non-numeric character
    Percent = if_else(str_detect(Percent, "[A-Za-z]+"), NA_character_, Percent),
    
    # Replace count as NA if contains non-numeric character 
    Count = if_else(str_detect(Count, "[A-Za-z]+"), NA_character_, Count),
    
    # Convert count and percent as numeric 
    across(c(Count, Percent), ~ as.numeric(.x))
  ) %>% 
  
  # Removing missing Count and Percent values to 
  # remove single school units with missing count and percentage values 
  filter(!is.na(Count), !is.na(Percent))


# Get unique school names based on the most frequently occuring name. 
# If the frequency between two name is the same, select the first row only
unique_school_names <- school_level_ela_math %>% 
  count(`School Code`, `School Name`) %>% 
  mutate(max_n = max(n), .by = `School Code`) %>% 
  filter(n == max_n) %>% 
  select(-c(n, max_n)) %>% 
  mutate(row_seq = row_number(), .by = `School Code`) %>% 
  filter(row_seq == 1) %>% select(-row_seq)

# Get unique LEA names 
unique_lea_names <- school_level_ela_math %>% 
  count(`LEA Code`, `LEA Name`) %>% 
  mutate(max_n = max(n), .by = `LEA Code`) %>% 
  filter(n == max_n) %>% 
  select(-c(n, max_n))

# Remove the individual years' LEA/School names, replace with 
# harmonized ones 
school_level_ela_math <- school_level_ela_math %>% 
  select(-`School Name`, -`LEA Name`) %>% 
  inner_join(
    unique_lea_names, by = c("LEA Code")
  ) %>% 
  inner_join(
    unique_school_names, by = c("School Code")
  ) %>% 
  relocate(`LEA Name`, .after = `LEA Code`) %>% 
  relocate(`School Name`, .after = `School Code`)

# Create a balanced panel, t = year (incl 2020, 2021), x = schools
school_level_ela_math <- school_level_ela_math %>% 
  pivot_wider(id_cols = c(`LEA Code`:Subject),
              names_from = year, values_from = c(Count, Percent)) %>% 
  mutate(Count_2020 = NA_real_, 
         Count_2021 = NA_real_,
         Percent_2020 = NA_real_, 
         Percent_2021 = NA_real_) %>% 
  relocate(c(Count_2020, Count_2021), .after = Count_2019) %>% 
  relocate(c(Percent_2020, Percent_2021), .after = Percent_2019)


# Create a long-format panel data, with school IDs and Year+Percent as the columns.
school_level_ela_math_long <- school_level_ela_math %>% 
  pivot_longer(
    cols = c(Count_2016:Percent_2023), 
    names_to = c("Variable", "Year"),
    names_sep = "_",
    values_to = "Value"
  ) %>% 
  pivot_wider(
    id_cols = c(`LEA Code`:Subject, Year), 
    names_from = Variable,
    values_from = Value
  )

  
# -----------------------------------------------------------------------------#
# Export cleaned data ----
# -----------------------------------------------------------------------------#


# Export school-level ELA data 
write_rds(
  school_level_ela_math,
  here('data', 'build', 'school_level_ela_math.rds')
)

# Long school-level 
write_rds(
  school_level_ela_math_long, 
  here('data', 'build', 'school_level_ela_math_long.rds')
)

# Build 2023 school-level enrollment characteristics 

# ---------------------------------------------------------------------------- #
# Preliminaries ----
# ---------------------------------------------------------------------------- #

# Load required libraries
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)

# ---------------------------------------------------------------------------- #
# Load data ----
# ---------------------------------------------------------------------------- #

# Load the 2023 school-level report card 
enrollment_2023 <- read_excel(
  here('data', 'osse', '2023 DC School Report Card Aggregate Enrollment Data.xlsx'), 
  sheet = "Ever Enrolled Population"
)


# ---------------------------------------------------------------------------- #
# Wrangle ----
# ---------------------------------------------------------------------------- #

# Get only select student group rows
school_enrollment_chars <- enrollment_2023 %>% 
  filter(`Student Group` %in% 
           c("Black/African-American", "Hispanic/Latino of any race", 
             "Students with Disabilities", "Economically Disadvantaged"), 
         `School Code` != "All") %>% 
  select(`School Code`, `School Name`, `Student Group`, `Percent Enrolled`) %>% 
  mutate(
    `Student Group` = case_when(
      `Student Group` == "Black/African-American" ~ "black", 
      `Student Group` == "Hispanic/Latino of any race" ~ "hispanic", 
      `Student Group` == "Students with Disabilities" ~ "swd", 
      `Student Group` == "Economically Disadvantaged" ~ "econ_dis"
    )
  ) %>% 
  pivot_wider(
    id_cols = c("School Code", "School Name"), 
    names_from = `Student Group`, 
    names_prefix = "pct_",
    values_from = `Percent Enrolled`
  ) %>% 
  mutate(
    across(c(pct_black:pct_swd), ~ as.numeric(.x))
  )


# ---------------------------------------------------------------------------- #
# Export the results ----
# ---------------------------------------------------------------------------- #

write_rds(
  school_enrollment_chars,
  file = here('data', 'build', 'school_enrollment_chars_2023.rds')
)


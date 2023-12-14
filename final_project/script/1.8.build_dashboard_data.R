# Script to build analytic datasets that are used for the visualization

# -----------------------------------------------------------------------------#
# Preliminaries ----
# -----------------------------------------------------------------------------#

# Loading the required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(here)
library(sfheaders)
library(sf)


# -----------------------------------------------------------------------------#
# Load data ----
# -----------------------------------------------------------------------------#

# Loading the school ELA/Math scores
school_ela_math <- read_rds(
  here('data', 'build', 'school_level_ela_math.rds')
) %>%
  # Convert school code to numeric
  mutate(
    `School Code` = as.numeric(`School Code`)
  )

# Load the long-format school ELA/Math scores
school_ela_math_long <- read_rds(
  here('data', 'build', 'school_level_ela_math_long.rds')
) %>% 
  # Convert school code to numeric
  mutate(
    `School Code` = as.numeric(`School Code`)
  )

# Load the school location data 
school_location <- read_rds(
  here('data', 'build', 'school_location.rds')
)

# Convert school location data as df 
school_location_df <- sf_to_df(school_location, fill = T) 

# Load the wards spfile 
wards_dc <- st_read(
  here('final_project', 'script', 'parcc_score_dashboard', 'DC_Wards')
)

# Load the school enrollment characteristics in 2023
school_enrollment_chars <- read_rds(
  here('data', 'build', 'school_enrollment_chars_2023.rds')
)

# Load the school grade characteristics 
school_grades <- read_rds(
  here('data', 'build', 'school_grades.rds')
)


# -----------------------------------------------------------------------------#
# Wrangle ----
# -----------------------------------------------------------------------------#

# Create data for Plot 1: Time series of proficiency rates by subject across the years ----
subject_year_overall <-  school_ela_math_long %>% 
  summarise(
    Proficiency = weighted.mean(Percent, w = Count, na.rm = TRUE),
    .by = c(Subject, Year)
  ) %>% 
  mutate(
    # Specifying year to be highlighted 
    Highlights = if_else(Year %in% c("2019", "2022"), TRUE, FALSE)
  )


# Create data for Plot 2: Map of ward-level score change in 2022 ----

# Compute school-level score diffs:
school_level_df <- school_ela_math %>% 
  rowwise() %>% 
  mutate(avg_16_19 = mean(c(Percent_2016, Percent_2017, 
                            Percent_2018, Percent_2019), 
                          na.rm = T)) %>% 
  ungroup() %>% 
  mutate(
    
    # (2022-2019) - (2019 - 2016):
    did_22_19_16 = (Percent_2022 - Percent_2019) - (Percent_2019 - Percent_2016),
    
    # (2022-2019)
    diff_22_19 = (Percent_2022 - Percent_2019), 
    
    # (2022-2019), Normalized to Avg1619 
    diff_22_19_percent = (Percent_2022 - Percent_2019) / avg_16_19,
    
    # (2022 - mean(2016-2019))
    diff_22_avg1619 = Percent_2022 - avg_16_19
    
  ) %>% 
  left_join(
    school_location_df, 
    by = "School Code"
  ) %>% 
  select(-`School Name.y`) %>% 
  rename(`School Name` = `School Name.x`)


# Compute ward-level score difference, to be displayed in map
ward_scorediff_avg <- school_level_df %>% 
  rowwise() %>% 
  mutate(
    Count_1622 = sum(c(Count_2016, Count_2019, Count_2022), na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    Count_1622 = if_else(is.na(diff_22_avg1619), NA_real_, Count_1622)
  ) %>% 
  summarise(
    diff_22_avg1619 = weighted.mean(diff_22_avg1619, w = Count_1622, na.rm = TRUE),
    .by = c(LABEL, Subject)
  )


# Create data for plot 3a: Compute ward-level time series -----
ward_scores_time_series <- school_level_df %>% 
  select(`School Code`, `School Name`, Subject,
         starts_with("Count"), starts_with("Percent"), LABEL) %>% 
  pivot_longer(
    cols = c(starts_with("Count"), starts_with("Percent")),
    names_to = c(".value", "Variable"), 
    names_sep = "_"
  ) %>% 
  rename(year = Variable) %>% 
  summarise(mean_percent = weighted.mean(Percent, w = Count, na.rm = T), 
            .by = c(LABEL, year, Subject)) %>% 
  mutate(year = as.numeric(year))


# Data for plot 3b: grade_level_ela_math.rds ----
# Data for plot 3c: race_level_ela_math.rds  ----
# Data for plot 3d: risk_level_ela_math.rds  ----
# Data for plots 4 and 5: school_level df ----

# Use school-level df, merge with school enrollment chars and school grades data
school_level_df <- school_level_df %>% 
  left_join(
    (school_grades %>% select(-`School Name`) %>% mutate(`School Code` = as.numeric(`School Code`))), 
    by = "School Code"
  ) %>% 
  left_join(
    (school_enrollment_chars %>% select(-`School Name`) %>% mutate(`School Code` = as.numeric(`School Code`))), 
    by = "School Code"
  )


# -----------------------------------------------------------------------------#
# Export data to shiny app folder ----
# -----------------------------------------------------------------------------#

# Export data for plot 1: subject-year overall scores
write_rds(
  subject_year_overall, 
  here('final_project', 'script', 'parcc_score_dashboard', 'results_subject_year_overall.rds')
)

# Export data for plot 2: ward level score difference
write_rds(
  ward_scorediff_avg, 
  here('final_project', 'script', 'parcc_score_dashboard', 'results_ward_scorediff_avg.rds')
)

# Export data for plot 3a: time series of ward scores 
write_rds(
  ward_scores_time_series, 
  here('final_project', 'script', 'parcc_score_dashboard', 'results_ward_scores_time_series.rds')
)

# Export data for plot 3b: time series of scores by grade level 
file.copy(
  here('data', 'build', 'grade_level_ela_math.rds'), 
  here('final_project', 'script', 'parcc_score_dashboard', 'results_grade_level_ela_math.rds'),
  overwrite = TRUE
)

# Export data for plot 3c: time series of scores by race group 
file.copy(
  here('data', 'build', 'race_level_ela_math.rds'),
  here('final_project', 'script', 'parcc_score_dashboard', 'results_race_group_ela_math.rds'),
  overwrite = TRUE
)

# Export data for plot 3d: time series of scores by At-risk grouop 
file.copy(
  here('data', 'build', 'risk_level_ela_math.rds'),
  here('final_project', 'script', 'parcc_score_dashboard', 'results_risk_level_ela_math.rds'),
  overwrite = TRUE
)

# Export data for plots 4 and 5: school-level data for the scatterplot and table 
write_rds(
  school_level_df,
  here('final_project', 'script', 'parcc_score_dashboard', 'results_school_level.rds')
)





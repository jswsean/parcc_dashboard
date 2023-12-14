# Build school level identifier 

# ---------------------------------------------------------------------------- #
# Preliminaries ----
# ---------------------------------------------------------------------------- #

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

# ---------------------------------------------------------------------------- #
# Data load ----
# ---------------------------------------------------------------------------- #

# Load school-level ELA and Math scores data 
school_level_ela_math <- read_csv(
  here('data', 'build', 'school_level_ela_math.csv'), 
  show_col_types = FALSE
)

# Load the 2023 report card aggregate enrollment data 
enrollment_2023 <- read_excel(
  here('data', 'osse', '2023 DC School Report Card Aggregate Enrollment Data.xlsx'), 
  sheet = "Ever Enrolled Population"
)


# ---------------------------------------------------------------------------- #
# Wrangle ----
# ---------------------------------------------------------------------------- #


# From the school-level ELA-Math scores, we want to construct average  
# scores by grade levels.

# Get unique school codes from the 2016-2023 data, as the base school frame
schools_df <- school_level_ela_math %>% 
  distinct(`School Code`, `School Name`) %>% 
  filter(`School Code` != "All") %>% 
  mutate(
    `School Code` = as.character(`School Code`)
  )


# Get list of grades for each school from the 2023 report card data 
schools_grades_df <- enrollment_2023 %>% 
  filter(!Grade %in% c("All", "Adult"), `School Code` != "All") %>%
  distinct(`School Code`, Grade) %>% 
  summarise(
    grades = paste0(Grade, collapse = ","),
    .by = `School Code`
  ) %>% 
  mutate(
    
    # Identify whether school contains elementary, middle and high schools
    has_elementary_grades = if_else(
      str_detect(grades, "\\b[1-5]\\b|PK3|PK4|KG"), TRUE, FALSE 
    ), 
    
    has_middle_grades = if_else(
      str_detect(grades, "[6-8]"), TRUE, FALSE
    ), 
    
    has_high_grades = if_else(
      str_detect(grades, "9|10|11|12"), TRUE, FALSE
    )
  ) %>% 
  select(-grades)


# Merge grade list back to the original frame 
schools_df <- schools_df %>%
  left_join(
    schools_grades_df,
    by = "School Code"
  )

# Manually scan unmerged schools, and get details on these schools
# from net browsing
schools_df <- schools_df %>% 
  mutate(
    has_elementary_grades = case_when(

      `School Code` == "102" ~ FALSE, 
      `School Code` == "1100" ~ TRUE, 
      `School Code` == "1113" ~ TRUE,
      `School Code` == "1120" ~ FALSE,
      `School Code` == "1124" ~ FALSE,
      `School Code` == "117" ~ TRUE,
      `School Code` == "127" ~ FALSE,
      `School Code` == "131" ~ TRUE,
      `School Code` == "134" ~ TRUE,
      `School Code` == "153" ~ FALSE,
      `School Code` == "178" ~ FALSE,
      `School Code` == "187" ~ FALSE,
      `School Code` == "210" ~ TRUE,
      `School Code` == "234" ~ TRUE,
      `School Code` == "474" ~ FALSE,
      .default = has_elementary_grades
    ),
    
    has_middle_grades = case_when(
      
      `School Code` == "102" ~ TRUE, 
      `School Code` == "1100" ~ TRUE, 
      `School Code` == "1113" ~ TRUE,
      `School Code` == "1120" ~ FALSE,
      `School Code` == "1124" ~ TRUE,
      `School Code` == "117" ~ TRUE,
      `School Code` == "127" ~ TRUE,
      `School Code` == "131" ~ FALSE,
      `School Code` == "134" ~ FALSE,
      `School Code` == "153" ~ FALSE,
      `School Code` == "178" ~ FALSE,
      `School Code` == "187" ~ TRUE,
      `School Code` == "210" ~ TRUE,
      `School Code` == "234" ~ FALSE,
      `School Code` == "474" ~ TRUE,
      .default = has_middle_grades
    ),
    
    has_high_grades = case_when(
      
      `School Code` == "102" ~ FALSE,
      `School Code` == "1100" ~ FALSE, 
      `School Code` == "1113" ~ FALSE,
      `School Code` == "1120" ~ TRUE,
      `School Code` == "1124" ~ FALSE,
      `School Code` == "117" ~ FALSE,
      `School Code` == "127" ~ TRUE,
      `School Code` == "131" ~ FALSE,
      `School Code` == "134" ~ FALSE,
      `School Code` == "153" ~ TRUE,
      `School Code` == "178" ~ TRUE,
      `School Code` == "187" ~ TRUE,
      `School Code` == "210" ~ FALSE,
      `School Code` == "234" ~ FALSE,
      `School Code` == "474" ~ TRUE,
      .default = has_high_grades
)
  ) 


# ---------------------------------------------------------------------------- #
# Export result ----
# ---------------------------------------------------------------------------- #

saveRDS(
  schools_df,
  here('data', 'build', 'school_grades.rds')
)





# Build ELA-Math average scores by grade and by year. 
# Only from years 2017 - 2023, since breakdown in 2016 is non-granular.

# ---------------------------------------------------------------------------- #
# Preliminary ----
# ---------------------------------------------------------------------------- #

library(here)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

# ---------------------------------------------------------------------------- #
# Data load ----
# ---------------------------------------------------------------------------- #

# Import the raw excel files 

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



# ---------------------------------------------------------------------------- #
# Cleaning ----
# ---------------------------------------------------------------------------- #

# Clean 2023 data ----

# Getting overall ELA and Math scores for grades 3-8:
grades_3_8_2023 <- raw_2023 %>%
  filter(`Tested Grade/Subject` %in% c("Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8"), 
         `Assessment Name` == "All",
         `Student Group Value` == "All Students",
         `School Framework` == "All", 
         `Total Count` != "DS") %>% 
  mutate(
    percent = as.numeric(Percent), 
    `Total Count` = as.numeric(`Total Count`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Tested Grade/Subject`)
  )

# For grades 9-12:
grades_9_12_2023 <- raw_2023 %>% 
  filter(`Grade of Enrollment` %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12"), 
         `Assessment Name` == "All",
         `Student Group Value` == "All Students",
         `School Framework` == "All", 
         `Tested Grade/Subject` == "All",
         `Total Count` != "DS") %>% 
  mutate(
    percent = as.numeric(Percent), 
    `Total Count` = as.numeric(`Total Count`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Grade of Enrollment`)
  )

# Append the two DFs
percent_by_grade_2023 <- bind_rows(
  (grades_3_8_2023 %>% rename(Grade = `Tested Grade/Subject`)), 
  (grades_9_12_2023 %>% rename(Grade = `Grade of Enrollment`))
) %>% 
  mutate(
    Grade = as.numeric(str_replace(Grade, "Grade ", "")),
    year = 2023
  ) %>% 
  arrange(Grade, Subject) 




# Clean 2022 data ----


# Getting grades 3-8:
grades_3_8_2022 <- raw_2022 %>% 
  filter(`Tested Grade/Subject` %in% c("Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8"), 
         `Assessment Name` == "All",
         `Student group` == "All", 
         `Total Count` != "DS") %>% 
  mutate(
    percent = as.numeric(Percent), 
    `Total Count` = as.numeric(`Total Count`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Tested Grade/Subject`)
  )


# Getting grades 9-12:
grades_9_12_2022 <- raw_2022 %>% 
  filter(`Grade of Enrollment` %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12"),
         `Assessment Name` == "All",
         `Student group` == "All", 
         `Tested Grade/Subject` == "All",
         `Total Count` != "DS") %>% 
  mutate(
    percent = as.numeric(Percent), 
    `Total Count` = as.numeric(`Total Count`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Grade of Enrollment`)
  )
  

# Append the two DFs
percent_by_grade_2022 <- bind_rows(
  (grades_3_8_2022 %>% rename(Grade = `Tested Grade/Subject`)), 
  (grades_9_12_2022 %>% rename(Grade = `Grade of Enrollment`))
) %>% 
  mutate(
    Grade = as.numeric(str_replace(Grade, "Grade ", "")),
    year = 2022
  ) %>% 
  arrange(Grade, Subject) 



# Clean 2019 data ----


# Getting grades for grade 3-8:
grades_3_8_2019 <- raw_2019 %>% 
  filter(
    `Tested Grade/Subject` %in% c("Grade 3", "Grade 4", "Grade 5", 
                                  "Grade 6", "Grade 7", "Grade 8"),
    `Assessment Type` == "All",
    `Subgroup` == "All",
    `Subgroup Value` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")),
    `Total Count` = as.numeric(`Total Number Valid Test Takers`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Tested Grade/Subject`)
  )


# Getting grades for grade 9-12:
grades_9_12_2019 <- raw_2019 %>% 
  filter(
    `Grade of Enrollment` %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12"),
    `Assessment Type` == "All", 
    `Subgroup` == "All", 
    `Subgroup Value` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")),
    `Total Count` = as.numeric(`Total Number Valid Test Takers`)
  ) %>%
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE), 
    .by = c(Subject, `Grade of Enrollment`)
  )
  
# Append the two DFs
percent_by_grade_2019 <- bind_rows(
  (grades_3_8_2019 %>% rename(Grade = `Tested Grade/Subject`)), 
  (grades_9_12_2019 %>% rename(Grade = `Grade of Enrollment`))
) %>% 
  mutate(
    Grade = as.numeric(str_replace(Grade, "Grade ", "")),
    year = 2019
  ) %>% 
  arrange(Grade, Subject) 




# Clean 2018 data ----


# Cleaning grades 3-8: 
grades_3_8_2018 <- raw_2018 %>% 
  filter(
    `Tested Grade/Subject` %in% c("3", "4", "5","6", "7", "8"),
    `Assessment Type` == "All",
    `Subgroup` == "All",
    `Subgroup Value` == "All"
  ) %>%
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")),
    `Total Count` = as.numeric(`Total Number Valid Test Takers`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Tested Grade/Subject`)
  )


# Clean grades 9-12: 
grades_9_12_2018 <- raw_2018 %>% 
  filter(
    `Grade of Enrollment` %in% c("9", "10", "11", "12"),
    `Assessment Type` == "All", 
    `Subgroup` == "All", 
    `Subgroup Value` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")),
    `Total Count` = as.numeric(`Total Number Valid Test Takers`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Grade of Enrollment`)
  )

# Append the two DFs
percent_by_grade_2018 <- bind_rows(
  (grades_3_8_2018 %>% rename(Grade = `Tested Grade/Subject`)), 
  (grades_9_12_2018 %>% rename(Grade = `Grade of Enrollment`))
) %>% 
  mutate(
    Grade = as.numeric(str_replace(Grade, "Grade ", "")),
    year = 2018
  ) %>% 
  arrange(Grade, Subject) 



# Clean 2017 data ----


# Skip grades 9-12 since no granular breakdown is provided in 2017.
# Clean grades 3-8:
percent_by_grade_2017 <- raw_2017 %>% 
  filter(
    `Tested Grade/Subject` %in% c("3", "4", "5","6", "7", "8"),
    `Assessment Type` == "All",
    `Subgroup` == "All",
    `Subgroup Value` == "All"
  ) %>% 
  mutate(
    percent = as.numeric(str_replace(`Percent Meeting or Exceeding Expectations`, "%", "")),
    `Total Count` = as.numeric(`Total Number Valid Test Takers`)
  ) %>% 
  summarise(
    percent = weighted.mean(percent, w = `Total Count`, na.rm = TRUE),
    .by = c(Subject, `Tested Grade/Subject`)
  ) %>% 
  rename(Grade = `Tested Grade/Subject`) %>% 
  mutate(
    Grade = as.numeric(str_replace(Grade, "Grade ", "")),
    year = 2017
  ) %>% 
  arrange(Grade, Subject) 



# Combine all data ----

# Bind all rows, reshape to create strong panel
percent_by_grade <- bind_rows(
  percent_by_grade_2017, 
  percent_by_grade_2018, 
  percent_by_grade_2019,
  percent_by_grade_2022, 
  percent_by_grade_2023
) %>% 
  pivot_wider(
    id_cols = Grade,
    names_from = c(Subject, year), 
    names_sep = "_",
    values_from = percent
  ) %>% 
  pivot_longer(
    cols = c(ELA_2017:Math_2023), 
    names_to = c("Subject", "Year"), 
    names_sep = "_",
    values_to = "Percent"
  ) %>% 
  mutate(
    Year = as.numeric(Year)
  )

# Generate additional columns for the plot
percent_by_grade <- percent_by_grade %>% 
  mutate(
    # Different colors for 2019 and 2022. 
    Highlight = if_else(Year %in% c(2019, 2022), TRUE, FALSE)
  )


# Mock graph:
# library(ggplot2)
# percent_by_grade %>% 
#   filter(Grade == 3, Subject == "ELA") %>%
#   ggplot(aes(x = Year, y = Percent)) +
#   geom_col(aes(fill = Highlight)) +
#   geom_text(aes(label = format(Percent, digits = 2)), 
#             vjust = 1.5, size = 4, color = "white") +
#   geom_point(
#     data = (percent_by_grade %>% filter(Grade == 3, Subject == "ELA", Highlight == TRUE)),
#     aes(x = Year, y = Percent),
#     color = "#FF9900", size = 4
#   ) +
#   geom_line(
#     data = (percent_by_grade %>% filter(Grade == 3, Subject == "ELA", Highlight == TRUE)),
#     aes(x = Year, y = Percent),
#     color = "#FF9900", size = 1.5, linetype = "dotdash"
#   ) +
#   scale_x_continuous(breaks = seq(2017, 2023)) +
#   scale_fill_manual(values = c("#CCCCCC", "#FF0000")) +
#   theme_classic() +
#   theme(
#     axis.ticks.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.line.y = element_blank(),
#     axis.title.y = element_blank(),
#     legend.position = "none"
#   )


# ---------------------------------------------------------------------------- #
# Export ----
# ---------------------------------------------------------------------------- #

write_rds(
  percent_by_grade,
  file = here('data', 'build', 'grade_level_ela_math.rds')
)


# DC PARCC Score Test Dashboard 

# -----------------------------------------------------------------------------#
# Preliminaries ----
# -----------------------------------------------------------------------------#

# Loading the required packages
#library(shiny)
library(shinythemes)
library(readr)
library(tidyr)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(reactable)
library(sparkline)

# -----------------------------------------------------------------------------#
# Load data ----
# -----------------------------------------------------------------------------#

# Load subject-year overall scores data
results_subject_year_overall <- read_rds(
  'results_subject_year_overall.rds'
)

# Load ward maps data 
results_ward_scorediff_avg <- read_rds(
  'results_ward_scorediff_avg.rds'
)

# Load ward data  
results_ward_time_series <- read_rds(
  'results_ward_scores_time_series.rds'
)

# Load grade-level data
results_grade_level_ela_math <- read_rds(
  'results_grade_level_ela_math.rds'
)

# Load race group level data 
results_race_group_ela_math <- read_rds(
  'results_race_group_ela_math.rds'
)

# Load at-risk level data 
results_risk_level_ela_math <- read_rds(
  'results_risk_level_ela_math.rds'
)

# Load school-level wide data 
results_school_level <- read_rds(
  'results_school_level.rds'
)

# Load DC ward maps data 
dc_wards <- st_read(
  'DC_Wards'
)

# -----------------------------------------------------------------------------#
# Wrangling ----
# -----------------------------------------------------------------------------#

# In subject-year data, convert year to numeric 
results_subject_year_overall <- results_subject_year_overall %>% 
  mutate(
    Year = as.numeric(Year)
  )

# In grades data, remove grades 9-12 since there's a lot of 
# missing values for these grades 
results_grade_level_ela_math <- results_grade_level_ela_math %>% 
  filter(Grade < 9)


# -----------------------------------------------------------------------------#
# Initializing input variables ----
# -----------------------------------------------------------------------------#

# Define input choices for subject_input
subjects <- results_school_level %>% 
  distinct(Subject) %>% 
  pull(Subject)

# Create long-format school data for the reactable 
results_school_time_series <- results_school_level %>%
  select(`School Code`, `School Name`, Subject, starts_with("Percent_")) %>% 
  pivot_longer(cols = starts_with("Percent"), names_to = "year", values_to = "Percent") %>% 
  mutate(
    year = as.numeric(str_replace_all(year, "Percent_", ""))
  ) %>% 
  arrange(`School Code`, Subject, year) %>% 
  summarise(
    Percent = list(Percent), 
    .by = c("School Code", "Subject")
  )


# -----------------------------------------------------------------------------#
# Define UI ----
# -----------------------------------------------------------------------------#

ui <- navbarPage(theme = shinytheme("cosmo"),
  
  # Dashboard title ----
  title = "Charting the Gap: Visualizing the Inequality of Pandemic-Induced Student Learning Loss in Washington, D.C.",
  
  # Define tabs ----
  tabsetPanel(
    
    # First tab ----
    tabPanel(
      
      # Tab title
      "Overall student learning loss",
      
      # Vertical space 
      br(), 
      
      # Fluid row, for input, graph, and narrative text.
      fluidRow(
        
        
        # First column: Input and graph output
        column(
          width = 7, style = "padding:30px",
          
          h3(strong("The pandemic caused significant learning losses throughout D.C.")),
          
          br(),
          
          # Input box 
          selectInput(
            inputId = "subject_input", 
            label = "Select the assessment subject:",
            choices = subjects
          ), 
          
          plotlyOutput("subject_year_overall"),
          
          br(), 
          
          p("Source: D.C. Office of the State Superintendent of Education (2023). The proficiency rate is based on the
            Partnership for Assessment of Readiness for College and Careers (PARCC) and Multi-State Alternate Assessments
            (MSAA) D.C. statewide assessments. Proficiency rate is defined as the percentage of test takers that meet or exceed 
            expectations (4+). The orange line indicates the immediate 2019-2022 decline in proficiency rates.
            Bar charts are missing for 2020 and 2021 as PARCC/MSAA assessments were not implemented during at-home learning.",
            style = "font-size:12px")
        ),
        
        # Second column: Text narrative
        column(
          width = 5, style = "font-size:18px; padding:60px",
          
          p("In early 2020, COVID-19 began spreading across the U.S."),
          
          br(), 
          
          p("The first case in D.C. was reported on March 7, 2020. 
            Since then, the government had enacted various mobility restriction measures to curb the spread of the virus,
            and among the most consequential measures were school closures."),
          
          br(),
          
          p("On March 13, the District of Columbia Public Schools (DCPS) announced two-weeks school shutdowns. Learning remained 
            fully virtual until November 2020, after which elementary schools began in-person classes. It was only in February 2021
            that students would revert to in-person learning."), 
          
          br(),
          
          p(HTML("As a result, <b>there have been widespread losses in student learning</b> across D.C.")),
          
          br(),
          
          p(HTML("The graph to the left shows that proficiency in ELA decreases by 6.3 percentage points from 2019 to 2022, 
            eradicating <b>1.8 years of learning gain progress</b>. Losses are even steeper for Math subject,
            where proficiency declines by 10.4 points, or equivalent to <b>5.4 years of learning gains</b>."))
        )
        
      ),
      
    ), 

    
    # Second tab ----
    tabPanel(
      
      # Tab title
      "Variation in learning losses across student groups, grades, and areas",
      
      # Add vertical space
      br(),
      
      
      # Row for Plot 2: Map of ward score diff
      fluidRow(
        
        # Column 1: For inputs 
        column(width = 2,
          
               
          # Input for subject
          selectInput(
            inputId = "subject_input_ward_map",
            label = "Select the assessment subject to be displayed on the map:",
            choices = subjects
          )
          
        ),
        
        # Column 2: For map
        column(width = 5,
               
          h3(strong("Wards 7 and 8 experienced steeper learning losses.")),
          
          br(),
          
          plotlyOutput("wards_map"), 
          
          br(),
          
          # Add caption
          p("Source: D.C. Office of the State Superintendent of Education (2023). 
            The fill gradient indicates the percentage points difference between 
            each ward's 2022 Math/ELA proficiency rates and their 2016-2019 averages.
            The proficiency rate is based on the Partnership for Assessment of Readiness for College and Careers 
            (PARCC) and Multi-State Alternate Assessments (MSAA) D.C. statewide assessments, and is defined as 
            the percentage of test takers that meet or exceed expectations (4+).",
            style = "font-size: 12px")
               
        ),
        
        
        # Column 3: For output narrative
        column(width = 4, style = "font-size:18px; padding: 70px",
          
          p(HTML("However, <b>not every students is equally affected</b>.
                 There is a strong socioeconomic aspect to the learning loss issue.")), 
          
          br(),
          
          p(HTML("The map to the left shows the learning loss magnitude across D.C. wards.
                <b>Wards 7 and 8—two wards with the lowest median income—tend to experience the steepest learning losses</b>, 
                 particularly for Math subject.")), 
          
          br(),
          
          p("A lot of factors could explain the differences in learning loss along socioeconomic lines. For example, 
            students from less well-off families typically have less proper digital learning infrastructure at home, 
            and their parents might likely have lesser time to assist in at-home learning."), 
          
          br(),
          
          p("Or, it could also be 
            that schools in lower-income neighborhoods lack the resources to facilitate remote instructions.")
               
        )
      ),
      
      br(),
      
      # Row for plot 3: Heterogeneity exploration
      fluidRow(
        
        # Column 1: Input
        column(width = 2,
          
          # Input for subject 
          selectInput(
            inputId = "subject_input_bar_charts",
            label = "Select the assessment subject for the bar chart:",
            choices = subjects
          ), 
          
          br(), 
          
          # Input for the exploration parameter
          radioButtons(
            inputId = "pivot_var",
            label = "Select dimension:",
            choices = c("By wards", "By grades", "By race group", "By risk"),
            selected = "By wards"
          )
          
        ), 
        
        
        # Column 2: Plot
        column(width = 5,
          
          h3(strong("Vulnerable population groups faced disproportionate learning losses.")),
          
          br(),
               
          # PlotlyOutput
          plotlyOutput('proficiency_plot'),
          
          br(), 
          
          # Add caption
          p("Source: D.C. Office of the State Superintendent of Education (2023). The proficiency rate is based on the
            Partnership for Assessment of Readiness for College and Careers (PARCC) and Multi-State Alternate Assessments
            (MSAA) D.C. statewide assessments. Proficiency rate is defined as the percentage of test takers within each group 
            that meet or exceed expectations (4+). The orange line indicates the immediate 2019-2022 decline in proficiency rates.
            Bar charts are missing for 2020 and 2021 as PARCC/MSAA assessments were not implemented during the at-home learning.",
            style = "font-size:12px")
          
        ),
        
        
        # Column 3: Narrative
        column(width = 4, style = "font-size:18px; padding: 70px",
          
          p(HTML("Regardless of the reason, it is clear that students that are <b>of lower-income families</b>,
                 <b>are economically-disadvantaged</b>, and <b>are Black</b> faced disproportionate 
                 learning losses.")), 
          
          br(),
          
          p(HTML("Worryingly, learning losses, especially in Math, tend to be concentrated among 
                 <b>early primary students</b> (grades 3 and 4), which could potentially jeopardize
                 long-term human capital."))
        ) 
        
      )
    ), 
    
    # Third tab ----
    tabPanel(
      
      # Tab title
      "Tracking learning loss at the school-level", 
      
      # Vertical space
      br(),
      
      # Row for plot 4: school-level scatterplot.
      fluidRow(
        
        # Column 1: Input fields
        column(width = 2,
          
          # Subject Input
          radioButtons(
            inputId = "subject_input_scatterplot",
            label = "Select the assessment subject for the scatterplot:", 
            choices = subjects
          ), 
          
          br(), 
          
          # School dimension inputs
          selectInput(
            inputId = "dimension_scatterplot",
            label = "Select variable to be plotted on horizontal axis:",
            choices = c("Median household income", "% Black", 
                        "% economically disadvantaged", "% Hispanic", 
                        "% with disabilities")
          )
        ),
        
        # Column 2: Output
        column(width = 6,
          
          # Plot output 
          plotlyOutput("scatterplot_exploration"),
          
          br(),
          
          p("Source: D.C. Office of the State Superintendent of Education (2023). Each points indicate D.C. schools. 
            The y-axis indicates the percentage points difference between 
            each school's 2022 Math/ELA proficiency scores and the 
            2016-2019 average. 
            The proficiency rate is based on the Partnership for Assessment of Readiness for College and Careers 
            (PARCC) and Multi-State Alternate Assessments (MSAA) D.C. statewide assessments, and is defined as 
            the percentage of test takers that meet or exceed expectations (4+). ",
            style = "font-size: 12px")
          
        ),
        
        # Column 3: Text narrative
        column(width = 3, style = "font-size:18px; padding: 40-px",
          
          p(HTML("Of particular relevance to policymakers is <b>the identification of which schools have the greatest need 
            for policy intervention</b>.")),
          
          br(),
          
          p("This dashboard page provides that functionality; the chart to the left
            provides an interactive scatterplot that 
            allows users to hover across the points and see which specific school 
            corresponds to a particular point."),
          
          br(),
          
          p("For example, one might want to focus intervention on schools located in the
            bottom left of the chart, since those are schools in the least well-off neighborhoods that 
            experienced the steepest learning losses."), 
        )
      ),
      
      
      br(), br(), br(), br(),
      
      # Second row: school-level table
      fluidRow(
        
        # First column: table inputs
        column(width = 2,
          
          # Subject input
          selectInput(
            inputId = "subject_input_table",
            label = "Select the assessment subject for the tracker table:",
            choices = subjects
          ),
          
          br(), 
          
          # Ward input
          selectInput(
            inputId = "ward_input_table",
            label = "Select specific wards: ",
            choices = c("All", "Ward 1", "Ward 2", "Ward 3", "Ward 4", 
                        "Ward 5", "Ward 6", "Ward 7", "Ward 8")
          ),
          
          br(), 
          
          # Grade input
          selectInput(
            inputId = "grade_input_table",
            label = "Select specific school grades: ", 
            choices = c("All", 
                        "Schools with elementary grades", 
                        "Schools with middle school grades", 
                        "Schools with high school grades")
          )
          
          
        ),
        
        
        # Second column: ReactTable Output
        column(width = 6,
               
          reactableOutput("school_table"),
          
          br(),
          
          p("Source: D.C. Office of the State Superintendent of Education (2023). The 
            fourth column visualizes the 2016-2023 proficiency trend, which is based on the 
            Partnership for Assessment of Readiness for College and Careers 
            (PARCC) and Multi-State Alternate Assessments (MSAA) D.C. statewide assessments, and is defined as 
            the percentage of test takers that meet or exceed expectations (4+). 
            Years 2020 and 2021 are missing from the trend line because PARCC/MSAA assessments
            were not conducted during at-home learning.",
            style = "font-size: 12px")
          
        ),
        
        # Third column: text narrative
        column(width = 3, style= "font-size: 18px",
          
          p("The table on the left contains specific details and the proficiency trends of each schools,
            and users can interactively sort or filter schools to identify those with steepest learning losses."),
          
          br(),
          
          p("What interventions, then, can work to mitigate these learning losses?"), 
          
          br(), 
          
          p(HTML("<a href='https://www.povertyactionlab.org/publication/transformative-potential-tutoring-pre-k-12-learning-outcomes-lessons-randomized?utm_source=the74&utm_medium=oped&utm_campaign=Tutoring_Evidence_Review#:~:text=J%2DPAL%27s%20Tutoring%20Evidence%20Review,the%20growth%20of%20academic%20disparities'>
                 A growing body of evidence</a> suggests that one promising avenue of intervention is <b>tutoring</b>.")),
          
          br(),
          
          p("When properly implemented, tutoring programs have been shown to consistently improve student learning outcomes, 
                 with an effect size of 0.37 standard deviations. This translates to a student's advancement from the 50th to 66th score percentile."),
          
          br(),
          
          p("Policymakers should consider designing a large-scaled tutoring program led by teachers or paraprofessionals."),
          
          br(),
          
          p("In addition, there is large value in focusing effort on students of earlier grades, 
            where steepest learning losses tend to occur, and also where the program can potentially yield the largest impacts.")
                 
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------#
# Server ----
# -----------------------------------------------------------------------------#

server <- function(input, output, session) {
  
  # Output 1: Subject-year time series plot  ----
  output$subject_year_overall <- renderPlotly({
    
    # Filter Subject: 
    plot <- results_subject_year_overall %>% 
      filter(
        Subject == input$subject_input
      ) %>% 
      ggplot(aes(x = Year, y = Proficiency)) +
      geom_col(aes(fill = Highlights, 
                   text = paste('Proficiency:', format(Proficiency, digits = 3)))) +
      geom_point(
        data = (results_subject_year_overall %>% 
                  filter(Subject == input$subject_input, 
                         Highlights == TRUE)),
        aes(x = Year, y = Proficiency), 
        color = "#FF9900", 
        size = 4
      ) +
      geom_line(
        data = (results_subject_year_overall %>% 
                  filter(Subject == input$subject_input, 
                         Highlights == TRUE)),
        aes(x = Year, y = Proficiency),
        color = "#FF9900", 
        linewidth = 1.5, 
        linetype = "dotdash"
      ) +
      scale_fill_manual(values = c("#CCCCCC", "#FF0000")) +
      scale_x_continuous(breaks = seq(2016, 2023)) +
      labs(
        x = "Year", y = "Proficiency Rate (% 4+)"
      ) +
      theme_classic() +
      theme(
        legend.position = "none"
      )
    
    # Show plotly
    ggplotly(plot, tooltip = c("text"))
  })
  
  
  
  
  # Output 2: Wards score-diff map ----
  output$wards_map <- renderPlotly({
    
    # Filtering the wards data to chosen subject, and joining the score diff
    wards_map_df <- dc_wards %>%
    inner_join(results_ward_scorediff_avg, by = "LABEL") %>%
    filter(Subject == input$subject_input_ward_map)


    # Create the static plot
    wards_map <- ggplot(wards_map_df) +
      geom_sf(aes(fill = diff_22_avg1619, 
                  text = paste0("Ward: ", WARD, 
                                "<br>", "Score difference: ", format(diff_22_avg1619, digits = 3))), 
              color = "white", linewidth = 1) +
      labs(fill = "Difference relative to \n2016-2019 average (p.p.):") +
      scale_fill_gradient(low = "red", high = "#FDDF03", na.value = NA) +
      theme_void() +
      theme(
        axis.line.x = element_blank(),
        axis.line.y = element_blank()
      )
    
    
    # Plotly
    ggplotly(wards_map, tooltip = "text") %>% style(hoveron = "fills")

  })
  
  
  
  
  # Output 3: Proficiency tabulations ----
  output$proficiency_plot <- renderPlotly({
    
    if (input$pivot_var == "By wards") {
      
      # Use ward time series data, filter on subject, indicate years to be highlighted
      plot_df <- results_ward_time_series %>% 
        filter(Subject == input$subject_input_bar_charts) %>% 
        mutate(
          Highlights = if_else(year %in% c(2019,2022), TRUE, FALSE)
        )
      
      # Define static plot
      static_plot <- plot_df %>%  
        ggplot(aes(x = year, y = mean_percent)) +
        geom_col(aes(fill = Highlights, 
                     text = paste('Proficiency:', format(mean_percent, digits = 3)))) +
        geom_point(
          data = (plot_df %>% 
                    filter(Highlights == TRUE)),
          aes(x = year, y = mean_percent), 
          color = "#FF9900", 
          size = 1.5
        ) +
        geom_line(
          data = (plot_df %>% 
                    filter(Highlights == TRUE)),
          aes(x = year, y = mean_percent),
          color = "#FF9900", 
          linewidth = 1, 
          linetype = "dotdash"
        ) +
        facet_wrap(~ LABEL, nrow = 3) +
        scale_fill_manual(values = c("#CCCCCC", "#FF0000")) +
        scale_x_continuous(breaks = seq(2016, 2023)) +
        labs(
          x = "Year", y = "Proficiency Rate (% 4+)"
        ) +
        theme_classic() +
        theme(
          axis.text.x = element_text(size = 4),
          legend.position = "none"
        )
      
      # Plotly
      ggplotly(static_plot, tooltip = "text")
      
    } else if (input$pivot_var == "By grades") {
      
      
      # Use grade time series data, filter on subject, indicate years to be highlighted.
      plot_df <- results_grade_level_ela_math %>% 
        filter(Subject == input$subject_input_bar_charts)
      
      
      # Define static plot
      static_plot <- plot_df %>%  
        ggplot(aes(x = Year, y = Percent)) +
        geom_col(aes(fill = Highlight, 
                     text = paste('Proficiency:', format(Percent, digits = 3)))) +
        geom_point(
          data = (plot_df %>% 
                    filter(Highlight == TRUE)),
          aes(x = Year, y = Percent), 
          color = "#FF9900", 
          size = 1.5
        ) +
        geom_line(
          data = (plot_df %>% 
                    filter(Highlight == TRUE)),
          aes(x = Year, y = Percent),
          color = "#FF9900", 
          linewidth = 1, 
          linetype = "dotdash"
        ) +
        facet_wrap(~ Grade, ncol = 3) +
        scale_fill_manual(values = c("#CCCCCC", "#FF0000")) +
        scale_x_continuous(breaks = seq(2017, 2023)) +
        labs(
          x = "Year", y = "Proficiency Rate (% 4+)"
        ) +
        theme_classic() +
        theme(
          axis.text.x = element_text(size = 4),
          legend.position = "none"
        )
      
      # Plotly
      ggplotly(static_plot, tooltip = "text")
      
    } else if (input$pivot_var == "By race group") {
      
      # Use race-group time series data, filter on subject, indicate years to be highlighted.
      plot_df <- results_race_group_ela_math %>% 
        filter(Subject == input$subject_input_bar_charts) %>% 
        mutate(
          Highlights = if_else(year %in% c(2019,2022), TRUE, FALSE)
        )
      
      
      # Define static plot
      static_plot <- plot_df %>%  
        ggplot(aes(x = year, y = percent)) +
        geom_col(aes(fill = Highlights, 
                     text = paste('Proficiency:', format(percent, digits = 3)))) +
        geom_point(
          data = (plot_df %>% 
                    filter(Highlights == TRUE)),
          aes(x = year, y = percent), 
          color = "#FF9900", 
          size = 1.5
        ) +
        geom_line(
          data = (plot_df %>% 
                    filter(Highlights == TRUE)),
          aes(x = year, y = percent),
          color = "#FF9900", 
          linewidth = 1, 
          linetype = "dotdash"
        ) +
        facet_wrap(~ Race, nrow = 2) +
        scale_fill_manual(values = c("#CCCCCC", "#FF0000")) +
        scale_x_continuous(breaks = seq(2017, 2023)) +
        labs(
          x = "Year", y = "Proficiency Rate (% 4+)"
        ) +
        theme_classic() +
        theme(
          axis.text.x = element_text(size = 4),
          legend.position = "none"
        )
      
      # Plotly
      ggplotly(static_plot, tooltip = "text")
      
      
    } else if (input$pivot_var == "By risk") {
      
      # Use at-risk time series data, filter on subject, indicate years to be highlighted.
      plot_df <- results_risk_level_ela_math %>% 
        filter(Subject == input$subject_input_bar_charts) %>% 
        mutate(
          Highlights = if_else(year %in% c(2019,2022), TRUE, FALSE)
        )
      
      
      # Define static plot
      static_plot <- plot_df %>%  
        ggplot(aes(x = year, y = percent)) +
        geom_col(aes(fill = Highlights, 
                     text = paste('Proficiency:', format(percent, digits = 3)))) +
        geom_point(
          data = (plot_df %>% 
                    filter(Highlights == TRUE)),
          aes(x = year, y = percent), 
          color = "#FF9900", 
          size = 1.5
        ) +
        geom_line(
          data = (plot_df %>% 
                    filter(Highlights == TRUE)),
          aes(x = year, y = percent),
          color = "#FF9900", 
          linewidth = 1, 
          linetype = "dotdash"
        ) +
        scale_fill_manual(values = c("#CCCCCC", "#FF0000")) +
        scale_x_continuous(breaks = seq(2017, 2023)) +
        labs(
          x = "Year", y = "Proficiency Rate (% 4+)"
        ) +
        theme_classic() +
        theme(
          axis.text.x = element_text(size = 6),
          legend.position = "none"
        )
      
      # Plotly
      ggplotly(static_plot, tooltip = "text")
      
    }
    
  })
  
  
  
  
  # Output 4: Scatterplot exploration ----
  output$scatterplot_exploration <- renderPlotly({
    
    # Filtering on subject of interest 
    plot_df <- results_school_level %>% 
      filter(Subject == input$subject_input_scatterplot)
    
    if (input$dimension_scatterplot == "Median household income") {
      
      # Static plot
      plot <- ggplot(plot_df, aes(x = log(blockgroup_hhinc_2019_est),
                                  y = diff_22_avg1619)) +
        geom_point(aes(text = paste0("School: ", `School Name`, "<br>", 
                                     "Score diff.: ", format(diff_22_avg1619, digits = 3), "<br>", 
                                     "Median HH block group income (2019): ", format(blockgroup_hhinc_2019_est, big.mark = ",")))) +
        geom_smooth() +
        geom_hline(yintercept = 0, color = "red") +
        labs(
          x = "Log 2019 block group median household income",
          y = "2022 score difference r.t. 2016-2019 average"
          ) +
        theme_classic() +
        theme(
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.text = element_text(size = 9)
        )
      
      # Plotly
      ggplotly(plot, tooltip = "text")
      
    } else if (input$dimension_scatterplot == "% Black") {
      
      # Static plot
      plot <- ggplot(plot_df, aes(x = pct_black,
                                  y = diff_22_avg1619)) +
        geom_point(aes(text = paste0("School: ", `School Name`, "<br>", 
                                     "Score diff.: ", format(diff_22_avg1619, digits = 3), "<br>", 
                                     "% Black students (2023): ", format(pct_black, digits = 3)))) +
        geom_smooth() +
        geom_hline(yintercept = 0, color = "red") +
        labs(
          x = "% Black students in the school (2023)",
          y = "2022 score difference r.t. 2016-2019 average"
        ) +
        theme_classic() +
        theme(
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.text = element_text(size = 9)
        )
      
      # Plotly
      ggplotly(plot, tooltip = "text")
      
    } else if (input$dimension_scatterplot == "% economically disadvantaged") {
      
      
      # Static plot
      plot <- ggplot(plot_df, aes(x = pct_econ_dis,
                                  y = diff_22_avg1619)) +
        geom_point(aes(text = paste0("School: ", `School Name`, "<br>", 
                                     "Score diff.: ", format(diff_22_avg1619, digits = 3), "<br>", 
                                     "% economically disadvantaged (2023): ", format(pct_econ_dis, digits = 3)))) +
        geom_smooth() +
        geom_hline(yintercept = 0, color = "red") +
        labs(
          x = "% economically-disadvantaged students (2023)",
          y = "2022 score difference r.t. 2016-2019 average"
        ) +
        theme_classic() +
        theme(
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.text = element_text(size = 9)
        )
      
      # Plotly
      ggplotly(plot, tooltip = "text")
      
    } else if (input$dimension_scatterplot == "% Hispanic") {
      
      
      # Static plot
      plot <- ggplot(plot_df, aes(x = pct_hispanic,
                                  y = diff_22_avg1619)) +
        geom_point(aes(text = paste0("School: ", `School Name`, "<br>", 
                                     "Score diff.: ", format(diff_22_avg1619, digits = 3), "<br>", 
                                     "% Hispanic students (2023): ", format(pct_hispanic, digits = 3)))) +
        geom_smooth() +
        geom_hline(yintercept = 0, color = "red") +
        labs(
          x = "% Hispanic students in the school (2023)",
          y = "2022 score difference r.t. 2016-2019 average"
        ) +
        theme_classic() +
        theme(
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.text = element_text(size = 9)
        )
      
      # Plotly
      ggplotly(plot, tooltip = "text")
      
    } else if (input$dimension_scatterplot == "% with disabilities") {
      
      
      # Static plot
      plot <- ggplot(plot_df, aes(x = pct_swd,
                                  y = diff_22_avg1619)) +
        geom_point(aes(text = paste0("School: ", `School Name`, "<br>", 
                                     "Score diff.: ", format(diff_22_avg1619, digits = 3), "<br>", 
                                     "% students with disabilities (2023): ", format(pct_swd, digits = 3)))) +
        geom_smooth() +
        geom_hline(yintercept = 0, color = "red") +
        labs(
          x = "% students with disabilities in the school (2023)",
          y = "2022 score difference r.t. 2016-2019 average"
        ) +
        theme_classic() +
        theme(
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.text = element_text(size = 9)
        )
      
      # Plotly
      ggplotly(plot, tooltip = "text")
    }
    
  })
  
  
  
  
  # Output 5: ReactTable Output ----
  output$school_table <- renderReactable({
    
    # Base dataframe:
    school_level_df <- results_school_level %>% 
      filter(Subject == input$subject_input_table, !is.na(diff_22_avg1619))
    
    # Filter layer 1: Ward
    if (input$ward_input_table != "All") {
      
      # Further filter based on ward input 
      school_level_df <- school_level_df %>% 
        filter(LABEL == input$ward_input_table)
      
    }
    
    # Filter layer 2: based on school-grades 
    if (input$grade_input_table == "Schools with elementary grades") {
      
      # Filter to schools with ES grades
      school_level_df <- school_level_df %>% 
        filter(has_elementary_grades == TRUE)
      
    } else if (input$grade_input_table == "Schools with middle school grades") {
      
      # Filter to schools with MS grades
      school_level_df <- school_level_df %>% 
        filter(has_middle_grades == TRUE)
      
    } else if (input$grade_input_table == "Schools with high school grades") {
      
      # Filter to schools with HS grades
      school_level_df <- school_level_df %>% 
        filter(has_high_grades == TRUE)
    }
    
    # Merge school_level_df with the time series index df, 
    # select only relevant columns 
    school_level_df <- school_level_df %>%
      select(`School Code`, `School Name`, LABEL, diff_22_avg1619) %>% 
      left_join((results_school_time_series %>%
                   filter(Subject == input$subject_input_table)),
                by = "School Code") %>%
      mutate(
        
        # Place holder for graph
        Trend = NA
      ) %>%
      relocate(Trend, .before = diff_22_avg1619)
    
    # Plot reactable
    reactable(
      school_level_df,
      columns = list(
        `School Code` = colDef(name = "School Code", align = "center"),
        `School Name` = colDef(name = "School Name", align = "left"),
        LABEL = colDef(name = "Ward", align = "center"),
        Percent = colDef(show = F),
        Subject = colDef(show = F),
        Trend = colDef(name = "2016-2023 Proficiency Trend",
                       align = "center",
                       cell = function(value, index) {
                         sparkline(school_level_df$Percent[[index]])
                        }
                       ),
        diff_22_avg1619 = colDef(name = "2022 Difference r.t. 2016-2019 Average",
                                 align = "center",
                                 format = colFormat(digits = 2))
      )
    )

  })
  
}

# -----------------------------------------------------------------------------#
# Run app ----
# -----------------------------------------------------------------------------#

shinyApp(ui = ui, server = server)

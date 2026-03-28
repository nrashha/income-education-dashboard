library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)
library(scales)
library(plotly)


# =============================================================================
# Loading Data
# =============================================================================

#' Load `characteristic`, `year`, `total`, `median_income`, `mean_income`, 
#' `standard_median_error`, `standard_mean_error`, and $5,000 buckets ranging
#' from `under_5_000` to `x200_000_and_over` tibble objects and the tibble 
#' objects between typically of the format `x###_###_to_###_###` with the 
#' hundred thousands and tens of thousands place not being specified if not 
#' relevant to that bucket.
load("data/income.RData")

# Load WIDE version of median and mean household income data
load("data/summary_wide.RData")

# Load LONG version of median and mean household income data
load("data/summary_long.RData")


# =============================================================================
# Creating global variables for button options
# =============================================================================

# Used for Median vs. Mean Tab 2
income_type_values <- c(
  "Median Household Income" = "median",
  "Mean Household Income" = "mean",
  "Show Both" = "both"
)

# Pull unique characteristic values and years from the dataframe for Tab 4
degree_choices <- unique(dataframe_hinc$characteristic)
year_choices <- sort(unique(dataframe_hinc$year))

# Used to identify unique education levels for Tab 3
education_choices <- c("All Education Levels", levels(degree_choices))


# =============================================================================
# User interface
# =============================================================================

ui <- fluidPage(
  theme = shinytheme("united"),
  # Overarching title for the app
  titlePanel("Changes in Income based on Education Level in the US"),
  
  # Database citation
  p(tags$div(
    HTML(paste("Data is sourced by ",
               tags$a(href = "https://www.census.gov/programs-surveys/cps.html",
                      "U.S. Census Bureau"),
               sep = ""))
  )),
  
  navbarPage(
    "Select a Tab:",
    
    # Tab 1 (INTRODUCTION PAGE): Maria 
    tabPanel(
      title = "Introduction",
      h1("Description"),
      
      p("On this app, you will find tabs which explore whether there
       is any association between the income and education level of household 
       owners in the United States. Through interactive visualizations, you can
       explore how different levels of educational attainment (from high school
       dropouts to professional holders) correlate with household income levels.
       The app presents demographic data, trends to help understand how 
       education might influence earning potential across American households.
      "),
      
      h1("The Dataset"),
      p("The Current Population Survey (CPS) is a monthly survey of U.S.
        households which tracks many features of the househould and householder.
        The householder is an adult reference point of the household 
        (not necessarily the 'head' of the household).
        "), 
      p(
        "Education level refers to the highest level of education that
        an individual has completed, which is distinct from the level of
        schooling that an individual is attending. Our dataset categorizes the
        educational level of household owners into 9 categories, which are
        as follows:"), 
      
      # List of educational levels
      tags$ul(
        tags$li(strong("<9th Grade:")," Individuals who have not completed any 
                formal education beyond elementary school"),
        tags$li(strong("9-12th (No Diploma):")," Individuals who attended 
                high school but did not graduate."),
        tags$li(strong("High School Graduate:") ," Individuals who have 
          completed high school and received a diploma."),
        tags$li(strong("Some College (No Degree):") ,"Individuals who have
          attended college but did not earn a degree."),
        tags$li(strong("Associate Degree:"), " Individuals who have completed 
          a two-year degree program at a community college or technical school
                ."),
        tags$li(strong("Bachelor's Degree:"), " Individuals who have completed
          a four-year undergraduate degree program."),
        tags$li(strong("Master's Degree:")," Individuals who have completed a
          graduate program beyond a bachelor's degree."),
        tags$li(strong("Professional Degree:"), " Individuals who have earned
          degrees in specific professions, such as law or medicine"),
        tags$li(strong("Doctorate Degree:"),  " Individuals who have completed 
          the highest level of academic degree, typically requiring original
                research")
      ),
      
      h1("Authors"),
      p("Deshan DeMel, Maria Khan, Nahian Rashha"),
      
    ),
    
    
    # Tab 2: Median vs. Mean Household Income (MARIA)
    tabPanel(
      title = "Household Income Comparison (2012-2023)",
      # Learned about helpText from:
      # https://shiny.posit.co/r/reference/shiny/0.11.1/helptext
      # Layout to choose income type
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "display_option",
            label = "Select Household Income Option:",
            choices = income_type_values,
            selected = "both"
          ),
          helpText("Tip: Select and highlight specific points on the plot to 
               display data for the selected range in a table below."),
          width = 3
        ),
        
        # Learned about brushOpts + usage from:
        # https://shiny.posit.co/r/reference/shiny/0.13.0/brushopts
        # and https://mastering-shiny.org/action-graphics.html
        # and https://shiny.posit.co/r/reference/shiny/0.14/plotoutput
        # Plot and table for mean vs. median
        mainPanel(
          plotOutput(
            outputId = "income_plot",
            brush = brushOpts(
              id = "plot_brush",
              resetOnNew = TRUE
            )
          ),
          dataTableOutput(outputId = "selected_points"),
          width = 9
        )
      )
    ),
    
    # Tab 3: Education (DESHAN)
    tabPanel(
      title = "Count of Householders by Education Level (2012-2023)",
      # Sidebar panel with drop-down menu of education levels
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "education",
                      label = "Education Level of Householder Count over 
                                 Time",
                      choices = education_choices),
          helpText("Tip: Hover over any point to check the year, # of 
                   householders of that education level, and education leve"),
          width = 3
        ),
        mainPanel(plotlyOutput(outputId = "education_level_lineplot"))
      )
    ),
    
    # Tab 4: Median Income by Educational Degree (RASHHA)
    tabPanel(
      title = "Median Income of Householder by Educational Degree (2012-2023)",
      
      sidebarLayout(
        sidebarPanel(
          
          # Using a conditional panel to show the degree drop-down menu iff. 
          # show_all_degrees is not selected
          # Same logic used for the year drop-down menu
          # Reference: 
          # https://shiny.posit.co/r/reference/shiny/1.5.0/conditionalpanel 
          conditionalPanel(
            condition = "!input.show_all_degrees",
            selectInput(inputId = "selected_degree",
                        label = "Select Educational Degree(s):",
                        choices = degree_choices,
                        multiple = TRUE,
                        selected = c("<9th Grade", "High School Graduate", 
                                     "Bachelor's Degree", "Master's Degree", "Doctorate Degree"))
          ),
          
          # Creating a checkbox to enable to user to select all degrees for display
          # Same logic used to allow user to select all years for display
          # Add help text for convenience of user
          # Reference: 
          # https://shiny.posit.co/r/reference/shiny/latest/checkboxinput
          # https://shiny.posit.co/r/reference/shiny/0.11.1/helptext
          checkboxInput(inputId = "show_all_degrees", 
                        label = "Show All Degrees", 
                        value = FALSE),
          helpText("Please choose any educational degrees you would like to explore from the above 
                   drop-down menu."),
          
          conditionalPanel(
            condition = "!input.show_all_years",
            selectInput(inputId = "selected_year",
                        label = "Select Year:",
                        choices = year_choices,
                        selected = "2022")
          ),
          
          checkboxInput(inputId = "show_all_years", 
                        label = "Show All Years", 
                        value = FALSE),
          helpText("Please choose any educational degrees you would like to explore from the above 
                   drop-down menu."),
          
          # Sliders for adjustable plot dimensions
          # Plot width is in percentage to allow the plot to scale for minimized windows 
          # Reference: 
          # https://shiny.posit.co/r/reference/shiny/0.14/sliderinput: 
          # https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny
          sliderInput(inputId = "plot_width", 
                      label = "Plot Width (%):", 
                      min = 50, 
                      max = 110, 
                      value = 110),
          sliderInput(inputId = "plot_height", 
                      label = "Plot Height (in pixels):", 
                      min = 400, 
                      max = 800, 
                      value = 550),
          
          width = 3
        ),
        
        # Use uiOutput() for the plot since its dimensions should be user-controlled
        # Reference: 
        # https://mastering-shiny.org/action-dynamic.html
        mainPanel(
          uiOutput(outputId = "dynamic_plot"),
          dataTableOutput(outputId = "income_table")
        )
      )
    )
  )
)
  

# =============================================================================
# Server
# =============================================================================
server <- function(input, output){
  
  
  # TAB 2: MEAN VS. MEDIAN HOUSEHOLD INCOME (MARIA)
  
  # reactive data filtering based on button input
  filtered_data <- reactive({
    # Filter for only both mean and median income types
    data <- summary2 |> 
      filter(income_type %in% c("mean_inc", "median_inc"))
    
    # Further filter based on user input (mean vs. median vs. both)
    # Referenced: https://stackoverflow.com/questions/45816510/use-
    # dplyr-conditional-filter-in-reactive-function-in-shiny
    if (input$display_option == "mean") {
      data |> filter(income_type == "mean_inc")
    } else if (input$display_option == "median") {
      data |> filter(income_type == "median_inc")
    } else {
      data  # Return both if option both is selected
    }
  })
  
  # plot for median and mean household incomes
  output$income_plot <- renderPlot({
    # Line graph for median and mean household income 
    ggplot(filtered_data(), aes(x = year, y = value,
                                color = income_type, 
                                group = income_type)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(
         values = c("mean_inc" = "#FF6B6B", "median_inc" = "#4ECDC4"),
         labels = c("mean_inc" = "Mean Income", "median_inc" = "Median Income")
       ) +
      labs(
        x = "Year",
        y = "Income",
        color = "Income Type",
        # change plot title based on user's selection
        # Referenced: https://dplyr.tidyverse.org/reference/case_when.html
        title = case_when(
          input$display_option == "both" ~ "Mean vs. Median Household Income in the US",
          input$display_option == "mean" ~ "Mean Household Income in the US",
          input$display_option == "median" ~ "Median Household Income in the US"
        )
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = if (input$display_option == "both") "bottom" 
                          else "none",
        panel.grid.major = element_line(color = "grey80", linetype = "dotted")
      ) +
      # Change y-axis scale to represent dollars 
      scale_y_continuous(labels = scales::dollar_format())
  })
  
  # Display detailed summary table information using dataTable() 
  # Referenced: https://shiny.posit.co/r/reference/shiny/0.14/plotoutput ex.
  # Referenced Prof. Bailey's demo project for dataTable usage
  output$selected_points <-
    renderDataTable({
      # Get the years using brushedPoints() and the filtered_data
      brushed_years <-
        brushedPoints(filtered_data(),
                      input$plot_brush,
                      xvar = "year",
                      yvar = "value")$year
  
      if (length(brushed_years) > 0) {
          summary_stats |>
          filter(year %in% brushed_years) |>
          datatable(
            colnames = c("Year",
                         "Mean Income ($)",
                         "Median Income ($)",
                         "Change in Mean ($)",
                         "Change in Median ($)"),
            rownames = FALSE
          )
      } else {
        NULL  # Return NULL if no points are selected using brush
      }
    })
  

  # TAB 3: EDUCATION (DESHAN)
  output$education_level_lineplot <- renderPlotly({
      ggplotly(ggplot({if (input$education != "All Education Levels")
        dataframe_hinc |>
          filter(characteristic == input$education) 
        else dataframe_hinc} |>
          rename("Year" = "year",
                 "Percentage" = "percentage",
                 "Characteristic" = "characteristic"),
        aes(x = Year,
            y = Percentage,
            group = Characteristic,
            color = Characteristic)) +
          geom_line() +
          geom_point() +
          {if (input$education != "All Education Levels")
            scale_color_manual(
              values = c("<9th Grade" = "#DB73FB",
                         "9-12th (No Diploma)" = "#07C2A0",
                         "High School Graduate" = "#609CFF",
                         "Associate Degree" = "#FF61C3",
                         "Some College (No Degree)" = "#02B9E3",
                         "Bachelor's Degree" = "#F8766D",
                         "Master's Degree" = "#93AA01",
                         "Professional Degree" = "#00BA39",
                         "Doctorate Degree" = "#D29200")
            )} +
          labs(y = str_glue("Percentage of {input$education}"),
               x = "Year",
               title = str_glue("Percentage of {input$education} Educated\nHouseholders from 2012 to 2023")) +
          theme_classic() +
          theme(text = element_text(size = 10),
                plot.title = element_text(hjust = 0.5, size = 15),
                axis.title.x = element_text(size = 10, face = "bold"),
                axis.title.y = element_text(size = 10, face = "bold"),
                # Added a lot of header margin due to clipping of text with plot
                plot.margin = margin(35, 15, 15, 15)) +
          guides(color = FALSE, group = FALSE),
        tooltip = c("year", "percentage", "characteristic"))
  })

  # TAB 4: Median Income by Degree (Rashha) 
  
  # Set-up reactive filtering of dataframe based on user input of years and degrees
  # Reference:
  # https://shiny.posit.co/r/getstarted/build-an-app/reactivity-essentials/reactive-elements.html
  # https://uomresearchit.github.io/RSE18-shiny-workshop/goingfurther/reactive/
  reactive_data <- reactive({
    if (input$show_all_years && input$show_all_degrees) {
      dataframe_hinc |>
        filter(characteristic %in% degree_choices)
    } else if (input$show_all_years) {
      dataframe_hinc |>
        filter(characteristic %in% input$selected_degree)
    } else if (input$show_all_degrees) {
      dataframe_hinc |>
        filter(year == input$selected_year, 
               characteristic %in% degree_choices)
    } else {
      dataframe_hinc |>
        filter(
          year == input$selected_year,
          characteristic %in% input$selected_degree
        )
    }
  })
  
  # Display plot using user input dimensions
  # Reference: 
  # https://shiny.posit.co/r/reference/shiny/1.7.2/renderui
  # https://stackoverflow.com/questions/48922595/selecting-dimensions-of-plot-in-shiny
  output$dynamic_plot <- renderUI({
    plotOutput("inc_plot", 
               width = paste0(input$plot_width, "%"), 
               height = paste0(input$plot_height, "px"))
  })
  
  # Create the Median Income vs. Degree plot
  
  # Conditional coloring of bars based on selection of show_all_years
  # If show_all_years checked, fill color by year
  # If unchecked, fill color by degree
  # Reference: 
  # https://haven.tidyverse.org/reference/as_factor.html
  output$inc_plot <- renderPlot({
    ggplot(reactive_data(), aes(x = characteristic, y = median_income, 
                                fill = if (input$show_all_years) as_factor(year) else characteristic)) +
      
      # Plot the bars and specify height and position for both cases
      # Reference: 
      # https://ggplot2.tidyverse.org/reference/geom_bar.html
      # https://ggplot2.tidyverse.org/reference/position_dodge.html
      # https://ggplot2.tidyverse.org/reference/position_stack.html
      geom_bar(stat = "identity", position = if (input$show_all_years) "dodge" else "stack") + 
      labs(
        title = if (input$show_all_years) "Median Income of Householder by Educational Degree (2012–2023)" 
        else paste("Median Income of Householder by Educational Degree in", input$selected_year),
        x = "Educational Degree",
        y = "Median Income in USD",
        fill = if (input$show_all_years) "Year" else "Educational Degree"
      ) +
      {if (!input$show_all_years) scale_fill_manual(values = c(
        "<9th Grade" = "#F8766D",
        "9-12th (No Diploma)" = "#D29200",
        "High School Graduate" = "#93AA01",
        "Some College (No Degree)" = "#00BA39",
        "Associate Degree" = "#07C2A0",
        "Bachelor's Degree" = "#02B9E3",
        "Master's Degree" = "#609CFF",
        "Professional Degree" = "#DB73FB",
        "Doctorate Degree" = "#FF61C3"
      ))} +
      scale_y_continuous(labels = label_comma()) + 
      theme_minimal() +
      theme(
        # Relative sizing of the plot title
        # Reference:
        # https://www.rdocumentation.org/packages/ggplot2/versions/2.1.0/topics/rel
        plot.title = element_text(size = rel(1.6), face = "bold", hjust = 0.5, color = "#654321"),
        axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 15)),
        axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 18)),
        axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold", color = "#654321"),  
        legend.text = element_text(size = 12, color = "#654321"),                    
        legend.background = element_rect(fill = "white", color = "#654321"),           
        legend.key = element_rect(fill = "white", color = NA)
      )
  })
  
  output$income_table <- renderDataTable({
    filter_data <- if (input$show_all_years && input$show_all_degrees) {
      dataframe_hinc |>
        filter(characteristic %in% degree_choices)
    } else if (input$show_all_years) {
      dataframe_hinc |>
        filter(characteristic %in% input$selected_degree)
    } else if (input$show_all_degrees) {
      dataframe_hinc |>
        filter(year == input$selected_year, 
               characteristic %in% degree_choices)
    } else {
      dataframe_hinc |>
        filter(
          year == input$selected_year,
          characteristic %in% input$selected_degree
        )
    }
    
    filter_data |>
      arrange(year, desc(median_income)) |>
      select(
        Year = year, 
        'Degree of Householder' = characteristic, 
        `Median Income, in Dollars` = median_income
      )
    
    # Set up custom pagelength and menu for the datatable 
    # Reference: 
    # https://clarewest.github.io/blog/post/making-tables-shiny/
  }, options = list(
    pageLength = 5,
    lengthMenu = c(5, 9, 18, 50, 100)
  ))
}


# =============================================================================
# Build app
# =============================================================================
shinyApp(ui = ui, server = server)
# This is a Shiny web application.
# Median Household Income vs. Highest Educational Attainment of Householder, by Year (2012-2023)
# Created by Nahian Rashha


# Load packages 
library(shiny)
library(tidyverse)
library(scales)

# Import data
load("data/income.RData")

# Pull unique characteristic values and years from the dataframe 
degree_choices <- unique(dataframe_hinc$characteristic)
year_choices <- sort(unique(dataframe_hinc$year))

############
#    ui    #
############
ui <- fluidPage(
  titlePanel("Median Household Income vs Educational Degree (2012-2023)"),
  
  tabPanel(
    title = "Median Income of Householder by Educational Degree (2012-2023)",
    
    sidebarLayout(
      sidebarPanel(
        
        # Using a conditional panel to show the degree drop-down menu iff. 
        # show_all_degrees is not selected
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
        # Add help text for convenience of user
        # Reference: 
        # https://shiny.posit.co/r/reference/shiny/latest/checkboxinput
        # https://shiny.posit.co/r/reference/shiny/0.11.1/helptext
        checkboxInput(inputId = "show_all_degrees", 
                      label = "Show All Degrees", 
                      value = FALSE),
        helpText("Please choose any educational degrees you would like to explore from the above 
                   drop-down menu."),
        
        # Using a conditional panel to show the degree drop-down menu iff. 
        # show_all_years is not selected
        conditionalPanel(
          condition = "!input.show_all_years",
          selectInput(inputId = "selected_year",
                      label = "Select Year:",
                      choices = year_choices,
                      selected = "2022")
        ),
        
        # Creating a checkbox to enable to user to select all years for display
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


############
# server   #
############
server <- function(input, output) {
  
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
  output$inc_plot <- renderPlot({
    # Conditional coloring of bars based on selection of show_all_years
    # If show_all_years checked, fill color by year
    # If unchecked, fill color by degree
    # Reference: 
    # https://haven.tidyverse.org/reference/as_factor.html
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

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

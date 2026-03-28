# =============================================================================
# This Shiny app explores household income
# Tab (Deshan): 
# =============================================================================

library(shiny)
library(ggplot2)
library(scales)
library(plotly)

#' Load `characteristic`, `year`, `total`, `median_income`, `mean_income`, 
#' `standard_median_error`, `standard_mean_error`, and $5,000 buckets ranging
#' from `under_5_000` to `x200_000_and_over` tibble objects and the tibble 
#' objects between typically of the format `x###_###_to_###_###` with the 
#' hundred thousands and tens of thousands place not being specified if not 
#' relevant to that bucket.
load("data/income.RData")

education_choices <- dataframe_hinc |>
    pull(characteristic) |>
    unique()


# education_choices <- dataframe_hinc |>
#   pull(characteristic) |>
#   unique()


# =============================================================================
# User interface
# =============================================================================

ui <- 
  navbarPage(
    title = "Households",
    
    tabPanel(
      title = "Level of Education over Time",
      # Sidebar panel with drop-down menu of education levels
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "education",
                      label = h2("Education Level of Householder Count over 
                                 Time"),
                      choices = education_choices),
          helpText("Tip: Hover over any point to check the education level,
                   number of people of that education level, and which year")
          ),
          mainPanel(plotlyOutput(outputId = "education_level_lineplot"))
      )
    )
  )
  
# =============================================================================
# Server
# =============================================================================
server <- function(input, output){
  
  output$education_level_lineplot <-
      renderPlotly({
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
       #' Manually colored by characteristic as dynamic implementation via
       #' color_label similar to y_label is not compatible with shinyApp.
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
              plot.margin = margin(55, 15, 15, 15)) +
        guides(color = FALSE, group = FALSE),
      tooltip = c("year", "percentage", "characteristic"))
      })
  
  #' output$education_level_lineplot <-
  #'   renderPlotly({
  #'     # Pulling respective label for variable using a dictionary
  #'     # y_label <- 
  #'     #   names(education_choices)[education_choices == input$education]
  #' 
  #'     #' Using ggplotly to create interactive plot.
  #'     #' I use conditional statements to construct the plot for
  #'     #' "All Education Levels" and individual Education levels
  #'     ggplotly(ggplot({if (input$education != "All Education Levels")
  #'       dataframe_hinc |> filter(characteristic == input$education)
  #'       else dataframe_hinc} |>
  #'         # Renamed to have user friendly tooltips upon mouse hovering over plot
  #'         rename("Year" = "year",
  #'                "Percentage" = "percentage",
  #'                "Characteristic" = "characteristic"),
  #'       aes(x = Year,
  #'           y = Percentage,
  #'           group = Characteristic,
  #'           color = Characteristic
  #'       )) +
  #'         geom_line() +
  #'         geom_point() +
  #'         scale_x_continuous(breaks = breaks_pretty()) +
  #'         #' Manually colored by characteristic as dynamic implementation via
  #'         #' color_label similar to y_label is not compatible with shinyApp.
  #'         {if (input$education != "All Education Levels")
  #'           scale_color_manual(
  #'             values = c("<9th Grade" = "#DB73FB",
  #'                        "9-12th (No Diploma)" = "#07C2A0",
  #'                        "High School Graduate" = "#609CFF",
  #'                        "Associate Degree" = "#FF61C3",
  #'                        "Some College (No Degree)" = "#02B9E3",
  #'                        "Bachelor's Degree" = "#F8766D",
  #'                        "Master's Degree" = "#93AA01",
  #'                        "Professional Degree" = "#00BA39",
  #'                        "Doctorate Degree" = "#D29200")
  #'           )} +
  #'         # Using `str_glue()` to have plot accurate titles
  #'         # labs(y = str_glue("Count of {y_label}"),
  #'         #      x = "Year",
  #'         #      title = str_glue("Count of {y_label}\nHouseholders from 2012 to 2023")) +
  #'         theme_classic() +
  #'         theme(text = element_text(size = 10),
  #'               plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
  #'               axis.title.x = element_text(size = 10, face = "bold"),
  #'               axis.title.y = element_text(size = 10, face = "bold"),
  #'               # Added a lot of header margin due to clipping of text with plot
  #'               plot.margin = margin(55, 15, 15, 15)) +
  #'         guides(color = FALSE, group = FALSE),
  #'       tooltip = c("year", "percentage", "characteristic"))
  #'   })
}

# =============================================================================
# Build app
# =============================================================================
shinyApp(ui = ui, server = server)
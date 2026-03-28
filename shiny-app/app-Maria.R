# =============================================================================
# This Shiny app explores median and mean household income over 10 years
# Tab (Maria): 
# =============================================================================


library(shiny)
library(tidyverse)
library(kableExtra)

load("data/summary_wide.RData")
load("data/summary_long.RData")

# vector for options button
income_type_values <- c(
  "Median Household Income" = "median",
  "Mean Household Income" = "mean",
  "Show Both" = "both"
)

# =============================================================================
# User interface
# =============================================================================
ui <- fluidPage(
  title = "Household Income Comparison (2012-2023)",
  
  # Layout to choose income type 
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "display_option",
        label = "Select Display Option:",
        choices = income_type_values,
        selected = "both"
      ),
      helpText("Tip: Use the brush tool to select and highlight specific points on the plot. 
               The table below will display data for the selected range."),
      width = 3
    ),
    
    # plot and table for mean vs. median
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
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output) {
  
  # reactive filtering based on button input
  filtered_data <- reactive({
    # Filter for only both mean and median income types
    data <- summary2 |> 
      filter(income_type %in% c("mean_inc", "median_inc"))
    
    # Further filter based on user input
    if (input$display_option == "mean") {
      data |> filter(income_type == "mean_inc")
    } else if (input$display_option == "median") {
      data |> filter(income_type == "median_inc")
    } else {
      data  # Return both if option both is selected
    }
  })
  
  # plot for median and mean household incomes
  output$income_plot <- 
    renderPlot({
    ggplot(filtered_data(), aes(x = year, y = value,
                                color = income_type, 
                                group = income_type)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(
        values = c("mean_inc" = "#FF6B6B", "median_inc" = "#4ECDC4"),
        labels = c("mean_inc" = "Mean Income", "median_inc" = "Median Income")
      ) +
      labs(
        x = "Year",
        y = "Income (Dollars)",
        color = "Income Type",
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
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = if (input$display_option == "both") "bottom" else "none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(15, 15, 15, 15)
      ) +
      scale_y_continuous(labels = scales::dollar_format())
  })
  
  output$selected_points <-
    renderDataTable({
      # Get the years from the brushed area using the filtered_data
      brushed_years <-
        brushedPoints(filtered_data(),
                      input$plot_brush,
                      xvar = "year",
                      yvar = "value")$year
      
      if (length(brushed_years) > 0) {
        summary_stats |>
          filter(year %in% brushed_years) |>
          select(
            year,
            median_inc,
            change_median_income,
            mean_inc,
            change_mean_income
          ) |>
          datatable(
            colnames = c("Year",
                         "Median Income ($)",
                         "Change in Median ($)",
                         "Mean Income ($)",
                         "Change in Mean ($)"),
            rownames = FALSE
          )
      } else {
        NULL  # Return NULL if no points are selected using brush
      }
    })
  
  
}

# =============================================================================
# Build app
# =============================================================================
shinyApp(ui = ui, server = server)

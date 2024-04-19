library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(plotly)

ui <- fluidPage(
  titlePanel("Car Analysis: Categorical Variables"),
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #89DDFF;",
      selectInput("variable", "Choose a variable:",
                  choices = c("Fuel Type", "Transmission", "Owner Type", "Brand", "Top Models", "Location")),
      pickerInput("colour", "Select colour:", 
                  choices = c("Blue" = "#0AE1FB", "Green" = "#0AFBBD", "Red", "Black", "Pink" = "#FAA4E8", "Orange" = "#F5B041"), 
                  selected = "#0AE1FB")  
    ),
    mainPanel(
      plotlyOutput("car_plot")
    )
  )
)

server <- function(input, output) {
  output$car_plot <- renderPlotly({
    variable <- switch(input$variable,
                       "Fuel Type" = "Fuel_Type",
                       "Transmission" = "Transmission",
                       "Owner Type" = "Owner_Type",
                       "Brand" = "Brand",
                       "Top Models" = "Model",  
                       "Location" = "Location")
    
    if (input$variable == "Top Models") {
      cars %>%
        count(Model) %>%
        arrange(desc(n)) %>%
        slice_head(n = 10) %>%
        ggplot(aes(x = Model, y = n)) +
        geom_col(fill = input$colour) +
        labs(title = "Count of cars per (top 10) model",
             x = "Models",
             y = "Count") +
        theme_light() +
        theme(plot.title = element_text(size = 16),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text.x = element_text(angle = 30, size = 10, vjust = 0.5))  # Customize for top models
    } else if (input$variable == "Brand") {
      cars %>%
        ggplot(aes_string(x = variable)) +
        geom_bar(fill = input$colour) +
        labs(title = paste("Count of cars per", input$variable),
             x = input$variable,
             y = "Count") +
        theme_light() +
        theme(plot.title = element_text(size = 16),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5))  # Customize for brand
    } else if (input$variable == "Location") {
      cars %>%
        ggplot(aes_string(x = variable)) +
        geom_bar(fill = input$colour) +
        labs(title = paste("Count of cars per", input$variable),
             x = input$variable,
             y = "Count") +
        theme_light() +
        theme(plot.title = element_text(size = 16),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text.x = element_text(angle = 30, size = 12, vjust = 0.5))  # Customize for location
    } else {
      # For other variables
      cars %>%
        ggplot(aes_string(x = variable)) +
        geom_bar(fill = input$colour) +
        labs(title = paste("Count of cars per", input$variable),
             x = input$variable,
             y = "Count") +
        theme_light() +
        theme(plot.title = element_text(size = 16),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text.x = element_text(size = 12))
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)

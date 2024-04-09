library(shiny)
library(DT)
library(shiny)

cars_data <- cars %>% 
  filter(Kilometers_Driven <= 4e+06) %>%
  mutate(Year = ym(str_c(Year, "-01"))) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(Ave_km_driven = mean(Kilometers_Driven, na.rm = T),
            Ave_mileage = mean(Mileage_value))

ui <- fluidPage(
  titlePanel("Car Trends in India"),
  sidebarLayout(
    sidebarPanel(
      selectInput("City", "Select City:",
                  choices = unique(cars$Location)),
      actionButton("update", "Update")
    ),
    mainPanel(
      plotOutput("avg_km_plot"),
      plotOutput("mileage_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive subset of data based on user input
  selected_data <- reactive({
    subset(cars_data, Location == input$City)
  })
  
  # Plot for average kilometers driven
  output$avg_km_plot <- renderPlot({
    ggplot(selected_data(), aes(x = Year, y = Ave_km_driven)) +
      geom_line() +
      labs(title = "Average Kilometers Driven per Year",
           x = "Year", y = "Average Kilometers Driven")
  })
  
  # Plot for mileage per year
  output$mileage_plot <- renderPlot({
    ggplot(selected_data(), aes(x = Year, y = Ave_mileage)) +
      geom_line() +
      labs(title = "Mileage per Year",
           x = "Year", y = "Mileage")
  })
}

#shinyApp(ui = ui, server = server)
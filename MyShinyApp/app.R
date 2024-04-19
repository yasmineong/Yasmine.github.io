library(shiny)
library(DT)
library(shiny)
library(tidyverse)
library(plotly)
library(ggthemes)

new_cars <- read.csv("used_cars_data.csv")
cars1 <- new_cars %>%
  separate(Mileage, into = c("Mileage_v", "Mileage_unit"), sep = " ")

cars2 <- cars1 %>%
  separate(Name, into = c("Brand", "Model"), sep = " ", extra = "merge") %>%
  mutate(Brand = case_when(
    Brand == "ISUZU" ~ "Isuzu",
    Brand == "Land" ~ "Land Rover",
    TRUE ~ Brand
  ))

cars <- cars2 %>%
  mutate("Engine/CC" = as.numeric(str_remove(Engine, "CC")),
         "Power/bhp" = as.numeric(str_remove(Power, "bhp")),
         "Mileage_value" = as.numeric(Mileage_v), 
         "Car_age" =  year(Sys.Date()) - Year
  ) %>%
  select(-c(Power, Engine, Mileage_v)) 

cars <- cars %>%
  select(Brand, Model, Location, Year, Car_age, Kilometers_Driven, Fuel_Type, Transmission, Owner_Type, Mileage_value, Mileage_unit, `Engine/CC`, `Power/bhp`, Seats, New_Price, Price)

cars_data <- cars %>% 
  mutate(Year = ym(str_c(Year, "-01"))) %>%
  na.omit() %>%
  group_by(Year) %>%
  mutate(Ave_km_driven = mean(Kilometers_Driven, na.rm = T),
         Ave_power = mean(`Power/bhp`))


ui <- fluidPage(
  navbarPage("Analysis of Kilometers Driven in different parts of India",
             tabPanel("Average Kilometers Driven",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("City", "Select City:",
                                      choices = unique(cars_data$Location)),
                          actionButton("update", "Update"),
                          style = "background-color:  #89DDFF; color: black;"
                        ),
                        mainPanel(
                          plotlyOutput("avg_km_plot")
                        )
                      )),
             tabPanel("Average Kilometers Driven by Brand",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("City_brand", "Select City:",
                                      choices = unique(cars_data$Location)),
                          actionButton("update_brand", "Update"),
                          style = "background-color:  #89DDFF; color: black;"
                        ),
                        mainPanel(
                          plotlyOutput("power_plot")
                        )
                      ))
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive subset of data based on user input
  selected_data <- reactive({
    subset(cars_data, Location == input$City)
  })
  
  selected_data_brand <- reactive({
    subset(cars_data, Location == input$City_brand)
  })
  
  # Plot for average kilometers driven
  output$avg_km_plot <- renderPlotly({
    p <- ggplot(selected_data(), aes(x = Year, y = Ave_km_driven)) +
      geom_line() +
      labs(x = "Year", y = "Average Kilometers Driven") +
      theme_light() +  
      theme(text = element_text(size = 12, family = "Arial")) +
      theme(panel.grid = element_blank())
    
    ggplotly(p)
  })
  
  # Plot for average kilometers driven by brand
  output$power_plot <- renderPlotly({
    p <- selected_data_brand() %>%
      group_by(Brand) %>%
      summarise(Avg_km_driven = mean(Kilometers_Driven, na.rm = T)) %>%
      ggplot(aes(x = Brand, y = Avg_km_driven, fill = Brand)) +
      geom_col() +
      labs(x = "Brand", y = "Average Kilometers Driven") +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            ) +
      guides(fill = "none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

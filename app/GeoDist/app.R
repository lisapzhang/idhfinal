#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)

#setwd("~/idhfinal/app")
statesjoin <- readRDS("statesjoin.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Geographical Distribution by Year"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "year",
                        label = "Select a year:",
                        choices = unique(statesjoin$Year))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput(outputId = "map",
                      width = "100%", 
                      height = "700")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selectyear <- reactive({
    z <- statesjoin |>
      filter(Year == input$year) 
    return(z)
  })

  output$map <- renderLeaflet({
    pal <- colorNumeric(palette = "RdPu", 10, domain = statesjoin$Number)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g students",
      selectyear()$NAME, selectyear()$Number) |>
      lapply(htmltools::HTML)
    
    selectyear() |>
      st_transform(crs = "+init=epsg:4326") |>
      leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(-100, 40, zoom = 4) |>
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(selectyear()$Number),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) 
  
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

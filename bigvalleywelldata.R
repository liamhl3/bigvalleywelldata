library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(plotly)
library(corrgram)
library(corrplot)
library(sp)
library(sf)
library(leaflet)
library(tidyverse)
library(stringr)
library(here)
library(widgetframe)
library(htmltools)
library(htmlwidgets)
library(tmap)
library(rgdal)
library(caret)
library(ggpubr)
library(car)
library(plyr)
library(PerformanceAnalytics)
library(ggcorrplot)
library(naniar)
library(grid)
library(mice)
library(egg)
library(shiny)
library(lubridate)
library(scales)
library(shinythemes)
library(rsconnect)


well.data.full <- read_csv("totalmerged.csv")

well_input <- read.csv("Well_Spatial.csv")
well_spatial <- st_as_sf(well_input, coords = c('X','Y'), crs = 4326)
well.project <- st_transform(well_spatial, "+proj=longlat +datum=WGS84")

bv.outline <- st_read("BV outline.shp")
bv.name <- read.csv("BV_Name.csv")
bv.merge <- merge(bv.outline, bv.name)

ui <- fluidPage(
  titlePanel("Big Valley Groundwater Basin Well Data"),
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("yeti"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'WellID', 
                  label = 'Select Well:',
                  choices = unique(well.data.full$WellID))),
    mainPanel(
      plotly::plotlyOutput('plot.well.data.full'),
      leafletOutput(outputId = "map"))
    
  )
)

server <- function(input, output, session){
  plot_trends <- function(){
    well.data.full %>%
      filter(WellID == input$WellID) %>%
      ggplot(aes(x = Date, y= Elevation, group = Type, color = Type))+
      scale_x_date(breaks = pretty_breaks(7))+
      geom_line()+
      xlab("Date")+
      ylab("Elevation (ft.)")
  }
  output$plot.well.data.full <- plotly::renderPlotly({
    plot_trends()
  })
  wells.map <- function(){
    well.project %>%
      filter(WellID == input$WellID)%>%
      leaflet()%>%
      addProviderTiles(providers$Esri.WorldImagery)%>%
      addMarkers(popup = ~WellID)%>%
      addPolygons(data = bv.merge,
                  popup = ~Name,
                  color = "#FCF300",
                  weight = 2,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.08)%>%
      setView(lng = -122.857141,
              lat = 38.974959,
              zoom = 12)
  }
  output$map <- renderLeaflet ({
    wells.map()
  })
}

shinyApp(ui = ui, server = server)

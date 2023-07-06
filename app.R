#load packages ---------------------------------------------------------------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly) 
library(readxl)
library(anytime)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(viridis)

#data loading and formatting--------------------------------------------------------------------------------------------------------------

setwd("C:/Users/anahi/OneDrive/Documents")

# Load and preprocess the data
BYS <- read.csv("GitHub/R-shiny/Data/Stream/BYS_LogLog_Q_GM.csv")
CLD <- read.csv("GitHub/R-shiny/Data/Stream/CLD_LogLog_Q.csv")
precip <- read_excel("GitHub/R-shiny/Data/Stream/BCC_all_precip.xlsx")
stat_location <- read.csv("GitHub/R-shiny/Data/Stream/CW3E_Stations_all.csv")

#format the dates from flow 
BYS$bys.dt2= as.POSIXct(BYS$bys.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#aggregate precip data to 15 min
#data$timestamp <- as.POSIXct(data$timestamp)

# Aggregate the data to 15-minute intervals
precip_data.15 <- precip %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, "15 mins")) %>%
  summarise(Rain_mm_Tot = sum(Rain_mm_Tot))

#merge precip with flow
merged_dataset <- merge(BYS, precip_data.15, by.x = "bys.dt2", by.y = "TIMESTAMP", all = TRUE)

date = merged_dataset$bys.dt2 #dates at daily format, however you can use any temporal resolution
flow = merged_dataset$bys.q4 # flow data
rainfall = merged_dataset$Rain_mm_Tot # rainfall data

CLD$cld.dt2
CLD$cld.q3

rainAx = list(
  overlaying = "y",
  side = "right",
  title = "Precipitation (mm)",
  #autorange="reversed",
  range = c(300,0),
  showgrid=FALSE)

#user interface--------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
  
  headerPanel(
    title=tags$a(href='https://cw3e.ucsd.edu/overview/',tags$img(src='logo.png', height = 80, width = 300), target="_blank"),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"))),
  
  titlePanel(strong("Streamflow Data Dashboard")),
 
  sidebarLayout(
    
    sidebarPanel(
      
      #widgets-----------------------------------------------------------------------------------------------------------------------
      
      selectInput(
        inputId="select_station",
        label="Select Station:",
        choices = stat_location$Name,
        selected="Boys Creek"),

      checkboxGroupInput(
        inputId = "var",
        label = "Select Variable(s):",
        choices = list("Streamflow", "Precipitation"),
        selected="Streamflow"),

      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range:",
        start = "2017-09-01",
        end = "2022-01-01",
        min = "2017-09-01",
        max = "2022-01-01"),
      
    br(),br(),br(),br(),
    
      selectInput(
        inputId = "dataset",
        label = "Choose Dataset to Download:",
        choices = stat_location$Name),
      
      downloadButton(
        outputId = "download_data",
        label = "Download CSV:"),
    
    br(), br(), br(), br(), br(),
      
      dateInput(
        inputId="choose_date",
        label="See Values for a Specific Date:",
        value="2014-01-01"),
    
      dataTableOutput(
        outputId="chosen_date")
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Hydrograph", plotlyOutput("graph"),height="80vh",plotlyOutput("var"),plotlyOutput("date_range")),
                  tabPanel("Station Map",leafletOutput(outputId = "map",height="60vh")),
                  tabPanel("Live Images", imageOutput(outputId = "recent_image")),
                  tabPanel("About", textOutput("info"),
                          h3("Station Names"),
                          h3("Data"),
                          p("talk about where data came from"))),
      
      imageOutput(outputId = "logo.png")
      
    )
  )
)

#cleaning up Site.Type in stat_location---------------------------------------------------------------------------

stat_location1 <- mutate(stat_location, Site.Type=as.character(Site.Type))

#server----------------------------------------------------------------------------------------------------------------------

server <- function(input,output,session){
  
#hydrograph--------------------------------------------------------------------------------------------------------------------  
  
  output$graph <- renderPlotly({
    req(input$select_station)
    plot_ly() %>%
        add_trace(
          x=~date, y=~flow,
          type="scatter", mode="lines", line = list
          (color = '#2fa839', width = 1, 
            dash = 'solid'),name = 'Streamflow') %>%        
      #add_trace(
        #x=CLD$cld.dt2, y=CLD$cld.q3,
        #type="scatter", mode="lines", line = list
        #(color = '#2fa839', width = 1, 
          #dash = 'solid'),name =input$select_station[2], "- Streamflow") %>%       
      add_trace(
        x=~date, y=~rainfall,
        type="bar", yaxis="y2", marker = list
        (color ="blue",width = 1),name = 'Precipitation - BCC') %>%    
      layout(#title = "Rainfall-Streamflow",
        xaxis =list
        (title = "time (daily)"), yaxis=list
        (title="Q  ft³/s",range=c(0,60)),yaxis2=rainAx)
  })
  
#map of stations-------------------------------------------------------------------------------------------------------------
  
  output$map <- renderLeaflet({
    leaflet(stat_location) %>%
      RdYlBu <- colorFactor("RdYlBu",domain=stat_location$Site.Type) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%                                      #addProviderTiles("Stamen.Terrain") %>%
      setView(map, lng = -119.7871, lat = 36.7378, zoom = 6)  %>%
      addMarkers(lng=~stat_location$Longitude, lat=~stat_location$Latitude,
                 color = ~RdYlBu(stat_location$Site.Type),
                 popup=paste(stat_location$Name, "<br>",
                              "CW3E Code:", stat_location$CW3E.Code, "<br>",
                              "Watershed:", stat_location$Watershed, "<br>",
                              "Elevation:", stat_location$Elevation..Approx..m.,"m", "<br>",
                              "(",stat_location$Latitude,stat_location$Longitude,")"
                              ))
  })

#download data------------------------------------------------------------------------------------------------------------------
    
  output$download_data <- downloadHandler({
    filename = function(){stringr::str_glue("GitHub/R-shiny/Data/Stream/BYS_LogLog_Q_GM.csv")}
  })

#choose data to output flow/precip/level--------------------------------------------------------------------------------------
  
  output$choose_date <- renderDataTable({
    req(input$choose_date)
    
  })
    
}

#run app----------------------------------------------------------------------------------------------------------------------

shinyApp(ui=ui,server=server)
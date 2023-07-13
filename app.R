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
library(DT)

#data loading and formatting--------------------------------------------------------------------------------------------------------------

setwd("C:/Users/anahi/OneDrive/Documents")

# Load and preprocess the data
BYS <- read.csv("GitHub/R-shiny/Data/Stream/BYS_LogLog_Q_GM.csv")
CLD <- read.csv("GitHub/R-shiny/Data/Stream/CLD_LogLog_Q.csv")
precip <- read_excel("GitHub/R-shiny/Data/Stream/BCC_all_precip.xlsx")
stat_location <- read.csv("GitHub/R-shiny/Data/Stream/CW3E_Stations_all.csv")
BYS_obs <- read.csv("GitHub/R-shiny/Data/Stream/BYS_stream_obs.csv")
CLD_obs <- read.csv("GitHub/R-shiny/Data/Stream/CLD_stream_obs.csv")

#change 'SMOIL' to 'Smoil' in stat_location
stat_location$Site.Type <- gsub("SMOIL", "Smoil", stat_location$Site.Type)

#format the dates from flow 
BYS$bys.dt2= as.POSIXct(BYS$bys.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
CLD$cld.dt2= as.POSIXct(CLD$cld.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

# Aggregate the data to 15-minute intervals
precip_data.15 <- precip %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, "15 mins")) %>%
  summarise(Rain_mm_Tot = sum(Rain_mm_Tot))

#merge precip with flow
merged_dataset <- merge(BYS, precip_data.15, by.x = "bys.dt2", by.y = "TIMESTAMP", all = TRUE)
merged_dataset1 <- merge(CLD,merged_dataset, by.x = "cld.dt2", by.y = "bys.dt2", all = TRUE)

date = merged_dataset1$cld.dt2 #dates at daily format
BYS = merged_dataset1$bys.q4 # flow data
CLD = merged_dataset1$cld.q3 
rainfall = merged_dataset1$Rain_mm_Tot # rainfall data
date1 = as.Date(merged_dataset1$cld.dt2)
BYSobs = BYS_obs$Q.cfs
CLDobs = CLD_obs$Q.cfs

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

  tabsetPanel(type = "tabs",
                  
    tabPanel("Hydrograph",
                          
      sidebarLayout(   
                        
        sidebarPanel(position = "left",
                           
                     selectizeInput(
                       inputId = "select_station",
                       label = "Select Station:",
                       choices = stat_location$CW3E.Code,
                       selected = "BYS"), br(),
                     
                     checkboxGroupInput(
                       inputId = "var",
                       label = "Select Variable(s):",
                       choices = list("Streamflow", "Level"),           #precip is fixed, so did not include 
                       selected="Streamflow"), br(),
                     
                     #sliderInput(
                     #  inputId = "date_range",
                      # label = "Select Date Range:",
                     #  min = as.Date("2017-09-01"),
                      # max = as.Date("2022-01-01"),
                       #value = c(as.Date("2017-09-01"), as.Date("2022-01-01")),
                       #timeFormat="%Y-%m-%d"), 
                     
                     sliderInput(
                       inputId = "date_range",
                       label = "Select Date Range:",
                       min = as.POSIXct("2017-09-09 12:30:00"),
                       max = as.POSIXct("2022-10-25 07:00:00"),
                       value=c(as.POSIXct("2017-09-09 12:30:00"),
                               as.POSIXct("2022-10-25 07:00:00")),
                       timeFormat="%Y-%m-%d %H:%M:%S"),
                     
                     br(),br(),br(),br(),
                     
                     selectInput(
                       inputId = "dataset",
                       label = "Choose Dataset to Download:",
                       choices = stat_location$CW3E.Code,
                       selected = "BYS"),
                     
                     downloadButton(
                       outputId = "download_data",
                       label = "Download CSV:")),
        
        mainPanel(position = "right",
                  plotlyOutput("graph"),
                  plotlyOutput("selected_var"),
                  plotlyOutput("selected_dates")
                  #imageOutput("recent_image")
        )
      )),                         
      
      tabPanel("Station Map",
          column(6, leafletOutput("map", height = "70vh")),
          column(6, dataTableOutput("data_table"))),
                  
      tabPanel("About", 
          textOutput("info"),  
          h4("Hydrographs:"),
          p("Present a time history of streamflow"),          
          h4("Data Sources:"),
          p("talk about where data came from, etc"),
          h4("Data Collection:"),
          p("what methods used to collect data"))),
      
      imageOutput(outputId = "logo.png")
      
    )

#server----------------------------------------------------------------------------------------------------------------------

server <- function(input,output,session){
  
  #hydrograph--------------------------------------------------------------------------------------------------------------------  
  
  y <- reactive({input$select_station})
  
  #x <- reactive({input$date_range})
  #reactiveDate <- reactive({date %>% filter(date>=input$date_range[1] & date<=input$date_range[2])})
  #add reactiveDate() in add_trace() if trying above function
  #error message: no applicable method for 'filter_' applied to an object of class "logical" since you cant filter POSIXct data i guess

  output$graph <- renderPlotly({
    
    filteredData <- subset(date, date >= input$date_range[1] & date <= input$date_range[2])
    #date1<-as.POSIXct(input$date_range, timeFormat="%Y-%m-%d %H:%M:%S")
    #subset_date <- subset(date1, date >= date1[1] & date <= date1[2])
    
    plot_ly() %>%
      add_trace(filteredData,
        #dplyr::filter(date >= input$date_range[1], date <= input$date_range[2]
        #x=~filter(between(date1, input$date_range[1], input$date_range[2])),
        #x=~subset(date1, date1 >= input$date_range[1], date1 <= input$date_range[2]),
        #x=~(date >= input$date_range[1], date1 <= input$date_range[2]),
        #x=~(input$date_range[1]:input$date_range[2]),
        x=~date,
        y=~get(input$select_station),                  
        type="scatter", mode="lines", line = list               
        (color = '#2fa839', width = 1, 
          dash = 'solid'),name = "Streamflow") %>%        
      
      add_trace(filteredData,
        x=~date,
        y=~rainfall,
        type="bar", yaxis="y2", marker = list
        (color ="blue",width = 1),name = 'Precipitation - BCC') %>%  
      
      layout(#title = "Rainfall-Streamflow",
        xaxis =list
        (title = "time (daily)"), yaxis=list
        (title="Q  ft³/s",range=c(0,60)),yaxis2=rainAx)
  })
  
  #map of stations-------------------------------------------------------------------------------------------------------------
  
  RdYlBu <- colorFactor("RdYlBu",domain=stat_location$Site.Type)
  
  output$map <- renderLeaflet({
    leaflet(stat_location) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%                          #addProviderTiles("Stamen.Terrain") %>%
      setView(map, lng = -119.7871, lat = 36.7378, zoom = 6)  %>%
      addCircleMarkers(lng=~stat_location$Longitude, lat=~stat_location$Latitude,
                 stroke = FALSE, fill=TRUE, fillOpacity=1,
                 color = ~RdYlBu(stat_location$Site.Type),
                 popup=paste(stat_location$Name, "<br>",
                             "CW3E Code:", stat_location$CW3E.Code, "<br>",
                             "Watershed:", stat_location$Watershed, "<br>",
                             "Elevation:", stat_location$Elevation..Approx..m.,"m", "<br>",
                             "(",stat_location$Latitude,stat_location$Longitude,")"
                 )) %>%
      addLegend("topleft", pal=RdYlBu, values=stat_location$Site.Type, title="Site Type",opacity=1)
  })
  
  #data table under station map-----------------------------------------------------------------------------------------------------

  output$data_table <- DT::renderDataTable({DT::datatable(stat_location, rownames=FALSE,
                                          colnames = c("Site Name","Watershed","CW3E Code",
                                                       "CDEC Code","CNRFC Code","Latitude",
                                                       "Longitude","Elevation(m)","Site Type"),
                                          list(lengthMenu = c(5,10,20,45), pageLength = 8))})

  #download data------------------------------------------------------------------------------------------------------------------

  data <- reactive({get(input$dataset)})
  
  output$download_data <- downloadHandler(
    filename = function(){paste("GitHub/R-shiny/Data/Stream/",input$dataset,"_LogLog_Q_GM.csv")},
    content=function(file){write.csv(data(),file)}
  )
  
  #image for selected station----------------------------------------------------------------------------------------------------------------
  
  #output$recent_image <- renderImage({
    #req(input$select_station)
      #})
  
}

#run app----------------------------------------------------------------------------------------------------------------------

shinyApp(ui=ui,server=server)
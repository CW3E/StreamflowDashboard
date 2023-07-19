#load packages----------------------------------------------------------------------------------------------------------------------------

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
library(data.table)

#data loading and formatting--------------------------------------------------------------------------------------------------------------

setwd("C:/Users/anahi/OneDrive/Documents")

#station location data from github
stat_location <- read.csv("GitHub/R-shiny/Data/Stream/CW3E_Stations_all.csv")
stat_location$Site.Type <- gsub("SMOIL", "Smoil", stat_location$Site.Type)

#github data
BYS_obs <- read.csv("GitHub/R-shiny/Data/Stream/BYS_stream_obs.csv")
#manuall <- BYS_obs$Q.cfs
#CLD_obs <- read.csv("GitHub/R-shiny/Data/Stream/CLD_stream_obs.csv")

#precipitation data
precip <- read_excel("GitHub/R-shiny/Data/Stream/BCC_all_precip.xlsx")
# Aggregate the data to 15-minute intervals
precip15 <- precip %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, "15 mins")) %>%
  summarise(Rain_mm_Tot = sum(Rain_mm_Tot))

#need to figure out how to clean up this data
#BCC <- read.csv("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/BCC/BoyesCreekCanyon_TwoMinWS.dat")
#BVS <- read.csv("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/BVS/BrownsValleySchool_TwoMin.dat")
#DRW <- read.table("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/DRW/Deerwood_TwoMinWS.dat")
#WDG <- read.table("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/WDG/WindyGap_TwoMinWS.dat",fill=TRUE)

rainfall = precip15$Rain_mm_Tot 

rainAx = list(overlaying = "y",side = "right",title = "Precipitation (mm)",range = c(300,0),showgrid=FALSE)

#streamflow data
BYS <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/BYS/Processed/BYS_LogLog_Q.csv")
CLD <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/CLD/Processed/CLD_LogLog_Q.csv")
MEW <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/MEW/Processed/MEW_LogLog_Q.csv")
MLL <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/MLL/Processed/MLL_LogLog_Q.csv")
PRY <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/PRY/Processed/PRY_LogLog_Q.csv")
WHT <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/WHT/Processed/WHT_LogLog_Q.csv")
#UDC <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/WHT/Processed/UDC_LogLog_Q.csv")
#SYR <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/WHT/Processed/SYR_LogLog_Q.csv")
#LDM <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/WHT/Processed/LDm_LogLog_Q.csv")
#SAP <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/SAP/Processed")

#format the dates from flow 
BYS$bys.dt2= as.POSIXct(BYS$bys.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
CLD$cld.dt2= as.POSIXct(CLD$cld.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MEW$mew.dt2= as.POSIXct(MEW$mew.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MLL$mill.dt2= as.POSIXct(MLL$mill.dt2,tz= "UTC",format= "%Y-%m-%d %H:%M:%S")
PRY$pry.dt2= as.POSIXct(PRY$pry.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
WHT$wht.dt2= as.POSIXct(WHT$wht.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#streamflow data    #this won't work as long as files are in separate folders, too slow
#flow_data = list.files(path = "//Skyriver/CW3E_data/CW3E_Streamflow_Archive/",pattern="*_LogLog_Q.csv",recursive=TRUE)
#flow = lapply(flow_data, read.csv)

setwd("Z:/")

#manual streamflow data
manual_data = list.files(path = "CW3E_Streamflow_Archive/Data/",pattern="*Manual_Q_R.xlsx")
#manual = lapply(manual_data,read.csv)

#stage data 
stage_data = list.files(path = "CW3E_Streamflow_Archive/Data/All_Barocorrected_Level/",pattern="*.csv")
#stage = lapply(stage_data,read.csv)

#stage[[1]]$TIMESTAMP.UTC=as.POSIXct(stage[[1]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[2]]$TIMESTAMP.UTC=as.POSIXct(stage[[2]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[3]]$TIMESTAMP.UTC=as.POSIXct(stage[[3]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[4]]$TIMESTAMP.UTC=as.POSIXct(stage[[4]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[5]]$TIMESTAMP.UTC=as.POSIXct(stage[[5]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[6]]$TIMESTAMP.UTC=as.POSIXct(stage[[6]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")

#date data
#date = BYS$bys.dt2

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
                                        choices = c("BYS","CLD","MEW","MLL","PRY","WHT","LDM","SYR","UDC"),
                                        selected = "BYS"), br(),
                                      
                                      checkboxGroupInput(
                                        inputId = "select_variable",
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
  #z <- reactive({input$select_variable})

  output$graph <- renderPlotly({
    
    filteredData <- subset(date, date >= input$date_range[1] & date <= input$date_range[2])
    
    plot_ly() %>%
      add_trace(filteredData,
                x=~date,
                y=~get(input$select_station),                  
                type="scatter", mode="lines", line = list               
                (color = '#2fa839', width = 1, 
                  dash = 'solid'),name = "Streamflow") %>%   
      
      #Manual streamflow data
      add_trace(
        x=~date,
        y=~manuall, type = "scatter", mode="markers",
        color='red',
        size=10,
        name = "Streamflow - Manual")  
      
      add_trace(filteredData,
                #choose precipitation station based on chosen stream station, nested ifelse statement
                #ifelse(~get(input$select_station)=="BYS",y="ATP",
                #      ifelse(~get(input$select_station)=="CLD"|y=="PRY"|y=="WHT"|y=="EFR",y="CPP",
                #            ifelse(~get(input$select_station)=="MEW",y="DPR",y="RVP"))),
                
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
  
  #data <- reactive({get(input$dataset)})
  
  #output$download_data <- downloadHandler(
  #  filename = function(){paste("GitHub/R-shiny/Data/Stream/",input$dataset,"_LogLog_Q_GM.csv")},
  #  content=function(file){write.csv(data(),file)}
  #)
  
  #image for selected station----------------------------------------------------------------------------------------------------------------
  
  #output$recent_image <- renderImage({
  #req(input$select_station)
  #})
  
}

#run app----------------------------------------------------------------------------------------------------------------------

shinyApp(ui=ui,server=server)

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

#station location data from github
stat_location <- read.csv("GitHub/R-shiny/Data/Stream/CW3E_Stations_all.csv")
stat_location$Site.Type <- gsub("SMOIL", "Smoil", stat_location$Site.Type)

#precipitation data from github
precip <- read_excel("GitHub/R-shiny/Data/Stream/BCC_all_precip.xlsx")
#Aggregate the data to 15-minute intervals
precip15 <- precip %>%group_by(TIMESTAMP=cut(TIMESTAMP,"15 mins"))%>%summarise(Rain_mm_Tot=sum(Rain_mm_Tot))

#precipitation data from skyriver
#BCC <- read.csv("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/BCC/BoyesCreekCanyon_TwoMinWS.dat")
#BVS <- read.csv("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/BVS/BrownsValleySchool_TwoMin.dat")
#DRW <- read.table("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/DRW/Deerwood_TwoMinWS.dat")
#WDG <- read.table("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/WDG/WindyGap_TwoMinWS.dat",fill=TRUE)

#streamflow data
BYS_Q <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/BYS/Processed/BYS_LogLog_Q.csv")
CLD_Q <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/CLD/Processed/CLD_LogLog_Q.csv")
MEW_Q <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/MEW/Processed/MEW_LogLog_Q.csv")
MLL_Q <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/MLL/Processed/MLL_LogLog_Q.csv")
PRY_Q <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/PRY/Processed/PRY_LogLog_Q.csv")
WHT_Q <- read.csv("//Skyriver/CW3E_data/CW3E_Streamflow_Archive/WHT/Processed/WHT_LogLog_Q.csv")

#format streamflow data timestamps
BYS_Q$bys.dt2= as.POSIXct(BYS_Q$bys.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
CLD_Q$cld.dt2= as.POSIXct(CLD_Q$cld.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MEW_Q$mew.dt2= as.POSIXct(MEW_Q$mew.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MLL_Q$mill.dt2=as.POSIXct(MLL_Q$mill.dt2,tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
PRY_Q$pry.dt2= as.POSIXct(PRY_Q$pry.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
WHT_Q$wht.dt2= as.POSIXct(WHT_Q$wht.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#manual streamflow data
BYS_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/BYS_Manual_Q_R.xlsx")
CLD_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/CLD_Manual_Q_R.xlsx")
MEW_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/MEW_Manual_Q_R.xlsx")
MLL_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/MLL_Manual_Q_R.xlsx")
PRY_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/PRY_Manual_Q_R.xlsx")
WHT_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/WHT_Manual_Q_R.xlsx")

#merge precipitation with streamflow (using CLD_QM as test for manual data)
merged <- merge(BYS_Q,precip15,by.x ="bys.dt2", by.y = "TIMESTAMP",all = TRUE)
merged <- merge(CLD_Q,merged, by.x = "cld.dt2", by.y = "bys.dt2", all = TRUE)
merged <- merge(MEW_Q,merged, by.x = "mew.dt2", by.y = "cld.dt2", all = TRUE)
merged <- merge(MLL_Q,merged, by.x = "mill.dt2",by.y = "mew.dt2", all = TRUE)
merged <- merge(PRY_Q,merged, by.x = "pry.dt2", by.y = "mill.dt2",all = TRUE)
merged <- merge(WHT_Q,merged, by.x = "wht.dt2", by.y = "pry.dt2", all = TRUE)
merged <- merge(CLD_QM,merged, by.x = "Time.UTC", by.y = "wht.dt2", all=TRUE)

#attempts to merge all streamflow data at once,error: 'join columns in x must be present in data'
#merge_Q <- list(BYS_Q,CLD_Q,MEW_Q,MLL_Q,PRY_Q,WHT_Q)
#merged_Q <- merge_Q %>% reduce(full_join, by=join_by("*dt2"))

CLD_M = merged$Q.cfs
BYS = merged$bys.q4 
CLD = merged$cld.q3 
MEW = merged$mew.q3
MLL = merged$mill.q3
PRY = merged$pry.q3
WHT = merged$wht.q3
date = merged$wht.dt2
rain = merged$Rain_mm_Tot 

#set right y axis for precipitation
rainAx = list(overlaying = "y",side = "right",title = "Precipitation (mm)",range = c(300,0),showgrid=FALSE)

#set wd to //Skyriver/CW3E_data/
setwd("Z:/")

#manual streamflow data, this used to work and then stopped working and i dont know why :(
#was trying this way to see if its faster but it doenst work, issue with the lapply function and reading path of file
#manual_data = list.files(path = "CW3E_Streamflow_Archive/Data/",pattern="*Manual_Q_R.xlsx")
#manual = lapply(manual_data,read_xlsx)

#stage data, same issue as above
#stage_data = list.files(path = "CW3E_Streamflow_Archive/Data/All_Barocorrected_Level/",pattern="*.csv")
#stage = lapply(stage_data,read.csv)

#stage[[1]]$TIMESTAMP.UTC=as.POSIXct(stage[[1]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[2]]$TIMESTAMP.UTC=as.POSIXct(stage[[2]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[3]]$TIMESTAMP.UTC=as.POSIXct(stage[[3]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[4]]$TIMESTAMP.UTC=as.POSIXct(stage[[4]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[5]]$TIMESTAMP.UTC=as.POSIXct(stage[[5]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")
#stage[[6]]$TIMESTAMP.UTC=as.POSIXct(stage[[6]]$TIMESTAMP.UTC,tz="UTC",format= "%Y-%m-%d %H:%M:%S")

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
                                      
                                      sliderInput(
                                        inputId = "date_range",
                                        label = "Select Date Range:",
                                        min = as.POSIXct("2015-09-09 12:30:00"),
                                        max = as.POSIXct("2023-10-25 07:00:00"),
                                        value=c(as.POSIXct("2015-09-09 12:30:00"),
                                                as.POSIXct("2023-10-25 07:00:00")),
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
 
  output$graph <- renderPlotly({
    
    #this is for date slider, doesn't really work but also doesn't create errors so just leaving it for now
    filteredData <- subset(date, date >= input$date_range[1] & date <= input$date_range[2])

    plot_ly() %>%
      
      #adding points for manual streamflow data, also attempted with add_trace below
      add_markers(
                  x=~date,
                  y=~CLD_M,
                  type="scatter",
                  marker = list
                  (color ="blue"),name="Manual Streamflow") %>%
      
      #add_trace(
      #           x=~date,
       #          y=~CLD_M,
        #         type="scatter",mode="markers",
         #        name="Manual Streamflow")%>%
      
      #adding lines for streamflow data
      add_trace(filteredData,
                x=~date,
                y=~get(input$select_station),                  
                type="scatter", mode="lines", line = list               
                (color='#2fa839',width=1,dash='solid'),name="Streamflow") %>%      

      #precipitation
      add_trace(filteredData,
                x=~date,
                y=~rain,
                type="bar", yaxis="y2", marker = list
                (color="blue",width=1),name='Precipitation') %>%  
      
      layout(
        xaxis =list
        (title = "Time (daily)"), yaxis=list
        (title="Q (ft³/s)",range=c(0,60)),yaxis2=rainAx)
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

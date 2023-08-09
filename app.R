#load packages ---------------------------------------------------------------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly) 
library(readxl)
library(shinythemes)
library(leaflet)
library(DT)
library(tidyverse)
library(anytime)
library(config)

#data loading and formatting--------------------------------------------------------------------------------------------------------------

#set environment and retrieve config file
Sys.setenv(R_CONFIG_ACTIVE = "anahita")
config <- config::get()                       
setwd(config$root_dir)

#station location data
stat_location <- read.csv(config$stat_location)
stat_location$Site.Type <- gsub("SMOIL", "Smoil", stat_location$Site.Type)
stat_location2 <- read.csv(config$stat_location2)

#precipitation data
BCC_P15 <- read.csv(paste(config$precip_data_path, "/BCC_precip15min.csv", sep = ""), header = TRUE)
BVS_P15 <- read.csv(paste(config$precip_data_path, "/BVS_precip15min.csv", sep = ""), header = TRUE)
DRW_P15 <- read.csv(paste(config$precip_data_path, "/DRW_precip15min.csv", sep = ""), header = TRUE)
WDG_P15 <- read.csv(paste(config$precip_data_path, "/WDG_precip15min.csv", sep = ""), header = TRUE)

#format precipitation data dates
BCC_P15$Date.Time = as.POSIXct(BCC_P15$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
BVS_P15$Date.Time = as.POSIXct(BVS_P15$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
DRW_P15$Date.Time = as.POSIXct(DRW_P15$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
WDG_P15$Date.Time = as.POSIXct(WDG_P15$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#stage data
BYS_Le <- read.csv(paste(config$stage_data_path,"BYS_barocorrected_level.csv", sep = ""), header = TRUE)
CLD_Le <- read.csv(paste(config$stage_data_path,"CLD_barocorrected_level.csv", sep = ""), header = TRUE)
MEW_Le <- read.csv(paste(config$stage_data_path,"MEW_barocorrected_level.csv", sep = ""), header = TRUE)
MLL_Le <- read.csv(paste(config$stage_data_path,"MLL_barocorrected_level.csv", sep = ""), header = TRUE)
PRY_Le <- read.csv(paste(config$stage_data_path,"PRY_barocorrected_level.csv", sep = ""), header = TRUE)
WHT_Le <- read.csv(paste(config$stage_data_path,"WHT_barocorrected_level.csv", sep = ""), header = TRUE)

#format stage data timestamps
BYS_Le$Date.Time = as.POSIXct(BYS_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
CLD_Le$Date.Time = as.POSIXct(CLD_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MEW_Le$Date.Time = as.POSIXct(MEW_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MLL_Le$Date.Time = as.POSIXct(MLL_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
PRY_Le$Date.Time = as.POSIXct(PRY_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
WHT_Le$Date.Time = as.POSIXct(WHT_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#clean up stage data
BYS_Le <- select(BYS_Le,-"X")
CLD_Le <- select(CLD_Le,-"X")
MEW_Le <- select(MEW_Le,-"X")
MLL_Le <- select(MLL_Le,-"X")
PRY_Le <- select(PRY_Le,-"X")
WHT_Le <- select(WHT_Le,-"X")

#streamflow data
BYS_Q <- read.csv(paste(config$processed_csv_path,"BYS/Processed/BYS_LogLog_Q.csv", sep = ""), header = TRUE)
CLD_Q <- read.csv(paste(config$processed_csv_path,"CLD/Processed/CLD_LogLog_Q.csv", sep = ""), header = TRUE)
MEW_Q <- read.csv(paste(config$processed_csv_path,"MEW/Processed/MEW_LogLog_Q.csv", sep = ""), header = TRUE)
MLL_Q <- read.csv(paste(config$processed_csv_path,"MLL/Processed/MLL_LogLog_Q.csv", sep = ""), header = TRUE)
PRY_Q <- read.csv(paste(config$processed_csv_path,"PRY/Processed/PRY_LogLog_Q.csv", sep = ""), header = TRUE)
WHT_Q <- read.csv(paste(config$processed_csv_path,"WHT/Processed/WHT_LogLog_Q.csv", sep = ""), header = TRUE)

#format streamflow data timestamps
BYS_Q$bys.dt2= as.POSIXct(BYS_Q$bys.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
CLD_Q$cld.dt2= as.POSIXct(CLD_Q$cld.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MEW_Q$mew.dt2= as.POSIXct(MEW_Q$mew.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MLL_Q$mill.dt2=as.POSIXct(MLL_Q$mill.dt2,tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
PRY_Q$pry.dt2= as.POSIXct(PRY_Q$pry.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
WHT_Q$wht.dt2= as.POSIXct(WHT_Q$wht.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#manual streamflow data
BYS_QM <- read_xlsx(paste(config$streamflow_data_path,"BYS_Manual_Q_R.xlsx",sep=""))     
CLD_QM <- read_xlsx(paste(config$streamflow_data_path,"CLD_Manual_Q_R.xlsx",sep=""))
MEW_QM <- read_xlsx(paste(config$streamflow_data_path,"MEW_Manual_Q_R.xlsx",sep=""))
MLL_QM <- read_xlsx(paste(config$streamflow_data_path,"MLL_Manual_Q_R.xlsx",sep=""))
PRY_QM <- read_xlsx(paste(config$streamflow_data_path,"PRY_Manual_Q_R.xlsx",sep=""))
WHT_QM <- read_xlsx(paste(config$streamflow_data_path,"WHT_Manual_Q_R.xlsx",sep=""))

#editing date format manual streamflow data
BYS_QM$Date.Time = as.POSIXct(BYS_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
BYS_QM <- select(BYS_QM,c("Date.Time","Q.cfs"))
BYS_QM <- rename(BYS_QM, Q.cfs.BYS = Q.cfs)
CLD_QM$Date.Time = as.POSIXct(CLD_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
CLD_QM <- select(CLD_QM,c("Date.Time","Q.cfs"))
CLD_QM <- rename(CLD_QM, Q.cfs.CLD = Q.cfs)
MEW_QM$Date.Time = as.POSIXct(MEW_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
MEW_QM <- select(MEW_QM,c("Date.Time","Q.cfs"))
MEW_QM <- rename(MEW_QM, Q.cfs.MEW = Q.cfs)
MLL_QM$Date.Time = as.POSIXct(MLL_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
MLL_QM <- select(MLL_QM,c("Date.Time","Q.cfs"))
MLL_QM <- rename(MLL_QM, Q.cfs.MLL = Q.cfs)
PRY_QM$Date.Time = as.POSIXct(PRY_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
PRY_QM <- select(PRY_QM,c("Date.Time","Q.cfs"))
PRY_QM <- rename(PRY_QM, Q.cfs.PRY = Q.cfs)
WHT_QM$Date.Time = as.POSIXct(WHT_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
WHT_QM <- select(WHT_QM,c("Date.Time","Q.cfs"))
WHT_QM <- rename(WHT_QM, Q.cfs.WHT = Q.cfs)

#merge precipitation with streamflow (using CLD_QM as test for manual data)
merged <- base::merge(BYS_Q,BCC_P15, by.x ="bys.dt2", by.y = "Date.Time",all = TRUE)
merged <- base::merge(CLD_Q, merged, by.x = "cld.dt2", by.y = "bys.dt2", all = TRUE)
merged <- base::merge(MEW_Q, merged, by.x = "mew.dt2", by.y = "cld.dt2", all = TRUE)
merged <- base::merge(MLL_Q, merged, by.x = "mill.dt2",by.y = "mew.dt2", all = TRUE)
merged <- base::merge(PRY_Q, merged, by.x = "pry.dt2", by.y = "mill.dt2",all = TRUE)
merged <- base::merge(WHT_Q, merged, by.x = "wht.dt2", by.y = "pry.dt2", all = TRUE)
merged <- base::merge(CLD_QM,merged, by.x = "Date.Time", by.y = "wht.dt2", all=TRUE)
merged <- base::merge(BYS_QM,merged, by = "Date.Time", all=TRUE)
merged <- base::merge(MEW_QM,merged, by = "Date.Time", all=TRUE)
merged <- base::merge(MLL_QM,merged, by = "Date.Time", all=TRUE)
merged <- base::merge(PRY_QM,merged, by = "Date.Time", all=TRUE)
merged <- base::merge(WHT_QM,merged, by = "Date.Time", all=TRUE)
merged <- base::merge(BYS_Le, merged, by = "Date.Time", all=TRUE)
merged <- base::merge(CLD_Le, merged, by = "Date.Time", all=TRUE)
merged <- base::merge(MEW_Le, merged, by = "Date.Time", all=TRUE)
merged <- base::merge(MLL_Le, merged, by = "Date.Time", all=TRUE)
merged <- base::merge(PRY_Le, merged, by = "Date.Time", all=TRUE)
merged <- base::merge(WHT_Le, merged, by = "Date.Time", all=TRUE)
merged <- base::merge(BVS_P15, merged, by = "Date.Time", all=TRUE)
merged <- base::merge(DRW_P15, merged, by = "Date.Time", all=TRUE)
merged <- base::merge(WDG_P15, merged, by = "Date.Time", all=TRUE)

BYS_M = merged$Q.cfs.BYS
CLD_M = merged$Q.cfs.CLD
MEW_M = merged$Q.cfs.MEW
MLL_M = merged$Q.cfs.MLL
PRY_M = merged$Q.cfs.PRY
WHT_M = merged$Q.cfs.WHT
BYS_L = merged$level.in.BYS
CLD_L = merged$level.in.CLD
MEW_L = merged$level.in.MEW
MLL_L = merged$level.in.MLL
PRY_L = merged$level.in.PRY
WHT_L = merged$level.in.WHT
BYS = merged$bys.q4 
CLD = merged$cld.q3 
MEW = merged$mew.q3
MLL = merged$mill.q3
PRY = merged$pry.q3
WHT = merged$wht.q3
date = merged$Date.Time
BCC = merged$rain_in_BCC
BVS = merged$rain_in_BVS
DRW = merged$rain_in_DRW
WDG = merged$rain_in_WDG

#set right y axis for precipitation
rainAx = list(overlaying="y",side="right",title="Precipitation (inches)",range=c(1,0),showgrid=FALSE)

#set left y axis for level
levelAx = list(side="left",title="Level (inches)",showgrid=FALSE)

#set left y axis for discharge
dischargeAx = list(side="left",title="Discharge (ft³/s)",showgrid=FALSE)

#user interface--------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  #set theme of app
  theme = shinytheme("sandstone"),
  
  #make header panel with CW3E logo linked to website
  headerPanel(
    title=tags$a(href='https://cw3e.ucsd.edu/overview/',tags$img(src='logo.png', height = 80, width = 300), target="_blank"),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"))),
  
  titlePanel(strong("Streamflow Dashboard")),
  
  #creating 3 tabs for dashboard
  tabsetPanel(type = "tabs",
              
              tabPanel("Hydrograph",
                       
                       sidebarLayout(   
                         
                         sidebarPanel(position = "left",
                                      
                                      selectizeInput(
                                        inputId = "select_station",
                                        label = "Select Station:",
                                        choices = c("BYS","CLD","MEW","MLL","PRY","WHT"),
                                        selected = "CLD"), 
                                      
                                      selectizeInput(
                                        inputId = "var",
                                        label = "Select Variable:",
                                        choices = list("Discharge","Level"), 
                                        selected="Discharge"), 
                                      
                                      p(strong("Notes on Manual Discharge:")),
                                      p("To add or remove manual discharge points from the hydrograph, click on 
                                        'Manual Discharge' in the legend located in the top right corner of the hydrograph."),
                                      
                                      p(strong("Notes on Precipitation:")),
                                      p("To add or remove precipitation from the hydrograph, click on 'Precipitation' in
                                         the legend located in the top right corner of the hydrograph."),
                                      p("*Please note that precipitation data will be taken from the closest available 
                                         surface meteorology station, which is different from the streamflow stations."),
                                      br(),
                                      
                                      sliderInput(
                                        inputId = "date_range",
                                        label = "Select Date Range:",
                                        min = as.POSIXct("2016-09-09 00:00:00"),
                                        max = as.POSIXct("2022-10-25 12:30:00"),
                                        value=c(as.POSIXct("2016-09-09 00:00:00"),
                                                as.POSIXct("2022-10-25 12:30:00")),
                                        timeFormat="%Y-%m-%d %H:%M:%S")),
                         
                         mainPanel(position = "right",
                                   plotlyOutput("graph"),
                                   br(),br(),
                                   dataTableOutput("data_table2"),
                                   plotlyOutput("selected_var"),
                                   plotlyOutput("selected_dates")
                         )
                       )),                         
              
              tabPanel("Location Map",
                       column(6, leafletOutput("map", height = "70vh")),
                       column(6, dataTableOutput("data_table"))),
              
              tabPanel("About", 
                       textOutput("info"),  
                       h3("Introduction:"),
                       p("This application displays streamflow and precipitation data gathered by CW3E. Source code is available at the",a("CW3E Streamflow Dashboard GitHub Repository",href="https://github.com/anahitajensen/CW3E.StreamflowDashBoard",".")),
                       h3("Hydrograph:"),
                       p("A hydrograph is a chart showing, most often, river stage (height of the water above an arbitrary altitude) and streamflow (amount of water, usually in cubic feet per second). Other properties, such as rainfall can also be plotted. This application gives the user the ability to choose either discharge or level to plot, with the option of adding precipitation to either plot (USGS)."),          
                       h3("Data Collection:"),
                       h4(em("Discharge (Flow):")),
                       p("Discharge data on this page is measured through multiple methods that will be described in detail below."),
                       p("When using a",strong("current meter"),", a stream channel cross section is first divided into numerous vertical subsections. In each subsection, the area is obtained by measuring the width and depth of the subsection, and the water velocity is determined using a current meter. The discharge in each subsection is computed by multiplying the subsection area by the measured velocity. The total discharge is then computed by summing the discharge of each subsection (USGS)."),
                       p("When using",strong("ADCP (Acoustic Doppler Current Profiler)"),", the Doppler Effect is used to determine water velocity by sending a sound pulse into the water and measuring the change in frequency of that sound pulse reflected back to the ADCP by sediment or other particulates being transported in the water. The ADCP also uses acoustics to measure water depth by measuring the travel time of a pulse of sound to reach the river bottom at back to the ADCP. To make a discharge measurement, the ADCP is mounted onto a boat or into a small watercraft with its acoustic beams directed into the water from the water surface. The ADCP is then guided across the surface of the river to obtain measurements of velocity and depth across the channel. The river-bottom tracking capability of the ADCP acoustic beams or a Global Positioning System (GPS) is used to track the progress of the ADCP across the channel and provide channel-width measurements. Using the depth and width measurements for calculating the area and the velocity measurements, the discharge is computed by the ADCP using discharge = area x velocity, similar to the conventional current-meter method (USGS)."),
                       p(strong("Computer Vision Stream Gaging (CVSG)")," captures stereo camera footage of the water surface, which is analyzed to estimate water level, surface velocities, and gauged discharge.  The system utilizes a cloud architecture that allows for remote management of device configurations, automated data processing, and integration of internal and external data sources (Hutley et al., 2022).CVSG is new technology developed by Xylem Inc., and is being used at CW3E's Santa Ana River site below Prado Dam (SAP)."),
                       h4(em("Level (Stage):")),
                       p("Stage is measured by CW3E using Solinst leveloggers and barologgers. Leveloggers measure absolute pressure (water pressure + atmospheric pressure). Barologgers record atmospheric pressure. The most accurate method of obtaining changes in water level is to compensate for atmospheric pressure fluctuations using a Barologger 5, avoiding time lag in the compensation. The Barologger 5 is set above high water level in one location on site, while the Levelogger is set in the water (Solinst)."),
                       h4(em("Precipitation:")),
                       p("CW3E’s surface meteorological stations measure precipitation, among other variables. Data is collected every two minutes."),
                       h3("Current Status:"),
                       p("The app is currently being developed. Data is raw and still being processed."),
                       br(),
                       h3("References:"),
                       p(a("USGS: How Streamflow is Measured",href="https://www.usgs.gov/special-topics/water-science-school/science/how-streamflow-measured")),
                       p(a("USGS: Streamflow and the Water Cycle",href="https://www.usgs.gov/special-topics/water-science-school/science/streamflow-and-water-cycle")),
                       p(a("CW3E: Surface Meteorology Observations",href="https://cw3e.ucsd.edu/cw3e-surface-meteorology-observations/")),
                       p(a("Solinst Barometric Compensation",href="https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/levelogger/datasheet/barometric-compensation.php")),
                       p(a("Hutley, N. R., Beecroft, R., Wagenaar, D., Soutar, J., Edwards, B., Deering, N., Grinham, A., and Albert, S.: Adaptively monitoring streamflow using a stereo computer vision system, EGUsphere [preprint], 2022.",href="https://doi.org/10.5194/egusphere-2022-735"))
              )),
  
  #this is to load CW3E logo
  imageOutput(outputId = "logo.png")
  
)

#server----------------------------------------------------------------------------------------------------------------------

server <- function(input,output,session){
  
  #hydrograph--------------------------------------------------------------------------------------------------------------------
  manual <- reactive({paste0(input$select_station,"_M")})
  y <- reactive({input$select_station})
  level <- reactive({paste0(input$select_station,"_L")})
  
  output$graph <- renderPlotly({
    
    req(input$var,input$date_range)
    
    filteredData <- subset(merged, Date.Time >= input$date_range[1] & Date.Time <= input$date_range[2])
    
    p <- plot_ly()
    
    if (input$var == "Discharge") {
      
      # Add points for manual discharge data
      p <- add_trace(p,
                     data = filteredData,
                     x = ~date,
                     y = ~base::get(manual()),
                     type = "scatter",
                     mode = "markers",
                     marker = list(color = "darkgreen"),
                     name = "Manual Discharge")
      
      ;
      
      # Add lines for discharge data
      p <- add_trace(p,
                     data = filteredData,
                     x = ~date,
                     y = ~base::get(y()),
                     type = "scatter",
                     mode = "lines",
                     line = list(color = '#2fa819', width = 1, dash = 'solid'),
                     name = "Discharge")
    } else {
      
      # Add lines for level data
      p <- add_trace(p,
                     data = filteredData,
                     x = ~date,
                     y = ~base::get(level()),
                     type = "scatter",
                     mode = "lines",
                     line = list(color = 'red', width = 1, dash = 'solid'),
                     name = "Level")
    }
    
    #add bars for precipitation
    p <- add_bars(p,
                  data = filteredData,
                  x = ~date,
                   y =  if (input$select_station %in% c("BYS", "WHT")) BCC else
                        if (input$select_station %in% c("CLD", "PRY", "MLL")) DRW else
                        if (input$select_station == "MEW") WDG,
                  yaxis = "y2",
                  marker = list(color = "blue", width = 1),
                  name = 'Precipitation')
    
    #if else statement to switch between discharge and level y-axis, x-axis and y-axis2 (right side) stay the same
    p <- layout(p,
                xaxis = list(title = "Time (15 minute intervals)"),
                yaxis = if (input$var == "Discharge" | input$var == "Manual Discharge") {dischargeAx} else {levelAx},
                yaxis2 = rainAx)
    
    return(p)
    
  })
  
  #map of stations-------------------------------------------------------------------------------------------------------------
  
  #color palette for the points
  RdYlBu <- colorFactor("RdYlBu",domain=stat_location$Site.Type)
  
  output$map <- renderLeaflet({
    leaflet(stat_location) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%                         
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
  
  #data table under hydrograph tab-----------------------------------------------------------------------------------------------------
  
  output$data_table2 <- DT::renderDataTable({DT::datatable(stat_location2, rownames=FALSE,
                                                           colnames = c("Site Name","Watershed","CW3E Code"),
                                                           list(lengthMenu = c(5,10,14), pageLength = 5))})
  
}

#run app----------------------------------------------------------------------------------------------------------------------

shinyApp(ui=ui,server=server)
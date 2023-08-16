#load packages---------------------------------------------------------------------------------------------------------------------------

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

#set environment and retrieve config file, change "anahita" to your environment
Sys.setenv(R_CONFIG_ACTIVE = "anahita")
config <- config::get()                       
setwd(config$root_dir)

#station location data for data table on 'station location' tab, has CW3E stations and their coordinates
stat_location <- read.csv(config$stat_location)
stat_location$Site.Type <- gsub("Smoil", "SMOIL", stat_location$Site.Type)
#currently only data from stream, precip met, and smoil stations is being used in dashboard so only kept those
stat_location <- stat_location[is.element(stat_location$Site.Type, c("Stream","Precip Met","SMOIL")),]
#station location data for data table on 'hydrograph' tab, only has name/watershed/site.type
stat_location2 <- read.csv(config$stat_location2)

#precipitation data, preprocessed from Skyriver (aggregated to 15min from 2min)
BCC_P15 <- read.csv(paste(config$precip_data_path, "/BCC_precip15min.csv", sep = ""), header = TRUE)
BVS_P15 <- read.csv(paste(config$precip_data_path, "/BVS_precip15min.csv", sep = ""), header = TRUE)
DRW_P15 <- read.csv(paste(config$precip_data_path, "/DRW_precip15min.csv", sep = ""), header = TRUE)
WDG_P15 <- read.csv(paste(config$precip_data_path, "/WDG_precip15min.csv", sep = ""), header = TRUE)

#getting rid of bad values - not sure what happened w/ data but it just alternated between 3 and 4 inches for a couple thousand timestamps
DRW_P15 <- DRW_P15[-(1:2502),]

#format precipitation data timestamps
BCC_P15$Date.Time = as.POSIXct(BCC_P15$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
BVS_P15$Date.Time = as.POSIXct(BVS_P15$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
DRW_P15$Date.Time = as.POSIXct(DRW_P15$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
WDG_P15$Date.Time = as.POSIXct(WDG_P15$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#if hourly instead of 15 minutes
BCC_hourly <- BCC_P15 %>%  group_by(Date.Time = cut(Date.Time, "60 mins")) %>%  summarise(rain_in_BCC = sum(rain_in_BCC))
BVS_hourly <- BVS_P15 %>%  group_by(Date.Time = cut(Date.Time, "60 mins")) %>%  summarise(rain_in_BVS = sum(rain_in_BVS))
DRW_hourly <- DRW_P15 %>%  group_by(Date.Time = cut(Date.Time, "60 mins")) %>%  summarise(rain_in_DRW = sum(rain_in_DRW))
WDG_hourly <- WDG_P15 %>%  group_by(Date.Time = cut(Date.Time, "60 mins")) %>%  summarise(rain_in_WDG = sum(rain_in_WDG))
BCC_hourly$Date.Time = as.POSIXct(BCC_hourly$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
BVS_hourly$Date.Time = as.POSIXct(BVS_hourly$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
DRW_hourly$Date.Time = as.POSIXct(DRW_hourly$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
WDG_hourly$Date.Time = as.POSIXct(WDG_hourly$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#stage data
BYS_Le <- read.csv(paste(config$stage_data_path,"BYS_barocorrected_level.csv", sep = ""), header = TRUE)
CLD_Le <- read.csv(paste(config$stage_data_path,"CLD_barocorrected_level.csv", sep = ""), header = TRUE)
MEW_Le <- read.csv(paste(config$stage_data_path,"MEW_barocorrected_level.csv", sep = ""), header = TRUE)
MLL_Le <- read.csv(paste(config$stage_data_path,"MLL_barocorrected_level.csv", sep = ""), header = TRUE)
PRY_Le <- read.csv(paste(config$stage_data_path,"PRY_barocorrected_level.csv", sep = ""), header = TRUE)
WHT_Le <- read.csv(paste(config$stage_data_path,"WHT_barocorrected_level.csv", sep = ""), header = TRUE)

#format stage data timestamps
BYS_Le$Date.Time = as.POSIXct(BYS_Le$Date.Time, tz= "UTC", format= "%m/%d/%Y %H:%M")
CLD_Le$Date.Time = as.POSIXct(CLD_Le$Date.Time, tz= "UTC", format= "%m/%d/%Y %H:%M")
MEW_Le$Date.Time = as.POSIXct(MEW_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MLL_Le$Date.Time = as.POSIXct(MLL_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
PRY_Le$Date.Time = as.POSIXct(PRY_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
WHT_Le$Date.Time = as.POSIXct(WHT_Le$Date.Time, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#streamflow data
BYS_Q <- read.csv(paste(config$streamflow_data_path,"BYS/Processed/BYS_LogLog_Q.csv", sep = ""), header = TRUE)
BYS_Q <- rename(BYS_Q, Q.cfs = bys.q4, Date.Time = bys.dt2)
CLD_Q <- read.csv(paste(config$streamflow_data_path,"CLD/Processed/CLD_LogLog_Q.csv", sep = ""), header = TRUE)
CLD_Q <- rename(CLD_Q, Q.cfs = cld.q3, Date.Time = cld.dt2)
MEW_Q <- read.csv(paste(config$streamflow_data_path,"MEW/Processed/MEW_LogLog_Q.csv", sep = ""), header = TRUE)
MEW_Q <- rename(MEW_Q, Q.cfs = mew.q3, Date.Time = mew.dt2)
MLL_Q <- read.csv(paste(config$streamflow_data_path,"MLL/Processed/MLL_LogLog_Q.csv", sep = ""), header = TRUE)
MLL_Q <- rename(MLL_Q, Q.cfs = mill.q3, Date.Time = mill.dt2)
PRY_Q <- read.csv(paste(config$streamflow_data_path,"PRY/Processed/PRY_LogLog_Q.csv", sep = ""), header = TRUE)
PRY_Q <- rename(PRY_Q, Q.cfs = pry.q3, Date.Time = pry.dt2)
WHT_Q <- read.csv(paste(config$streamflow_data_path,"WHT/Processed/WHT_LogLog_Q.csv", sep = ""), header = TRUE)
WHT_Q <- rename(WHT_Q, Q.cfs = wht.q3, Date.Time = wht.dt2)

#format streamflow data timestamps
BYS_Q$Date.Time = as.POSIXct(BYS_Q$Date.Time, tz="UTC", format= "%Y-%m-%d %H:%M:%S")
CLD_Q$Date.Time = as.POSIXct(CLD_Q$Date.Time, tz="UTC", format= "%Y-%m-%d %H:%M:%S")
MEW_Q$Date.Time = as.POSIXct(MEW_Q$Date.Time, tz="UTC", format= "%Y-%m-%d %H:%M:%S")
MLL_Q$Date.Time = as.POSIXct(MLL_Q$Date.Time, tz="UTC", format= "%Y-%m-%d %H:%M:%S")
PRY_Q$Date.Time = as.POSIXct(PRY_Q$Date.Time, tz="UTC", format= "%Y-%m-%d %H:%M:%S")
WHT_Q$Date.Time = as.POSIXct(WHT_Q$Date.Time, tz="UTC", format= "%Y-%m-%d %H:%M:%S")

#manual streamflow data
BYS_QM <- read_xlsx(paste(config$manual_streamflow_data_path,"BYS_Manual_Q_R.xlsx",sep=""))     
CLD_QM <- read_xlsx(paste(config$manual_streamflow_data_path,"CLD_Manual_Q_R.xlsx",sep=""))
MEW_QM <- read_xlsx(paste(config$manual_streamflow_data_path,"MEW_Manual_Q_R.xlsx",sep=""))
MLL_QM <- read_xlsx(paste(config$manual_streamflow_data_path,"MLL_Manual_Q_R.xlsx",sep=""))
PRY_QM <- read_xlsx(paste(config$manual_streamflow_data_path,"PRY_Manual_Q_R.xlsx",sep=""))
WHT_QM <- read_xlsx(paste(config$manual_streamflow_data_path,"WHT_Manual_Q_R.xlsx",sep=""))

#format manual streamflow data timestamps
BYS_QM$Date.Time = as.POSIXct(BYS_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
CLD_QM$Date.Time = as.POSIXct(CLD_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
MEW_QM$Date.Time = as.POSIXct(MEW_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
MLL_QM$Date.Time = as.POSIXct(MLL_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
PRY_QM$Date.Time = as.POSIXct(PRY_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")
WHT_QM$Date.Time = as.POSIXct(WHT_QM$Date.Time, tz="UTC",format= "%m/%d/%y %H:%M:%S")

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
  
  #make header panel with CW3E logo linked to CW3E website
  headerPanel(
    title=tags$a(href='https://cw3e.ucsd.edu/overview/',tags$img(src='logo.png', height = 80, width = 300), target="_blank"),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"))),
  
  titlePanel(strong("Streamflow Dashboard")),
  
  #creating 3 tabs for dashboard
  tabsetPanel(type = "tabs",
              
              #first tab
              tabPanel("Hydrograph",
                       
                       sidebarLayout(   
                         
                         #this is for the user inputs on the left side of the dashboard
                         sidebarPanel(position = "left",
                                      
                                      #creates option for user to select the station
                                      selectizeInput(
                                        inputId = "select_station",     #user does not see this, need this to reference in server
                                        label = "Select Station:",      #user sees this
                                        choices = c("BYS","CLD","MEW","MLL","PRY","WHT"),       
                                        selected = "CLD"),              #the station that shows up when dashboard first loads
                                      
                                      #creates option for user to select the variable 
                                      selectizeInput(
                                        inputId = "var",
                                        label = "Select Variable:",
                                        choices = list("Discharge","Level"), 
                                        selected = "Discharge"), 
                                      
                                      p(strong("Notes on Manual Discharge:")),
                                      p("To add or remove manual discharge points from the hydrograph, click on 
                                        'Manual Discharge' in the legend located in the top right corner of the hydrograph."),
                                      
                                      p(strong("Notes on Precipitation:")),
                                      p("To add or remove precipitation from the hydrograph, click on 'Precipitation' in
                                         the legend located in the top right corner of the hydrograph."),
                                      p("*Please note that precipitation data will be taken from the closest available 
                                         surface meteorology station, which is different from the streamflow stations."),
                                      br(), 
                                      
                                      #creates option for user to select which date range they want for hydrograph, does not work right now
                                      dateRangeInput(
                                        inputId = "date_range",
                                        label = "Select Date Range:",
                                        start = as.POSIXct("2018-01-01"),      
                                        end = as.POSIXct("2022-10-25"),
                                        min = as.POSIXct("2018-01-01"),      
                                        max = as.POSIXct("2022-10-25"),
                                       )),
                         
                         #this is what appears on the right side of the 'Hydrograph' tab, so it's the hydrograph, data table, and map
                         mainPanel(position = "right",
                                   plotlyOutput("graph"),
                                   br(),br(),
                                   column(6,leafletOutput("map2")),
                                   column(6,dataTableOutput("data_table2")),
                                   plotlyOutput("selected_var"),
                                   plotlyOutput("selected_dates")
                         )
                       )),                         
              
              #second tab with map and data table
              tabPanel("Location Map",
                       column(6, leafletOutput("map", height = "70vh")),
                       column(6, dataTableOutput("data_table"))),
              
              #third tab with about section
              tabPanel("About", 
                       textOutput("info"),  
                       h3("Introduction:"),
                       p("This application displays streamflow and precipitation data gathered by CW3E. Source code is available at the",a("CW3E Streamflow Dashboard GitHub Repository",href="https://github.com/anahitajensen/CW3E.StreamflowDashBoard",".")),
                       h3("Hydrograph:"),
                       p("A hydrograph is a chart showing, most often, river stage (height of the water above an arbitrary altitude) and streamflow (amount of water, usually in cubic feet per second). Other properties, such as rainfall can also be plotted. This application gives the user the ability to choose either discharge or level to plot, with the option of adding precipitation to either plot (USGS)."),          
                       h3("Site Type:"),
                       p(strong("Stream:"),"Streamflow data is collected at these sites, through the processes detailed below in the 'Data Collection' section."),
                       p(strong("Precip Met:"),"Precipitation data is collected at these sites."),
                       p(strong("SMOIL:"),"SMOIL (CW3E specific term) stations collect various data, including air temperature, precipitation, wind speed, soil moisture, and soil temperature."),
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
                       p("The app is currently being developed. Data is mostly raw and still being processed."),
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
  
  #takes the station input and creates a function that references the data frame specific to that variable and station
  manual_discharge <- reactive({paste0(input$select_station,"_QM")})
  discharge <- reactive({paste0(input$select_station,"_Q")})           
  level <- reactive({paste0(input$select_station,"_Le")}) 
  
  #if statements select the precipitation station based on the streamflow station chosen
  #switch MEW from WDG TO PVN
  precipitation_x <- reactive({ if (input$select_station %in% c("BYS", "MLL")) BCC_hourly$Date.Time else
                                if (input$select_station %in% c("CLD", "PRY", "WHT")) DRW_hourly$Date.Time else
                                if (input$select_station == "MEW") WDG_hourly$Date.Time })
  
  precipitation_y <- reactive({ if (input$select_station %in% c("BYS", "MLL")) BCC_hourly$rain_in_BCC else
                                if (input$select_station %in% c("CLD", "PRY", "WHT")) DRW_hourly$rain_in_DRW else
                                if (input$select_station == "MEW") WDG_hourly$rain_in_WDG })
  
  #creating the plot
  output$graph <- renderPlotly({
    
    req(input$var)
    
    p <- plot_ly()
    
    #if else statement is for changing plot based on what variable is selected; manual discharge and discharge will plot if discharge selected
    #precipitation plots regardless so it is outside of the if else statement
    if (input$var == "Discharge") {
      
      # Add points for manual discharge data
      p <- add_trace(p,
                     x = ~base::get(manual_discharge())$Date.Time,
                     y = ~base::get(manual_discharge())$Q.cfs,
                     type = "scatter",
                     mode = "markers",
                     marker = list(color = "darkgreen"),
                     name = "Manual Discharge")
      
      ;
      
      # Add lines for discharge data
      p <- add_trace(p,
                     x = ~base::get(discharge())$Date.Time,
                     y = ~base::get(discharge())$Q.cfs,
                     type = "scatter",
                     mode = "lines",
                     line = list(color = '#2fa819', width = 1, dash = 'solid'),
                     name = "Discharge")
    } else {
      
      # Add lines for level data
      p <- add_trace(p,
                     x = ~base::get(level())$Date.Time,
                     y = ~base::get(level())$level.in,
                     type = "scatter",
                     mode = "lines",
                     line = list(color = 'red', width = 1, dash = 'solid'),
                     name = "Level")
    }
    
    #add bars for precipitation
    p <- add_bars(p,
                  x = precipitation_x(),
                  y = precipitation_y(),
                  yaxis = "y2",
                  marker = list(color = "blue", width = 1),
                  name = 'Precipitation')
    
    #if else statement to switch between discharge and level y-axis; x-axis and y-axis2 (right side) stay the same
    p <- layout(p,
                xaxis = list(title = "Time (15 minute intervals)"),
                yaxis = if (input$var == "Discharge" | input$var == "Manual Discharge") {dischargeAx} else {levelAx},
                yaxis2 = rainAx)
    
    return(p)
    
  })
  
 
  
  #map of stations for second tab-------------------------------------------------------------------------------------------------------------
  
  #color palette for the points
  RdYlBu <- colorFactor("RdYlBu",domain=stat_location$Site.Type)
  
  output$map <- renderLeaflet({
    leaflet(stat_location) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%              #this is what the map looks like (many different options, can change)                      
      setView(map, lng = -119.7871, lat = 36.7378, zoom = 6)  %>%   #how zoomed out the map is when it first loads, made it so that all stations show up       
      #adding points for the stations
      addCircleMarkers(lng=~stat_location$Longitude, lat=~stat_location$Latitude,         
                       stroke = FALSE, fill=TRUE, fillOpacity=1,
                       color = ~RdYlBu(stat_location$Site.Type),
                       #what info pops up when you click on point
                       popup=paste(stat_location$Name, "<br>",
                                   "CW3E Code:", stat_location$CW3E.Code, "<br>",
                                   "Watershed:", stat_location$Watershed, "<br>",
                                   "Elevation:", stat_location$Elevation..Approx..m.,"m", "<br>",
                                   "(",stat_location$Latitude,stat_location$Longitude,")"
                       )) %>%
      addLegend("topleft", pal=RdYlBu, values=stat_location$Site.Type, title="Site Type",opacity=1)
  })
  
  #map of stations for first tab, same code as above-------------------------------------------------------------------------------------------------------------
  
 
  RdYlBu <- colorFactor("RdYlBu",domain=stat_location$Site.Type)
  
  output$map2 <- renderLeaflet({
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
                                                          list(lengthMenu = c(5,10,20,33), pageLength = 8))})
  
  #data table under hydrograph tab-----------------------------------------------------------------------------------------------------
  
  output$data_table2 <- DT::renderDataTable({DT::datatable(stat_location2, rownames=FALSE,
                                                           colnames = c("Site Name","Watershed","CW3E Code"),
                                                           list(lengthMenu = c(5,10,14), pageLength = 5))})
  
}

#run app----------------------------------------------------------------------------------------------------------------------

shinyApp(ui=ui,server=server)
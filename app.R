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

#data loading and formatting--------------------------------------------------------------------------------------------------------------

setwd("C:/Users/anahi/OneDrive/Documents")

#station location data from github
stat_location <- read.csv("GitHub/R-shiny/Data/Stream/CW3E_Stations_all.csv")
stat_location$Site.Type <- gsub("SMOIL", "Smoil", stat_location$Site.Type)
stat_location2 <- read.csv("/Users/anahi/OneDrive/Documents/GitHub/CW3E.StreamflowDashBoard/station_location2.csv")

#precipitation data from github
precip <- read_excel("GitHub/R-shiny/Data/Stream/BCC_all_precip.xlsx")
#Aggregate the data to 15-minute intervals
precip15 <- precip %>%group_by(TIMESTAMP=cut(TIMESTAMP,"15 mins"))%>%summarise(Rain_mm_Tot=sum(Rain_mm_Tot))

#precipitation data from skyriver
#BCC <- read.csv("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/BCC/BoyesCreekCanyon_TwoMinWS.dat")
#BVS <- read.csv("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/BVS/BrownsValleySchool_TwoMin.dat")
#DRW <- read.table("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/DRW/Deerwood_TwoMinWS.dat")
#WDG <- read.table("//Skyriver/CW3E_data/CW3E_SurfaceMet_Archive/WDG/WindyGap_TwoMinWS.dat",fill=TRUE)

#set wd to //Skyriver/CW3E_data/
setwd("Z:/")

#streamflow data
BYS_Q <- read.csv("/CW3E_Streamflow_Archive/BYS/Processed/BYS_LogLog_Q.csv")
CLD_Q <- read.csv("/CW3E_Streamflow_Archive/CLD/Processed/CLD_LogLog_Q.csv")
MEW_Q <- read.csv("/CW3E_Streamflow_Archive/MEW/Processed/MEW_LogLog_Q.csv")
MLL_Q <- read.csv("/CW3E_Streamflow_Archive/MLL/Processed/MLL_LogLog_Q.csv")
PRY_Q <- read.csv("/CW3E_Streamflow_Archive/PRY/Processed/PRY_LogLog_Q.csv")
WHT_Q <- read.csv("/CW3E_Streamflow_Archive/WHT/Processed/WHT_LogLog_Q.csv")

#format streamflow data timestamps
BYS_Q$bys.dt2= as.POSIXct(BYS_Q$bys.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
CLD_Q$cld.dt2= as.POSIXct(CLD_Q$cld.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MEW_Q$mew.dt2= as.POSIXct(MEW_Q$mew.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
MLL_Q$mill.dt2=as.POSIXct(MLL_Q$mill.dt2,tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
PRY_Q$pry.dt2= as.POSIXct(PRY_Q$pry.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")
WHT_Q$wht.dt2= as.POSIXct(WHT_Q$wht.dt2, tz= "UTC", format= "%Y-%m-%d %H:%M:%S")

#manual streamflow data
#BYS_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/BYS_Manual_Q_R.xlsx")      #for bys, need to edit excel sheet first 
CLD_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/CLD_Manual_Q_R.xlsx")
MEW_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/MEW_Manual_Q_R.xlsx")
MLL_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/MLL_Manual_Q_R.xlsx")
PRY_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/PRY_Manual_Q_R.xlsx")
WHT_QM <- read_xlsx("Z:/CW3E_Streamflow_Archive/Data/WHT_Manual_Q_R.xlsx")

#editing date format manual streamflow data
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

#stage data
stage_data = list.files(path = "/Users/anahi/OneDrive/Documents/GitHub/CW3E.StreamflowDashBoard/",pattern="*level.csv",full.names=TRUE)
stage = lapply(stage_data,read.csv)

#merge precipitation with streamflow (using CLD_QM as test for manual data)
merged <- merge(BYS_Q,precip15,by.x ="bys.dt2", by.y = "TIMESTAMP",all = TRUE)
merged <- merge(CLD_Q,merged, by.x = "cld.dt2", by.y = "bys.dt2", all = TRUE)
merged <- merge(MEW_Q,merged, by.x = "mew.dt2", by.y = "cld.dt2", all = TRUE)
merged <- merge(MLL_Q,merged, by.x = "mill.dt2",by.y = "mew.dt2", all = TRUE)
merged <- merge(PRY_Q,merged, by.x = "pry.dt2", by.y = "mill.dt2",all = TRUE)
merged <- merge(WHT_Q,merged, by.x = "wht.dt2", by.y = "pry.dt2", all = TRUE)
merged <- merge(CLD_QM,merged, by.x = "Date.Time", by.y = "wht.dt2", all=TRUE)
merged <- merge(MEW_QM,merged, by = "Date.Time", all=TRUE)
merged <- merge(MLL_QM,merged, by = "Date.Time", all=TRUE)
merged <- merge(PRY_QM,merged, by = "Date.Time", all=TRUE)
merged <- merge(WHT_QM,merged, by = "Date.Time", all=TRUE)

CLD_M = merged$Q.cfs.CLD
MEW_M = merged$Q.cfs.MEW
MLL_M = merged$Q.cfs.MLL
PRY_M = merged$Q.cfs.PRY
WHT_M = merged$Q.cfs.WHT
BYS = merged$bys.q4 
CLD = merged$cld.q3 
MEW = merged$mew.q3
MLL = merged$mill.q3
PRY = merged$pry.q3
WHT = merged$wht.q3
date = merged$Date.Time
rain = merged$Rain_mm_Tot 

#set right y axis for precipitation
rainAx = list(overlaying="y",side="right",title="Precipitation (mm)",range=c(300,0),showgrid=FALSE)

#set left y axis for level
levelAx = list(side="left",title="Level (inches)",range=c(0,50),showgrid=FALSE)

#set left y axis for discharge
dischargeAx = list(side="left",title="Discharge (ft³/s)",showgrid=FALSE)

#user interface--------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
  
  headerPanel(
    title=tags$a(href='https://cw3e.ucsd.edu/overview/',tags$img(src='logo.png', height = 80, width = 300), target="_blank"),
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"))),
  
  titlePanel(strong("Streamflow Dashboard")),
  
  tabsetPanel(type = "tabs",
              
              tabPanel("Hydrograph",
                       
                       sidebarLayout(   
                         
                         sidebarPanel(position = "left",
                                      
                                      selectizeInput(
                                        inputId = "select_station",
                                        label = "Select Station:",
                                        choices = stat_location$CW3E.Code,
                                        selected = "CLD"), 
                                      
                                      selectizeInput(
                                        inputId = "var",
                                        label = "Select Variable:",
                                        choices = list("Discharge", "Level"), 
                                        selected="Discharge"), 
                                      
                                      p(strong("Notes on Precipitation:")),
                                      p("To add or remove precipitation from the hydrograph, click on 'Precipitation' in
                                         the legend located in the top right corner of the hydrograph."),
                                      p("*Please note that precipitation data will be taken from the closest available 
                                         surface meteorology station, which is different from the streamflow stations."),
                                      br(),
                                      
                                      sliderInput(
                                        inputId = "date_range",
                                        label = "Select Date Range:",
                                        min = as.POSIXct("2015-09-09 12:30:00"),
                                        max = as.POSIXct("2023-10-25 07:00:00"),
                                        value=c(as.POSIXct("2015-09-09 12:30:00"),
                                                as.POSIXct("2023-10-25 07:00:00")),
                                        timeFormat="%Y-%m-%d %H:%M:%S")),

                         mainPanel(position = "right",
                                   plotlyOutput("graph"),
                                   br(),br(),
                                   dataTableOutput("data_table2"),
                                   plotlyOutput("selected_var"),
                                   plotlyOutput("selected_dates")
                                   #imageOutput("recent_image")
                         )
                       )),                         
              
              tabPanel("Location Map",
                       column(6, leafletOutput("map", height = "70vh")),
                       column(6, dataTableOutput("data_table"))),
              
              tabPanel("About", 
                       textOutput("info"),  
                       h3("Introduction:"),
                       p("This application displays streamflow and precipitation data gathered by CW3E. Source code is available at the",a("CW3E Streamflow Dashboard GitHub Repository",href="https://github.com/anahitajensen/CW3E.StreamflowDashBoard")),
                       h3("Hydrograph:"),
                       p("A hydrograph is a chart showing, most often, river stage (height of the water above an arbitrary altitude) and streamflow (amount of water, usually in cubic feet per second). Other properties, such as rainfall and water-quality parameters can also be plotted."),          
                       h3("Data Collection:"),
                       h4(em("Discharge (Flow):")),
                       p("Discharge data on this page is measured through multiple methods that will be described in detail below."),
                       p("When using a",strong("current meter"),", a stream channel cross section is first divided into numerous vertical subsections. In each subsection, the area is obtained by measuring the width and depth of the subsection, and the water velocity is determined using a current meter. The discharge in each subsection is computed by multiplying the subsection area by the measured velocity. The total discharge is then computed by summing the discharge of each subsection."),
                       p("When using",strong("ADCP (Acoustic Doppler Current Profiler)"),", the Doppler Effect is used to determine water velocity by sending a sound pulse into the water and measuring the change in frequency of that sound pulse reflected back to the ADCP by sediment or other particulates being transported in the water. The ADCP also uses acoustics to measure water depth by measuring the travel time of a pulse of sound to reach the river bottom at back to the ADCP. To make a discharge measurement, the ADCP is mounted onto a boat or into a small watercraft with its acoustic beams directed into the water from the water surface. The ADCP is then guided across the surface of the river to obtain measurements of velocity and depth across the channel. The river-bottom tracking capability of the ADCP acoustic beams or a Global Positioning System (GPS) is used to track the progress of the ADCP across the channel and provide channel-width measurements. Using the depth and width measurements for calculating the area and the velocity measurements, the discharge is computed by the ADCP using discharge = area x velocity, similar to the conventional current-meter method."),
                       p(strong("Computer Vision Stream Gaging (CVSG)")," captures stereo camera footage of the water surface, which is analyzed to estimate water level, surface velocities, and gauged discharge.  The system utilizes a cloud architecture that allows for remote management of device configurations, automated data processing, and integration of internal and external data sources (Hutley et al., 2022).CVSG is new technology developed by Xylem Inc., and is being used at CW3E's Santa Ana River site below Prado Dam (SAP)."),
                       h4(em("Level (Stage):")),
                       p("Stage is measured by CW3E using Solinst leveloggers and barologgers. Leveloggers measure absolute pressure (water pressure + atmospheric pressure). Barologgers record atmospheric pressure. The most accurate method of obtaining changes in water level is to compensate for atmospheric pressure fluctuations using a Barologger 5, avoiding time lag in the compensation. The Barologger 5 is set above high water level in one location on site."),
                       h4(em("Precipitation:")),
                       p("CW3E’s surface meteorological stations measure precipitation, among other variables. Data is collected every two minutes."),
                       br(),
                       h3("References:"),
                       p(a("https://www.usgs.gov/special-topics/water-science-school/science/how-streamflow-measured")),
                       p(a("https://www.usgs.gov/special-topics/water-science-school/science/streamflow-and-water-cycle")),
                       p(a("https://cw3e.ucsd.edu/cw3e-surface-meteorology-observations/")),
                       p(a("https://www.solinst.com/products/dataloggers-and-telemetry/3001-levelogger-series/levelogger/datasheet/barometric-compensation.php")),
                       p("Hutley, N. R., Beecroft, R., Wagenaar, D., Soutar, J., Edwards, B., Deering, N., Grinham, A., and Albert, S.: Adaptively monitoring streamflow using a stereo computer vision system, EGUsphere [preprint],",a("https://doi.org/10.5194/egusphere-2022-735"),", 2022.")
              )),
  
  imageOutput(outputId = "logo.png")
  
)

#server----------------------------------------------------------------------------------------------------------------------

server <- function(input,output,session){
  
  #hydrograph--------------------------------------------------------------------------------------------------------------------  
  manual <- reactive({paste0(input$select_station,"_M")})
  y <- reactive({input$select_station})
  #manual <- paste(deparse(substitute(input$select_station)),"_M")
  
  output$graph <- renderPlotly({
    
    filteredData <- subset(merged, date >= input$date_range[1] & date <= input$date_range[2])
    
    plot_ly() %>%
      
      #add points for manual streamflow data
      add_markers(data=filteredData,
        x=~date,
        y=~get(manual()),
        type="scatter",
        marker = list
        (color ="darkgreen"),name="Manual Streamflow") %>%
      
      #adding lines for streamflow data
      add_trace(data=filteredData,
                x=~date,
                y=~get(y()),                  
                type="scatter", mode="lines", line = list               
                (color='#2fa819',width=1,dash='solid'),name="Streamflow") %>%      
      
      #add bars for precipitation
      add_trace(data=filteredData,
                x=~date,
                y=~rain,
                type="bar", yaxis="y2", marker = list
                (color="blue",width=1),name='Precipitation') %>%
      
      layout(
        xaxis = list (title = "Time (daily)"), 
        yaxis = if(input$var == "Discharge"){dischargeAx} else{levelAx},
        yaxis2 = rainAx) 
    
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
  
  #data table under hydrograph tab-----------------------------------------------------------------------------------------------------
  
  output$data_table2 <- DT::renderDataTable({DT::datatable(stat_location2, rownames=FALSE,
                                                          colnames = c("Site Name","Watershed","CW3E Code"),
                                                          list(lengthMenu = c(5,10,20,45), pageLength = 5))})
  
}

#run app----------------------------------------------------------------------------------------------------------------------

shinyApp(ui=ui,server=server)
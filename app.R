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
library(htmlwidgets)
library(data.table)

#data loading and formatting--------------------------------------------------------------------------------------------------------------

#set environment and retrieve config file, change "anahita" to your environment
Sys.setenv(R_CONFIG_ACTIVE = "sarah")
config <- config::get()                       
setwd(config$root_dir)

#station location data for data table on 'station location' tab, has CW3E stations and their coordinates
stat_location <- read.csv(config$stat_location)
stat_location$Site.Type <- gsub("Smoil", "SMOIL", stat_location$Site.Type)
#currently only data from stream, precip met, and smoil stations is being used in dashboard so only kept those
stat_location <- stat_location[is.element(stat_location$Site.Type, c("Stream","Precip Met","SMOIL")),]
#station location data for data table on 'hydrograph' tab, only has name/watershed/site.type
stat_location2 <- read.csv(config$stat_location2)

#for loop for precipitation data, add new station to 'stations' below (will work as long as data is formatted the exact same, see data in GitHub to view formatting style)

stations <- c("BCC","BVS","DRW","WDG")

for (station in stations) {
  
  precip_data <- read.csv(paste(config$precip_data_path, paste("/",station,"_precip15min.csv", sep = ""), sep = ""), header = TRUE)
  if (station == "DRW") {precip_data <- precip_data[-(1:2502),]}        #DRW had very obviously incorrect values so I got rid of them, not sure why
  
  #format timestamps
  precip_data$Date.Time <- as.POSIXct(precip_data$Date.Time, tz = "UTC", format =  "%m/%d/%Y %H:%M")
  
  #make data hourly instead of 15 minutes
  precip_data_hourly <- precip_data %>%  group_by(Date.Time = cut(Date.Time, "60 mins")) %>%  summarise("rain_in" = sum(!!sym("rain_in")))
  precip_data_hourly$Date.Time <- as.POSIXct(precip_data_hourly$Date.Time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  assign(paste(station, "_hourly", sep = ""), precip_data_hourly)
}

#for loop for stage, discharge, manual discharge data; add new site to 'sites' below

sites <- c("BYS","CLD","MEW","MLL","WHT","PRY")

for (site in sites) {
  
  #stage data
  stage_data <- read.csv(paste(config$stage_data_path, paste(site,"_barocorrected_level.csv", sep = ""), sep = ""), header = TRUE)
  
  #format stage data timestamps
  stage_data$Date.Time <- as.POSIXct(stage_data$Date.Time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  
  #if the site is PRY, connect the stage data to the field camera photos
  if (site == "PRY") {
    #get hourly stage values
    stage_hourly <- stage_data
    #print(head(stage_hourly))
    #add an hourly column
    stage_hourly$Date.Time.Hour <- round_date(stage_hourly$Date.Time, unit = "hour") #rounds date to the nearest hour; will round up if midway through the hour
    #print(head(stage_hourly))
    #load in photo paths and times, and load in labels
    PRY_photo_path <- read.csv(paste(config$photo_data_path, paste(site, "_path_date.csv", sep = ""),sep = ""), header = TRUE) #format makes it so that we can do this with new sites
    #print(head(PRY_photo_path))
    #get the date formatted for PST time zone (these photos at least were taken in PST)
    PRY_photo_path$date <- ymd_hms(PRY_photo_path$date,tz = "America/Los_Angeles") #get time zone
    #print(head(PRY_photo_path))
    #round date to nearest hour and put in UTC timezone
    PRY_photo_path$Date.Time.Hour <- round_date(with_tz(PRY_photo_path$date, tz = "GMT"), unit = "hour") #convert to GMT, hourly
    #print(head(PRY_photo_path))
    #make sure date is the same format as the stage data (likely could make these steps more concise)
    PRY_photo_path$Date.Time.Hour <- as.POSIXct(PRY_photo_path$Date.Time.Hour, tz = "UTC", format = "%Y-%m-%d %H:%M:%S") #make into same format as stage_data dates
    #print(head(PRY_photo_path))
    #merge paths/times with stage_hourly
    photo_discharge <- base::merge(stage_hourly, PRY_photo_path,by="Date.Time.Hour", all.y = TRUE) #LIKELY GET RID OF THE X columns (index) of each so that there isn't the "Warning:  2 failed to parse." warning
    #print(head(photo_discharge))
    #put data into a table so that it is easier to plot later
    photo_table <- data.table(timestamp = as.POSIXct(photo_discharge$Date.Time), timehour = as.POSIXct(photo_discharge$Date.Time.Hour), type = "photo",location = photo_discharge$path, value = photo_discharge$level.in)
    print(head(photo_table))
  }
  else {photo_table <- NULL}
  
  #discharge data
  streamflow_data <- read.csv(paste(config$streamflow_data_path, paste(site, "/Processed/", site, "_LogLog_Q_GM.csv", sep = ""), sep = ""), header = TRUE)
  streamflow_data <- rename(streamflow_data, Q.cfs = paste(tolower(site), ".q3", sep = ""), Date.Time = paste(tolower(site), ".dt2", sep = ""))
  
  #format discharge data timestamps
  streamflow_data$Date.Time <- as.POSIXct(streamflow_data$Date.Time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  
  #manual discharge data
  manual_streamflow_data <- read_xlsx(paste(config$manual_streamflow_data_path, paste(site, "_Manual_Q_R.xlsx", sep = ""), sep = ""))
  
  #format manual discharge data timestamps
  manual_streamflow_data$Date.Time <- as.POSIXct(manual_streamflow_data$Date.Time, tz = "UTC", format = "%m/%d/%y %H:%M:%S")
  
  #assign the data frames to specific variables
  assign(paste(site, "_Ph",sep = ""),photo_table)
  assign(paste(site, "_Le", sep = ""), stage_data)
  assign(paste(site, "_Q", sep = ""), streamflow_data)
  assign(paste(site, "_QM", sep = ""), manual_streamflow_data)
}

#set right y axis for precipitation
rainAx = list(overlaying="y",side="right",title="Precipitation (inches)",range=c(1,0),showgrid=FALSE)

#set left y axis for level
levelAx = list(side="left",title="Level (inches)",showgrid=FALSE)

#set left y axis for discharge
dischargeAx = list(side="left",title="Discharge (ft³/s)",showgrid=FALSE)

#user interface--------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  #set theme of app
  theme = shinytheme("flatly"),
  
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
                                        selected = "PRY"),              #the station that shows up when dashboard first loads
                                      
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
                                      p(strong("Pairing of Streamflow Sites with Surface Met (Precipitation) Sites:")),
                                      p("Format: Streamflow Site(Surface Met Site)"),
                                      p("CLD(DRW),  PRY(DRW),  WHT(DRW),  BYS(BCC),  MLL(BCC),  MEW(WDG)"),
                                      br(), 
                                      
                                      #creates option for user to select which date range they want for hydrograph, does not work right now
                                      dateRangeInput(
                                        inputId = "date_range",
                                        label = "Select Date Range:",
                                        start = as.POSIXct("2018-01-01"),      
                                        end = as.POSIXct("2023-01-01"),
                                        min = as.POSIXct("2018-01-01"),      
                                        max = as.POSIXct("2023-01-01"),
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
  photo <- reactive({paste0(input$select_station,"_Ph")}) 
  
  #make a reactive expression to filter the data based on the date range input
  filtered_data <- reactive({
    req(input$date_range)
    
    #precipitation data, select precipitation station based on selected streamflow station
    selected_data <- if (input$select_station %in% c("BYS", "MLL")) {BCC_hourly} else 
      if (input$select_station %in% c("CLD", "PRY", "WHT")) {DRW_hourly} else 
        if (input$select_station == "MEW") {WDG_hourly}
    
    #filter data by date range input
    precipitation_data_filtered <- filter(selected_data, Date.Time >= input$date_range[1] & Date.Time <= input$date_range[2])
    streamflow_data_filtered <- filter(streamflow_data, Date.Time >= input$date_range[1] & Date.Time <= input$date_range[2])
    stage_data_filtered <- filter(stage_data, Date.Time >= input$date_range[1] & Date.Time <= input$date_range[2])
    manual_streamflow_data_filtered <- filter(manual_streamflow_data, Date.Time >= input$date_range[1] & Date.Time <= input$date_range[2])
    if (is.null(photo_table)){
      photo_data_filtered <- NULL
    } 
    else {
      photo_data_filtered <- filter(photo_table, timestamp >= input$date_range[1] & timestamp <= input$date_range[2])
      print(photo_data_filtered)
    }
    
    #return filtered data
    list(
      discharge = streamflow_data_filtered, 
      level = stage_data_filtered, 
      manual_discharge = manual_streamflow_data_filtered,
      precipitation = precipitation_data_filtered,
      photo = photo_data_filtered)
    
  })
  
  #--------------creating the plot-------------------------------
  output$graph <- renderPlotly({
    
    req(input$var)

    p <- plot_ly()
    
    #if else statement is for changing plot based on what variable is selected; manual discharge and discharge will plot if discharge selected
    #precipitation plots regardless so it is outside of the if else statement
    if (input$var == "Discharge") {
      
      # Add points for manual discharge data
      p <- add_trace(p,
                     x = ~filtered_data()$manual_discharge$Date.Time,
                     y = ~filtered_data()$manual_discharge$Q.cfs,
                     type = "scatter",
                     mode = "markers",
                     marker = list(color = "darkgreen"),
                     name = "Manual Discharge")
      
      ;
      
      # Add lines for discharge data
      p <- add_trace(p,
                     x = ~filtered_data()$discharge$Date.Time,
                     y = ~filtered_data()$discharge$Q.cfs,
                     type = "scatter",
                     mode = "lines",
                     line = list(color = '#2fa819', width = 1, dash = 'solid'),
                     name = "Discharge")
    } else {
      print("hi")
      print(site) #testing to see what site it is, and it says it is PRY now that I changed the order to PRY being the last site in the list (used to just say WHT)
      print(head(filtered_data()$photo$timestamp))
      print(head(gsub("^.*\\/", "", filtered_data()$photo$location))) #this gives the image name
      #add points for times of photos
      if (site == "PRY") {
        p <- add_trace(p,
                       x = filtered_data()$photo$timestamp,
                       y = filtered_data()$photo$value,
                       type = "scatter",
                       mode = "markers",
                       marker = list(color = "black"),
                       text = gsub("^.*\\/", "", filtered_data()$photo$location))
      }
      ;
      # Add lines for level data
      p <- add_trace(p,
                     x = ~filtered_data()$level$Date.Time,
                     y = ~filtered_data()$level$level.in,
                     type = "scatter",
                     mode = "lines",
                     line = list(color = 'red', width = 1, dash = 'solid'),
                     name = "Level")
    }
      
    #add bars for precipitation
    p <- add_bars(p,
                  x = filtered_data()$precipitation$Date.Time,
                  y = filtered_data()$precipitation$rain_in,
                  yaxis = "y2",
                  marker = list(color = "blue", width = 1),
                  name = 'Precipitation')
    
    #if else statement to switch between discharge and level y-axis; x-axis and y-axis2 (right side) stay the same
    p <- layout(p,
                xaxis = list(title = "Time (15 minute intervals)"),
                yaxis = if (input$var == "Discharge" | input$var == "Manual Discharge") {dischargeAx} else {levelAx},
                yaxis2 = rainAx)
    
    #add field camera photos that pop up when hovering over graph, does not work but also doesn't prevent app from working
    #could an issue with this be that the photo's dates are not connecting to the dates on the graph?
    
    p %>% htmlwidgets::onRender("
    function(el, x) {
      // when hovering over an element, do something
      el.on('plotly_hover', function(d) {
        
        // extract tooltip text
        console.log(p)
        point = d.points[0].pointIndex;
        path = d.points[0].text[point] //data.text[point]
        console.log(point)
        console.log(path)
        // image is stored locally
        image_location = 'https://raw.githubusercontent.com/seogle/PRY_thumbnails/main/PRY_all/' + path
        console.log(image_location);
    
        // define image to be shown
        var img = {
          // location of image
          source: image_location, 
          // top-left corner
          x: 0.4,
          y: 1,
          sizex: 0.2,
          sizey: 0.2,
          xref: 'paper',
          yref: 'paper'
        };

        // show image and annotation 
        Plotly.relayout(el.id, {
            images: [img] 
        });
      })
    }
    ")
    
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
  
  #map of stations for first tab-------------------------------------------------------------------------------------------------------------
  station <- reactive({paste0(input$select_station)})
  selected_station_data <- stat_location[stat_location$CW3E.Code == ~get(station()), ]
  
  RdYlBu2 <- colorFactor("RdYlBu",domain=selected_station_data$Site.Type)
  
  output$map2 <- renderLeaflet({
    leaflet(selected_station_data) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%                         
      setView(map, lng = -119.7871, lat = 36.7378, zoom = 6)  %>%
      addCircleMarkers(lng=~selected_station_data$Longitude, lat=~selected_station_data$Latitude,
                       stroke = FALSE, fill=TRUE, fillOpacity=1,
                       color = ~RdYlBu2(selected_station_data$Site.Type),
                       popup=paste(selected_station_data$Name, "<br>",
                                   "CW3E Code:", selected_station_data$CW3E.Code, "<br>",
                                   "Site Type:", selected_station_data$Site.Type, "<br",
                                   "Watershed:", selected_station_data$Watershed, "<br>",
                                   "Elevation:", selected_station_data$Elevation..Approx..m.,"m", "<br>",
                                   "(",selected_station_data$Latitude,selected_station_data$Longitude,")"
                       )) %>%
      addLegend("topleft", pal=RdYlBu2, values=selected_station_data$Site.Type, title="Site Type",opacity=1)
    
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
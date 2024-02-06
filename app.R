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
library(bslib)
library(data.table)

#data loading and formatting----------------------------------------------------------------------------------------------------------------------------------------------------

#set environment and retrieve config file, change "anahita" to your environment (see config file in GitHub for clarity)
Sys.setenv(R_CONFIG_ACTIVE = "anahita")
config <- config::get()                       
setwd(config$root_dir)

#station location data for data table on 'station location' tab, has CW3E stations and their coordinates
stat_location <- read.csv(config$stat_location)
#station location data for data table on 'hydrograph' tab, only has name/watershed/site.type
stat_location2 <- read.csv(config$stat_location2)

#precipitation data loading and processing------------------------------------------------------------------------------------------------------------------------------------------------------------------

#read in precipitation data
BVS <- read.csv("//Skyriver.ucsd.edu/CW3E_data/CW3E_SurfaceMet_Archive/BVS/BrownsValleySchool_TwoMin.dat",skip=3,header=TRUE,sep = ",",fill=TRUE)
BCC <- read.csv("//Skyriver.ucsd.edu/CW3E_data/CW3E_SurfaceMet_Archive/BCC/BoyesCreekCanyon_TwoMinWS.dat",skip=3,header=TRUE,sep = ",",fill=TRUE)
DRW <- read.csv("//Skyriver.ucsd.edu/CW3E_data/CW3E_SurfaceMet_Archive/DRW/Deerwood_TwoMinWS.dat",skip=3,header=TRUE,sep = ",",fill=TRUE)
WDG <- read.csv("//Skyriver.ucsd.edu/CW3E_data/CW3E_SurfaceMet_Archive/WDG/WindyGap_TwoMinWS.dat",skip=3,header=TRUE,sep = ",",fill=TRUE)

#keep only precipitation
BVS <- BVS %>%  select(X, Tot)
BCC <- BCC %>%  select(X, Tot)
DRW <- DRW %>%  select(X, Tot)
WDG <- WDG %>%  select(X, Tot)

#convert precipitation units from mm to inches
BVS <- BVS %>% mutate(Tot = Tot * 0.0393701)
BCC <- BCC %>% mutate(Tot = Tot * 0.0393701)
DRW <- DRW %>% mutate(Tot = Tot * 0.0393701)
WDG <- WDG %>% mutate(Tot = Tot * 0.0393701)

#change timestamps to UTC
BVS$X <- as.POSIXct(BVS$X, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
BCC$X <- as.POSIXct(BCC$X, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
DRW$X <- as.POSIXct(DRW$X, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
WDG$X <- as.POSIXct(WDG$X, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

#change data to hourly
BVS_hourly <- BVS %>%  group_by(X = cut(X, "60 mins")) %>%  summarise("Tot" = sum(!!sym("Tot")))
BCC_hourly <- BCC %>%  group_by(X = cut(X, "60 mins")) %>%  summarise("Tot" = sum(!!sym("Tot")))
DRW_hourly <- DRW %>%  group_by(X = cut(X, "60 mins")) %>%  summarise("Tot" = sum(!!sym("Tot")))
WDG_hourly <- WDG %>%  group_by(X = cut(X, "60 mins")) %>%  summarise("Tot" = sum(!!sym("Tot")))

#change timestamps to UTC (have to do this step again b/c changing data to hourly affects format)
BVS_hourly$X <- as.POSIXct(BVS_hourly$X, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
BCC_hourly$X <- as.POSIXct(BCC_hourly$X, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
DRW_hourly$X <- as.POSIXct(DRW_hourly$X, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
WDG_hourly$X <- as.POSIXct(WDG_hourly$X, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

#discharge and stage and photo data loading and processing-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sites <- c("BYS","CLD","LDM","MEW_DS","MEW_US","MLL","PRY","UDC","WHT") #took out SYR since data not updated

for (site in sites) {
  
  #discharge and stage data
  discharge_stage_data <- read.csv(paste(config$discharge_stage_path, site,"/Processed/ALL.",site,".Level.Discharge.csv",sep = ""), header = TRUE)
  #format timestamps 
  discharge_stage_data$timestamp_UTC <- as.POSIXct(discharge_stage_data$timestamp_UTC, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  #change stage units from feet to inches
  #discharge_stage_data$Level.ft.corrected <- 12*(discharge_stage_data$Level.ft.corrected)
  
  # Replace NULL values with the date from the next row and time set to "00:00:00"
  for (i in 2:(nrow(discharge_stage_data) - 1)) {
    if (is.na(discharge_stage_data$timestamp_UTC[i])) {
      next_row_timestamp <- discharge_stage_data$timestamp_UTC[i + 1]
      discharge_stage_data$timestamp_UTC[i] <- as.POSIXct(paste0(format(next_row_timestamp, "%Y-%m-%d"), " 00:00:00"), tz = "UTC")
    }
  }
  
  #manual discharge data  #for MEW, DS and US sites have different rating curve discharges, but same manual discharge
  if (site == "MEW_DS" | site == "MEW_US") {manual_discharge_data <- read.csv(paste(config$manual_discharge_path, "MEW/Manual_Discharge/MEW_manual_q_final.csv", sep = ""), header = TRUE)}
  else {manual_discharge_data <- read.csv(paste(config$manual_discharge_path, site,"/Manual_Discharge/", site, "_manual_q_final.csv", sep = ""), header = TRUE)}
  #format timestamps
  if (site=="CLD"|site=="LDM"|site=="PRY"|site=="UDC") {manual_discharge_data$date.time <- as.POSIXct(manual_discharge_data$date.time, tz = "UTC", format = "%m/%d/%Y %H:%M")}
  else {manual_discharge_data$date.time <- as.POSIXct(manual_discharge_data$date.time, tz = "UTC", format = "%Y/%m/%d %H:%M:%S")}
  
  # #if the site is PRY, connect the stage data to the field camera photos
  #may run into issue, i think old level was in inches and new level is in feet 
  # if (site == "PRY") {
  #   #get hourly stage values
  #   stage_hourly <- discharge_stage_data
  #   #add an hourly column
  #   stage_hourly$Date.Time.Hour <- round_date(stage_hourly$timestamp_UTC, unit = "hour") #rounds date to the nearest hour; will round up if midway through the hour
  #   #load in photo paths and times, and load in labels
  #   PRY_photo_path <- read.csv(paste(config$photo_data_path, paste(site, "/Trail Cam/Thumbnails/", sep = ""),sep = ""), header = TRUE) #format makes it so that we can do this with new sites
  #   #get the date formatted for PST time zone (these photos at least were taken in PST)
  #   PRY_photo_path$date <- ymd_hms(PRY_photo_path$date,tz = "America/Los_Angeles") #get time zone
  #   #round date to nearest hour and put in UTC timezone
  #   PRY_photo_path$Date.Time.Hour <- round_date(with_tz(PRY_photo_path$date, tz = "GMT"), unit = "hour") #convert to GMT, hourly
  #   #make sure date is the same format as the stage data (likely could make these steps more concise)
  #   PRY_photo_path$Date.Time.Hour <- as.POSIXct(PRY_photo_path$Date.Time.Hour, tz = "UTC", format = "%Y-%m-%d %H:%M:%S") #make into same format as stage_data dates
  #   #merge paths/times with stage_hourly
  #   photo_discharge <- base::merge(stage_hourly, PRY_photo_path,by="Date.Time.Hour", all.y = TRUE) #LIKELY GET RID OF THE X columns (index) of each so that there isn't the "Warning:  2 failed to parse." warning
  #   #put data into a table so that it is easier to plot later
  #   photo_table <- data.table(timestamp = as.POSIXct(photo_discharge$Date.Time), timehour = as.POSIXct(photo_discharge$Date.Time.Hour), type = "photo",location = photo_discharge$path, value = photo_discharge$level_corrected_ft)
  # }
  # else {photo_table <- NULL}
  
  #assign the data frames to specific variables
  assign(paste(site, "_D_S", sep = ""), discharge_stage_data)
  assign(paste(site, "_QM", sep = ""), manual_discharge_data)
  #assign(paste(site, "_Ph", sep = ""), photo_table)
}

#rating curve data, just using UDC for now to test out rating curve
stream_sites <- ("UDC")
for (stream_site in stream_sites) {
  ratingcurve_data <- read.csv(paste(config$ratingcurve_data_path, paste("UDC_Rating_Curve.csv", sep = ""), sep = ""), header = TRUE)
  assign(paste(stream_site, "_RC", sep = ""), ratingcurve_data)
}
ratingcurve_data <- ratingcurve_data[-c(1,2),]

#set right y axis for precipitation
rainAx = list(overlaying="y",side="right",title="Precipitation (inches)",range=c(2,0),showgrid=FALSE)

#set left y axis for level
levelAx = list(side="left",title="Level (inches)",showgrid=FALSE)

#set left y axis for discharge
dischargeAx = list(side="left",title="Discharge (ft³/s)",showgrid=FALSE)

#user interface--------------------------------------------------------------------------------------------------------------

#create theme to match CW3E website
#custom_theme <- bs_theme(bg="#eaeaea", fg="#206c8c", primary="#206c8c", primary_light="#206c8c",base_font="Times New Roman", "font-size-base"="1.0rem", border_color="#1e6b8b")

ui <- fluidPage(
  
  theme = shinytheme("flatly"),    #set theme of app

  #make header panel with CW3E logo linked to CW3E website
  headerPanel(title=tags$a(href='https://cw3e.ucsd.edu/overview/',tags$img(src='logo.png', height = 80, width = 300), target="_blank"),
              tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"))),
  
  #title of app, 'strong' makes text bold
  titlePanel(strong("Streamflow Dashboard")),
  
  #creating 3 tabs for dashboard
  tabsetPanel(type = "tabs",
              
              #first tab
              tabPanel("Hydrograph",
                       
                       sidebarLayout(   
                         
                         #creates the user inputs on the left side of the hydrograph
                         sidebarPanel(position = "left",
                                      
                                      #creates option for user to select the station
                                      selectizeInput(
                                        inputId = "select_station",     #user does not see this, need to reference this in server
                                        label = "Select Station:",      #user sees this
                                        choices = c("BYS","CLD","LDM","MEW_DS","MEW_US","MLL","PRY","SYR","WHT","UDC"),       
                                        selected = "BYS"),              #the station that shows up when dashboard first loads
                                      
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
                                      
                                      #creates option for user to select the date range for the hydrograph, edit dates as necessary
                                      dateRangeInput(
                                        inputId = "date_range",
                                        label = "Select Date Range:",
                                        start = as.POSIXct("2018-01-01"),      
                                        end = as.POSIXct("2024-01-01"),
                                        min = as.POSIXct("2018-01-01"),      
                                        max = as.POSIXct("2024-01-01"),
                                      )),
                         
                         #this is what appears on the right side of the 'Hydrograph' tab, so it's the hydrograph, data table, and map
                         mainPanel(position = "right",
                                   plotlyOutput("graph"),
                                   textOutput("qcstatus_text"),
                                   br(),br(),
                                   column(6,leafletOutput("map2")),
                                   column(6,dataTableOutput("data_table2")),
                                   plotlyOutput("selected_var"),
                                   plotlyOutput("selected_dates")
                         )
                       )),                         
              
              #second tab with rating curves
              tabPanel("Rating Curve",
                       
                       sidebarLayout(
                         
                         sidebarPanel(position = "left",
                                      
                                      #creates option for user to select the station
                                      selectizeInput(
                                        inputId = "choose_station",     
                                        label = "Select Station:",      
                                        choices = c("BYS","CLD","MEW","MLL","PRY","WHT","UDC","LDM","SYR"),       
                                        selected = "CLD"),              
                         ),
                         mainPanel(position = "right",
                                   plotlyOutput("ratingcurve_plot")
                         )
                       )),
              
              
              #third tab with map and data table
              tabPanel("Map",
                       column(6, leafletOutput("map", height = "70vh")),
                       column(6, dataTableOutput("data_table"))),
              
              #fourth tab with about section
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
                       p("The app is currently being developed."),
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

#server--------------------------------------------------------------------------------------------------------------------------------------------------------------

server <- function(input,output,session){
  
  #hydrograph---------------------------------------------------------------------------------------------------------------------------------------
  
  #make a reactive expression to filter the data based on the date range input
  filtered_data <- reactive({
    
    req(input$date_range)
    
    #precipitation data, automatically select precipitation station based on selected streamflow station
    precipitation_data <-  if (input$select_station %in% c("BYS", "MLL")){BCC_hourly} else 
                           if (input$select_station %in% c("CLD", "PRY", "WHT")){DRW_hourly} else 
                           if (input$select_station == "MEW") {WDG_hourly} else
                           if (input$select_station %in% c("UDC", "LDM", "SYR")) {BVS_hourly}  
     
    manual_streamflow_data <- if (input$select_station == "BYS"){BYS_QM} else
                              if (input$select_station == "CLD"){CLD_QM} else
                              if (input$select_station == "MEW"){MEW_QM} else
                              if (input$select_station == "MLL"){MLL_QM} else
                              if (input$select_station == "PRY"){PRY_QM} else
                              if (input$select_station == "WHT"){WHT_QM} else
                              if (input$select_station == "UDC"){UDC_QM} else
                              if (input$select_station == "LDM"){LDM_QM} #else
                              #if (input$select_station == "SYR"){SYR_QM}

    discharge_stage_data <- if (input$select_station == "BYS"){BYS_D_S} else
                            if (input$select_station == "CLD"){CLD_D_S} else
                            if (input$select_station == "MEW_DS"){MEW_DS_D_S} else
                            if (input$select_station == "MEW_US"){MEW_US_D_S} else
                            if (input$select_station == "MLL"){MLL_D_S} else
                            if (input$select_station == "PRY"){PRY_D_S} else
                            if (input$select_station == "WHT"){WHT_D_S} else
                            if (input$select_station == "UDC"){UDC_D_S} else
                            if (input$select_station == "LDM"){LDM_D_S} #else
                            #if (input$select_station == "SYR"){SYR_D_S} 
    
    ratingcurve_data <- if (input$choose_station == "UDC"){"UDC_RC"} else    {"UDC_RC"}
                
    #filter data by date range inputted by user
    precipitation_data_filtered <-     filter(precipitation_data, X >= input$date_range[1] & X <= input$date_range[2])
    discharge_stage_data_filtered <-   filter(discharge_stage_data, timestamp_UTC >= input$date_range[1] & timestamp_UTC <= input$date_range[2])
    manual_streamflow_data_filtered <- filter(manual_streamflow_data, date.time >= input$date_range[1] & date.time <= input$date_range[2])
    
    #if (is.null(photo_table)){photo_data_filtered <- NULL } 
    #else {photo_data_filtered <- filter(photo_table, timestamp >= input$date_range[1] & timestamp <= input$date_range[2])}
    
    #return filtered data
    list(
      discharge_stage = discharge_stage_data_filtered, 
      manual_discharge = manual_streamflow_data_filtered,
      precipitation = precipitation_data_filtered,
      #photo = photo_data_filtered,
      ratingcurve = ratingcurve_data)
  })
  
  #--------------creating the plot-------------------------------
  output$graph <- renderPlotly({
    
    req(input$var)
    
    p <- plot_ly()
    
    # if else statement is for changing plot based on what variable is selected; manual discharge and discharge will plot if discharge selected
    # precipitation plots regardless so it is outside of the if else statement
    if (!is.null(filtered_data()$manual_discharge) && nrow(filtered_data()$manual_discharge) > 0) {
      # add_trace for manual discharge
      if ("q.cfs" %in% colnames(filtered_data()$manual_discharge)) {
        p <- add_trace(p,
                       x = ~filtered_data()$manual_discharge$date.time,
                       y = ~filtered_data()$manual_discharge$q.cfs,
                       type = "scatter",
                       mode = "markers",
                       marker = list(color = "darkgreen"),
                       name = "Manual Discharge")
                                                                    }
                                                                                                   }
    
    if (!is.null(filtered_data()$discharge_stage) && nrow(filtered_data()$discharge_stage) > 0) {
      #add discharge
      if ("discharge.cfs" %in% colnames(filtered_data()$discharge_stage)) {
        p <- add_trace(p,
                       x = ~filtered_data()$discharge_stage$timestamp_UTC,
                       y = ~filtered_data()$discharge_stage$discharge.cfs,
                       type = "scatter",
                       mode = "lines",
                       line = list(color = '#2fa819', width = 1, dash = 'solid'),
                       name = "Discharge")
        # Print the value of y for discharge
        print(filtered_data()$discharge_stage$discharge.cfs)
        
      } else {
        #add stage
        if ("level_corrected_ft" %in% colnames(filtered_data()$discharge_stage)) {
          p <- add_trace(p,
                         x = ~filtered_data()$discharge_stage$timestamp_UTC,
                         y = ~filtered_data()$discharge_stage$level_corrected_ft,
                         type = "scatter",
                         mode = "lines",
                         line = list(color = 'red', width = 1, dash = 'solid'),
                         name = "Level")
                                                                                  }
             } 
    
      #  ;
      # # add points for times of photos
      # # time is for level measurement
      # if (input$select_station == "PRY" && !any(is.null(filtered_data()$photo$timestamp))) {
      #   p <- add_trace(p,
      #                  x = filtered_data()$photo$timestamp,
      #                  y = filtered_data()$photo$value,
      #                  type = "scatter",
      #                  mode = "markers",
      #                  marker = list(color = "black",size=1),
      #                  text = gsub("^.*\\/", "", filtered_data()$photo$location))
      # }
    }
    
    # add bars for precipitation
    if (!any(is.null(filtered_data()$precipitation$Tot))) {
      p <- add_bars(p,
                    x = filtered_data()$precipitation$X,
                    y = filtered_data()$precipitation$Tot,
                    yaxis = "y2",
                    marker = list(color = "blue", width = 1),
                    name = 'Precipitation')
    }

    #if else statement to switch between discharge and level y-axis; x-axis and y-axis2 (right side) stay the same
    p <- layout(p,
                xaxis = list(title = "Time (15 minute intervals)"),
                yaxis = if (input$var == "Discharge" | input$var == "Manual Discharge") {dischargeAx} else {levelAx},
                yaxis2 = rainAx)
    
    p <- p %>% onRender("function(el, x) {
      // when hovering over an element, do something
      el.on('plotly_hover', function(d) {
        console.log('Hello, world')
        // extract tooltip text
        // console.log(p)
        p = d.points[0].pointIndex;
        path = d.points[0].data.text[p]
        // console.log(point)
        // console.log(path)
        // image is stored locally
        image_location = 'https://raw.githubusercontent.com/seogle/PRY_thumbnails/main/PRY_all/' + path
        console.log(image_location);

        // define image to be shown
        var img = {
          // location of image
          source: image_location,
          // top-left corner
          x: 0,
          y: 1,
          sizex: 0.3,
          sizey: 0.3,
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
  
  #provisional vs approved------------------------------------------------------------------------------------------------------
  output$qcstatus_text <- renderText({
    data <- filtered_data()
    
    if ("qc_status" %in% colnames(data$discharge_stage)) {
      qc_status <- unique(data$discharge_stage$qc_status)
      status <- if ("Provisional" %in% qc_status) "Provisional" else "Approved"
      return(status)
    } else {return("QC Status Unknown")}
  })
  
  #rating curve plot for second tab-----------------------------------------------------------------------------------------------------------
  
  output$ratingcurve_plot <- renderPlotly({
    
    p <- plot_ly()
    
    # Convert x and y data to data frames
    x_data <- data.frame(Level.ft.corrected = filtered_data()$ratingcurve$Level.ft.corrected)
    y_data <- data.frame(Discharge.cfs = filtered_data()$ratingcurve$Discharge.cfs)
    
    #add rating curve
    p <- add_trace(p,
                   x = filtered_data$ratingcurve$Level.ft.corrected,
                   y = filtered_data$ratingcurve$Level.ft.corrected,
                   type = "scatter",
                   mode = "lines",
                   marker = list(color = "darkgreen"),
                   name = "Rating Curve")
    
    #add manual discharge points
    p <- add_trace(p,
                   x = ~filtered_data()$ratingcurve$Level.ft.corrected,
                   y = ~filtered_data()$ratingcurve$Discharge.cfs,
                   type = "scatter",
                   mode = "markers",  # Change mode to "markers" for individual points
                   marker = list(color = "darkgreen"),
                   name = "Manual Discharge Points")
    
    #axes
    p <- layout(p,
                xaxis = list(title = "Stage (ft)"),
                yaxis = list(title = "Discharge (cfs)"))
    
    return(p)
      })
  
  #map of stations for second tab-------------------------------------------------------------------------------------------------------------
  
  #color palette for the points
  RdYlBu <- colorFactor("RdYlBu",domain=stat_location$Site.Type)
  
  output$map <- renderLeaflet({
    leaflet(stat_location) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%              #basemap, can change if desired                    
      setView(map, lng = -119.7871, lat = 36.7378, zoom = 6)  %>%   #how zoomed out the map is when it first loads      
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
  
  #map on first tab-------------------------------------------------------------------------------------------------------------
  
  filtered <- reactive({stat_location[stat_location$CW3E.Code == input$select_station,]})
  
  output$map2 <- renderLeaflet({leaflet(stat_location) %>% addProviderTiles("Esri.WorldTopoMap")})
  
  map_proxy <- leafletProxy("map2")
  
  observe({
    fdata <- filtered()
    map_proxy %>%
      clearMarkers() %>%
      addMarkers(lng = fdata$Longitude, lat = fdata$Latitude,
                 popup=paste(fdata$Name, "<br>",
                             "CW3E Code:", fdata$CW3E.Code, "<br>",
                             "Watershed:", fdata$Watershed, "<br>",
                             "Elevation:", fdata$Elevation..Approx..m.,"m", "<br>",
                             "(",fdata$Latitude,fdata$Longitude,")" ,options = popupOptions(closeButton = FALSE))) %>%
      flyTo(lng = fdata$Longitude, lat = fdata$Latitude, zoom = 10)    #this 'flies' to the station selected
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
                                                           list(lengthMenu = c(5,10,20,33), pageLength = 5))})
}

#run app----------------------------------------------------------------------------------------------------------------------

shinyApp(ui=ui,server=server)

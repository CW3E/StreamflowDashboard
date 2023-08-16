# Streamflow Dashboard Guide
Contact Anahita Jensen (a7jensen@ucsd.edu, anahita.onyx@gmail.com) with questions.

# Description
The app displays streamflow (discharge, manual discharge, stage) and precipitation data collected by CW3E at various sites across California. The app was created using the Shiny package in R. The user can customize the hydrograph by choosing variables and locations to suit their purposes. 

# Helpful Links
Shiny Basics, good for understanding formatting of code: https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html
Shiny Dashboard Examples, if you want to see other apps' code: https://shiny.posit.co/r/gallery/
Manual Discharge File Update Process: https://docs.google.com/document/d/10jDUoGGsu6WNDdUU6q4eKx2tOvcpWmxcnOr9RBisDP8/edit#heading=h.dnhwnvz2emgf

# Config.yml file 
This configuration file allows you to define various paths and settings for different environments, such as different users or machines. This file is to make the app deployable on the website, and not just your personal computer. You can either add your own environment with your own paths, or use the default environment. If you are adding code with datapaths, make sure to update all of the environments in the config.yml file in GitHub. For a specific environment (ie “anahita”) the relative file paths are defined for each variable, with the root directory also being defined. Root directory + relative file path  = absolute path. Then in the script for the app, you use the path name (ie “streamflow_data_path”) to load the data. Each environment may have different paths but they all need to have the same path names (ie “streamflow_data_path”) so that the app code doesn’t need to be changed depending on who is running it. 

# Script Layout
This code is organized into 3 main sections: data loading & processing, the UI (user interface), and server. Shiny apps have 2 components, a user interface object and a server function, that are passed as arguments to the shinyApp function that creates a Shiny app object from this UI/server pair. 

# Data Loading & Processing
The data came from Skyriver. I loaded all the datasets, grouped by variable. Most, if not all, of the datasets were preprocessed and the data put into the GitHub repository to speed up the app. In the future, if we are using real time data, the data may need to be processed in the app. 

# The UI (User Interface)
All the user inputs occur in the UI section (like ‘Select Station’, ‘Select Variable’, etc). Each of these options is created using a ‘widget’ (see Shiny Basics link above). This has all the aesthetic formatting of the dashboard. This includes the ‘About’ section of the dashboard. 

# The Server
This takes the user inputs and changes the plot/outputs based on that. This includes the hydrograph, the data tables, and the maps. Precipitation stations are chosen by the closest to the streamflow station (in both distance & elevation), if you have a better way let me know. 

# Package Versions I (Anahita) Used
> sessionInfo()

R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] config_0.3.1      anytime_0.3.9     lubridate_1.9.2   forcats_1.0.0     stringr_1.5.0     purrr_1.0.1       readr_2.1.4      
 [8] tidyr_1.3.0       tibble_3.2.1      tidyverse_2.0.0   DT_0.28           leaflet_2.1.2     shinythemes_1.2.0 readxl_1.4.3     
[15] plotly_4.10.2     dplyr_1.1.1       ggplot2_3.4.2     shiny_1.7.4.1  


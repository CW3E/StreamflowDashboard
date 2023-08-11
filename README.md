# Streamflow Dashboard

# Description
This app displays streamflow (discharge, manual discharge, stage) and precipitation data collected by CW3E at various sites across California. 
The app was created using the Shiny package in R. 
The user can customize the hydrograph by locations and either discharge or level. Plotly allows the user to zoom in on parts of the hydrograph.

# Layout
This code is organized into 3 main sections: data loading & processing, the UI (user interface), and server. Shiny apps have 2 components, a user interface object and a server function, that are passed as arguments to the shinyApp function that creates a Shiny app object from this UI/server pair. 

# Data Collection
The 'About' tab on the app details how the data is collected. 

# Sample Data
All data is taken from Skyriver. Sample data is available in this repository, organized as it would be in Skyriver. 



Please note that the app is still a work in progress. 

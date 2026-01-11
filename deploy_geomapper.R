# Deploy GeoMapper to shinyapps.io
library(rsconnect)

# Set account info (replace <SECRET> with your actual secret)
# rsconnect::setAccountInfo(
#   name = 'spatialgeography',
#   token = 'YOUR_TOKEN_HERE',
#   secret = 'YOUR_SECRET_HERE'
# )

# Install required packages if not already installed
required_packages <- c("shiny", "terra", "sf", "leaflet", "ggplot2", "viridis")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Deploy the app
rsconnect::deployApp(
  appDir = 'f:/Agent/Tools/spatial_website/shiny_apps/geomapper',
  appName = 'geomapper',
  account = 'spatialgeography',
  forceUpdate = TRUE
)

# Deploy script for TIF Clipper Shiny App
library(rsconnect)

# Set account info (you'll need to replace <SECRET> with your actual secret)
rsconnect::setAccountInfo(
  name = '55xsd7-spatial-geography',
  token = 'FC2400E06DC0D219592F9B0DAB1DE2FB',
  secret = Sys.getenv("SHINY_SECRET")  # Set this in your environment
)

# Deploy the app
rsconnect::deployApp(
  appDir = 'shiny_apps/tif_clipper',
  appName = 'tif-clipper',
  account = '55xsd7-spatial-geography',
  forceUpdate = TRUE
)

library(shiny)
library(terra)
library(sf)
library(leaflet)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly", version = 5),
  titlePanel("🗺️ GeoMapper - Advanced Raster Visualization Tool"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # File Upload Section
      h4("📁 Data Upload"),
      fileInput("tif_file", "Drag & Drop GeoTIFF", 
                accept = c(".tif", ".tiff"), 
                multiple = FALSE,
                buttonLabel = "Browse...",
                placeholder = "Drop .tif file here"),
      
      fileInput("shp_zip", "Shapefile (Optional, .zip)", 
                accept = c(".zip"), 
                multiple = FALSE),
      
      hr(),
      
      # Band Selection
      uiOutput("band_selector"),
      
      # Visualization Controls
      h4("🎨 Visualization"),
      selectInput("color_palette", "Color Palette:",
                  choices = c("Viridis" = "viridis",
                              "Magma" = "magma",
                              "Inferno" = "inferno",
                              "Plasma" = "plasma",
                              "Spectral" = "Spectral",
                              "RdYlGn" = "RdYlGn",
                              "RdBu" = "RdBu",
                              "Rainbow" = "rainbow",
                              "Terrain" = "terrain.colors",
                              "Topo" = "topo.colors"),
                  selected = "viridis"),
      
      conditionalPanel(
        condition = "input.tabs == 'Interactive Map'",
        sliderInput("map_opacity", "Raster Opacity:", 
                    min = 0, max = 1, value = 0.8, step = 0.1),
        selectInput("basemap_type", "Basemap:",
                    choices = c("OpenStreetMap" = "OpenStreetMap",
                                "Satellite (Esri)" = "Esri.WorldImagery",
                                "CartoDB Positron" = "CartoDB.Positron",
                                "CartoDB Dark Matter" = "CartoDB.DarkMatter"))
      ),
      
      # Plot Customization (for Publication Map)
      conditionalPanel(
        condition = "input.tabs == 'Publication Map'",
        textInput("plot_title", "Plot Title", value = "GeoMapper Visualization"),
        selectInput("plot_theme", "Plot Theme:",
                    choices = c("Minimal" = "minimal",
                                "Classic" = "classic",
                                "Light" = "light",
                                "Dark" = "dark",
                                "Void" = "void")),
        
        sliderInput("dpi", "DPI (Resolution):", min = 72, max = 600, value = 300, step = 50)
      ),

      # Clipping Option
      checkboxInput("apply_clip", "Apply Shapefile Clip", value = FALSE),
      
      hr(),
      
      # Download Section
      h4("📥 Export"),
      downloadButton("download_tif", "Download Clipped Raster"),
      br(), br(),
      downloadButton("download_map", "Download Map (PNG)")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "tabs",
        tabPanel("Interactive Map", 
                 leafletOutput("map", height = "700px")),
        tabPanel("Publication Map", 
                 plotOutput("pub_map", height = "700px")),
        tabPanel("Statistics & Histograms",
                 fluidRow(
                   column(6, verbatimTextOutput("stats")),
                   column(6, plotOutput("histogram", height = "400px"))
                 ))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  data <- reactiveValues(r = NULL, v = NULL, band_names = NULL)
  
  # Process Raster
  observeEvent(input$tif_file, {
    req(input$tif_file)
    tryCatch({
      r <- terra::rast(input$tif_file$datapath)
      data$r <- r
      data$band_names <- names(r)
    }, error = function(e) {
      showNotification(paste("Error loading TIFF:", e$message), type = "error")
    })
  })
  
  # Process Shapefile
  observeEvent(input$shp_zip, {
    req(input$shp_zip)
    temp_dir <- tempdir()
    unzip(input$shp_zip$datapath, exdir = temp_dir)
    shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
    
    if (!is.na(shp_file)) {
      v <- sf::st_read(shp_file, quiet = TRUE)
      data$v <- v
    }
  })
  
  # Band Selector UI
  output$band_selector <- renderUI({
    req(data$r)
    if (nlyr(data$r) > 1) {
      selectInput("selected_band", "Select Band:",
                  choices = setNames(1:nlyr(data$r), data$band_names),
                  selected = 1)
    } else {
      p("Single-band raster detected")
    }
  })
  
  # Get selected raster (with optional clipping)
  selected_raster <- reactive({
    req(data$r)
    
    # Select band
    if (nlyr(data$r) > 1 && !is.null(input$selected_band)) {
      r <- data$r[[as.numeric(input$selected_band)]]
    } else {
      r <- data$r
    }
    
    # Apply clipping if requested
    if (input$apply_clip && !is.null(data$v)) {
      v_aligned <- sf::st_transform(data$v, terra::crs(r))
      vect_aligned <- terra::vect(v_aligned)
      r <- terra::crop(r, vect_aligned, mask = TRUE)
    }
    
    return(r)
  })
  
  # Helper function for getting palette
  get_palette <- function(palette_name, n = 256) {
    if (palette_name %in% c("viridis", "magma", "inferno", "plasma")) {
      return(viridis::viridis_pal(option = palette_name)(n))
    } else if (palette_name == "rainbow") {
        return(rainbow(n))  
    } else if (palette_name %in% c("terrain.colors", "topo.colors")) {
      return(get(palette_name)(n))
    } else {
      # RColorBrewer palettes
      return(colorRampPalette(brewer.pal(11, palette_name))(n))
    }
  }

  # Helper function for ggplot scale
  get_ggplot_scale <- function(palette_name) {
     if (palette_name %in% c("viridis", "magma", "inferno", "plasma")) {
       scale_fill_viridis_c(option = substr(palette_name, 1, 1))
     } else if (palette_name == "rainbow") {
        scale_fill_gradientn(colors = rainbow(256))
     } else if (palette_name %in% c("terrain.colors", "topo.colors")) {
        scale_fill_gradientn(colors = get(palette_name)(256))
     } else {
       scale_fill_distiller(palette = palette_name)
     }
  }
  
  # Theme helper
  get_theme <- function(theme_name) {
    switch(theme_name,
           "minimal" = theme_minimal(),
           "classic" = theme_classic(),
           "light" = theme_light(),
           "dark" = theme_dark(),
           "void" = theme_void(),
           theme_minimal())
  }

  # Interactive Leaflet Map
  output$map <- renderLeaflet({
    req(selected_raster())
    
    r <- selected_raster()
    
    # Project to Web Mercator for Leaflet
    r_proj <- terra::project(r, "EPSG:3857")
    
    # Create Palette function
    domain <- values(r_proj)
    domain <- domain[!is.na(domain)]
    
    # Color logic
    pal_colors <- get_palette(input$color_palette, 20) # get colors
    pal <- colorNumeric(pal_colors, domain = domain, na.color = "transparent")

    m <- leaflet() %>% 
        addProviderTiles(input$basemap_type)
    
    m <- m %>% addRasterImage(r_proj, 
                              opacity = input$map_opacity, 
                              colors = pal) %>%
               addLegend(pal = pal, values = domain, title = "Value")
    
    # Add shapefile if present
    if (!is.null(data$v)) {
      v_proj <- sf::st_transform(data$v, 4326)
      m <- m %>% addPolygons(data = v_proj, 
                             color = "black", 
                             fill = FALSE, 
                             weight = 2)
    }
    
    # Fit bounds
    ext <- terra::ext(terra::project(r, "EPSG:4326"))
    m <- m %>% fitBounds(ext[1], ext[3], ext[2], ext[4])
    
    m
  })
  
  # Publication Quality Map (Reactive for both display and download)
  pub_plot <- reactive({
    req(selected_raster())
    
    r <- selected_raster()
    r_df <- as.data.frame(r, xy = TRUE)
    colnames(r_df)[3] <- "value"
    
    band_label <- if(!is.null(input$selected_band)) data$band_names[as.numeric(input$selected_band)] else ""
    
    p <- ggplot() +
      geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
      coord_equal() +
      get_theme(input$plot_theme) +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11, face = "bold")
      ) +
      labs(
        title = if(input$plot_title != "") input$plot_title else paste("Band:", band_label),
        x = "Longitude",
        y = "Latitude",
        fill = "Value"
      ) +
      get_ggplot_scale(input$color_palette)
    
    # Add shapefile boundary
    if (!is.null(data$v) && input$apply_clip) {
      v_proj <- sf::st_transform(data$v, terra::crs(r))
      p <- p + geom_sf(data = v_proj, fill = NA, color = "black", size = 1)
    }
    
    return(p)
  })

  output$pub_map <- renderPlot({
    pub_plot()
  })
  
  # Statistics
  output$stats <- renderPrint({
    req(selected_raster())
    r <- selected_raster()
    vals <- values(r, mat = FALSE)
    vals <- vals[!is.na(vals)]
    
    cat("=== Raster Statistics ===\n\n")
    cat("Dimensions:", ncol(r), "x", nrow(r), "(pixels)\n")
    cat("Resolution:", paste0(round(res(r), 4), collapse = ", "), "\n")
    cat("CRS:", as.character(crs(r)), "\n\n")
    cat("Statistics:\n")
    cat("  Min:   ", min(vals), "\n")
    cat("  Max:   ", max(vals), "\n")
    cat("  Mean:  ", mean(vals), "\n")
    cat("  Median:", median(vals), "\n")
    cat("  SD:    ", sd(vals), "\n")
  })
  
  # Histogram
  output$histogram <- renderPlot({
    req(selected_raster())
    r <- selected_raster()
    r_df <- as.data.frame(r, xy = FALSE)
    colnames(r_df)[1] <- "value"
    
    ggplot(r_df, aes(x = value)) +
      geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.8) +
      theme_minimal() +
      labs(title = "Value Distribution", x = "Raster Value", y = "Frequency")
  })
  
  # Download Clipped Raster
  output$download_tif <- downloadHandler(
    filename = function() {
      paste0("geomapper_export_", Sys.Date(), ".tif")
    },
    content = function(file) {
      terra::writeRaster(selected_raster(), file, overwrite = TRUE)
    }
  )
  
  # Download Publication Map
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("geomapper_map_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, pub_plot(), width = 12, height = 8, dpi = input$dpi, bg = "white")
    }
  )
}

# Run the App
shinyApp(ui, server)

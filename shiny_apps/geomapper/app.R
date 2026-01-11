library(shiny)
library(terra)
library(sf)
library(leaflet)
library(ggplot2)
library(viridis)

# Define UI
ui <- fluidPage(
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
      
      # Color Palette Selection
      h4("🎨 Visualization"),
      selectInput("color_palette", "Color Palette:",
                  choices = c("Viridis" = "viridis",
                              "Magma" = "magma",
                              "Inferno" = "inferno",
                              "Plasma" = "plasma",
                              "Spectral" = "Spectral",
                              "RdYlGn" = "RdYlGn",
                              "Terrain" = "terrain.colors",
                              "Topo" = "topo.colors"),
                  selected = "viridis"),
      
      # Clipping Option
      checkboxInput("apply_clip", "Apply Shapefile Clip", value = FALSE),
      
      hr(),
      
      # Download Section
      h4("📥 Export"),
      downloadButton("download_tif", "Download Clipped Raster"),
      br(), br(),
      downloadButton("download_map", "Download Publication Map (PNG)")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Interactive Map", 
                 leafletOutput("map", height = "600px")),
        tabPanel("Publication Map", 
                 plotOutput("pub_map", height = "700px")),
        tabPanel("Statistics",
                 verbatimTextOutput("stats"))
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
    r <- terra::rast(input$tif_file$datapath)
    data$r <- r
    data$band_names <- names(r)
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
  
  # Interactive Leaflet Map
  output$map <- renderLeaflet({
    req(selected_raster())
    
    r <- selected_raster()
    m <- leaflet() %>% addTiles()
    
    # Project to Web Mercator
    r_proj <- terra::project(r, "EPSG:3857")
    
    # Get color palette
    pal_func <- switch(input$color_palette,
                       "viridis" = viridis::viridis,
                       "magma" = viridis::magma,
                       "inferno" = viridis::inferno,
                       "plasma" = viridis::plasma,
                       function(n) terrain.colors(n))
    
    m <- m %>% addRasterImage(r_proj, 
                              opacity = 0.8, 
                              colors = pal_func(256))
    
    # Add shapefile if present
    if (!is.null(data$v)) {
      v_proj <- sf::st_transform(data$v, 4326)
      m <- m %>% addPolygons(data = v_proj, 
                             color = "black", 
                             fill = FALSE, 
                             weight = 2)
    }
    
    # Center map
    ext <- terra::ext(terra::project(r, "EPSG:4326"))
    m <- m %>% fitBounds(ext[1], ext[3], ext[2], ext[4])
    
    m
  })
  
  # Publication Quality Map
  output$pub_map <- renderPlot({
    req(selected_raster())
    
    r <- selected_raster()
    r_df <- as.data.frame(r, xy = TRUE)
    colnames(r_df)[3] <- "value"
    
    p <- ggplot() +
      geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
      coord_equal() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11),
        panel.grid = element_blank()
      ) +
      labs(
        title = paste("GeoMapper Visualization -", 
                      if(!is.null(input$selected_band)) data$band_names[as.numeric(input$selected_band)] else ""),
        x = "Longitude",
        y = "Latitude",
        fill = "Value"
      )
    
    # Apply color palette
    if (input$color_palette %in% c("viridis", "magma", "inferno", "plasma")) {
      p <- p + scale_fill_viridis_c(option = substr(input$color_palette, 1, 1))
    } else if (input$color_palette == "Spectral") {
      p <- p + scale_fill_distiller(palette = "Spectral")
    } else if (input$color_palette == "RdYlGn") {
      p <- p + scale_fill_distiller(palette = "RdYlGn")
    }
    
    # Add shapefile boundary
    if (!is.null(data$v) && input$apply_clip) {
      v_proj <- sf::st_transform(data$v, terra::crs(r))
      p <- p + geom_sf(data = v_proj, fill = NA, color = "black", size = 1)
    }
    
    p
  })
  
  # Statistics
  output$stats <- renderPrint({
    req(selected_raster())
    r <- selected_raster()
    
    cat("=== Raster Statistics ===\n\n")
    cat("Dimensions:", dim(r)[1], "x", dim(r)[2], "\n")
    cat("Resolution:", res(r), "\n")
    cat("CRS:", as.character(crs(r)), "\n\n")
    cat("Value Range:\n")
    cat("  Min:", min(values(r), na.rm = TRUE), "\n")
    cat("  Max:", max(values(r), na.rm = TRUE), "\n")
    cat("  Mean:", mean(values(r), na.rm = TRUE), "\n")
    cat("  SD:", sd(values(r), na.rm = TRUE), "\n")
  })
  
  # Download Clipped Raster
  output$download_tif <- downloadHandler(
    filename = function() {
      paste0("geomapper_", Sys.Date(), ".tif")
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
      req(selected_raster())
      
      r <- selected_raster()
      r_df <- as.data.frame(r, xy = TRUE)
      colnames(r_df)[3] <- "value"
      
      p <- ggplot() +
        geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
        coord_equal() +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          legend.title = element_text(size = 11),
          panel.grid = element_blank()
        ) +
        labs(
          title = paste("GeoMapper Visualization -", 
                        if(!is.null(input$selected_band)) data$band_names[as.numeric(input$selected_band)] else ""),
          x = "Longitude",
          y = "Latitude",
          fill = "Value"
        )
      
      if (input$color_palette %in% c("viridis", "magma", "inferno", "plasma")) {
        p <- p + scale_fill_viridis_c(option = substr(input$color_palette, 1, 1))
      } else if (input$color_palette == "Spectral") {
        p <- p + scale_fill_distiller(palette = "Spectral")
      } else if (input$color_palette == "RdYlGn") {
        p <- p + scale_fill_distiller(palette = "RdYlGn")
      }
      
      if (!is.null(data$v) && input$apply_clip) {
        v_proj <- sf::st_transform(data$v, terra::crs(r))
        p <- p + geom_sf(data = v_proj, fill = NA, color = "black", size = 1)
      }
      
      ggsave(file, p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
}

# Run the App
shinyApp(ui, server)

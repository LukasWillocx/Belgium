# ==============================================================================
# BELGIUM - MAIN APP
# ==============================================================================

library(shiny)
library(leaflet)
library(shinyjs)
library(markdown)

# Source external files
source("database_functions.R")

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  useShinyjs(),
  
  # Include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=6"),
    tags$style(HTML("
      .about-modal-content img {
        max-width: 100%;
        height: auto;
        display: block;
        margin: 15px auto;
      }
      .about-modal-content {
        line-height: 1.6;
      }
      .about-modal-content h1 {
        margin-top: 0;
      }
      .about-modal-content h2 {
        margin-top: 20px;
        border-bottom: 1px solid #e0e0e0;
        padding-bottom: 5px;
      }
    "))
  ),
  
  tags$button(class = "dark-mode-toggle", id = "darkModeToggle", "🌙"),
  
  # Left sidebar toggle button
  tags$div(class = "sidebar-toggle hidden", id = "sidebarToggle", "☰"),
  
  # Right metrics toggle button
  tags$div(class = "metrics-toggle hidden", id = "metricsToggle", "📊"),
  
  # Left Panel
  tags$div(
    class = "left-panel",
    id = "leftPanel",
    tags$button(class = "close-sidebar", id = "closeLeftPanel", "×"),
    tags$div(class = "panel-title", "Location Selection"),
    
    tags$div(
      class = "filter-group",
      selectInput("region", "Region", choices = NULL)
    ),
    
    shinyjs::hidden(
      tags$div(
        class = "filter-group",
        id = "provinceGroup",
        selectInput("province", "Province", choices = NULL)
      )
    ),
    
    shinyjs::hidden(
      tags$div(
        class = "filter-group",
        id = "municipalityGroup",
        selectInput("municipality", "Municipality", choices = NULL)
      )
    ),
    
    tags$div(
      class = "filter-group",
      actionButton("reset", "Reset Selection", class = "btn-secondary btn-block")
    ),
    
    tags$div(
      class = "filter-group",
      actionButton("aboutBtn", "About", class = "btn-info btn-block", icon = icon("info-circle"))
    )
  ),
  
  # Right Panel
  tags$div(
    class = "right-panel",
    id = "rightPanel",
    tags$button(class = "close-sidebar", id = "closeRightPanel", "×"),
    tags$div(class = "panel-title", "Information"),
    
    conditionalPanel(
      condition = "output.show_metrics",
      uiOutput("metrics_display")
    )
  ),
  
  # Map
  leafletOutput("map", width = "100%", height = "100vh"),
  
  # JavaScript
  tags$script(HTML("
    $(document).ready(function() {
      // Function to remove legend background
      function removeLegendBackground() {
        $('.info.legend, .leaflet-control.legend, div.legend').each(function() {
          $(this).css({
            'background': 'transparent !important',
            'background-color': 'transparent !important',
            'border': 'none !important',
            'box-shadow': 'none !important'
          });
          $(this).attr('style', $(this).attr('style') + '; background: transparent !important; background-color: transparent !important; border: none !important; box-shadow: none !important;');
        });
      }
      
      // Dark mode toggle
      let darkMode = false;
      $('#darkModeToggle').click(function() {
        darkMode = !darkMode;
        $('body').toggleClass('dark-mode');
        Shiny.setInputValue('darkModeState', darkMode);
        
        // Force legend transparency after dark mode toggle
        setTimeout(removeLegendBackground, 50);
        setTimeout(removeLegendBackground, 200);
        setTimeout(removeLegendBackground, 500);
      });
      
      // Force legend transparency on load and periodically
      setTimeout(removeLegendBackground, 100);
      setTimeout(removeLegendBackground, 500);
      setTimeout(removeLegendBackground, 1000);
      setTimeout(removeLegendBackground, 2000);
      
      // Monitor for legend changes
      setInterval(removeLegendBackground, 1000);
      
      // Left panel controls
      $('#closeLeftPanel').click(function() {
        $('#leftPanel').addClass('closed');
        $('#sidebarToggle').removeClass('hidden');
      });
      
      $('#sidebarToggle').click(function() {
        $('#leftPanel').removeClass('closed');
        $('#sidebarToggle').addClass('hidden');
      });
      
      // Right panel controls
      $('#closeRightPanel').click(function() {
        $('#rightPanel').addClass('closed');
        $('#metricsToggle').removeClass('hidden');
      });
      
      $('#metricsToggle').click(function() {
        $('#rightPanel').removeClass('closed');
        $('#metricsToggle').addClass('hidden');
      });
      
      // Handle slider change - send to Shiny when value changes
      $(document).on('change', '#topNamesSlider', function() {
        Shiny.setInputValue('topNamesCount', parseInt($(this).val()));
      });
      
      // Force solid slider background and override any gradients
      function forceSliderSolidBackground() {
        $('#topNamesSlider').css({
          'background': '#e0e0e0',
          'background-image': 'none'
        });
        if ($('body').hasClass('dark-mode')) {
          $('#topNamesSlider').css({
            'background': '#2a2a2a',
            'background-image': 'none'
          });
        }
      }
      
      // Initialize slider when it appears
      var initSliderInterval = setInterval(function() {
        var slider = $('#topNamesSlider');
        if (slider.length) {
          forceSliderSolidBackground();
          clearInterval(initSliderInterval);
        }
      }, 100);
      
      // Re-apply on dark mode toggle
      $('#darkModeToggle').on('click', function() {
        setTimeout(forceSliderSolidBackground, 50);
      });
      
      // Monitor for any changes to slider
      setInterval(forceSliderSolidBackground, 500);
    });
  "))
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  conn <- get_db_connection()
  onStop(function() { dbDisconnect(conn) })
  
  # Cache for municipality data to speed up loading
  municipalities_cache <- reactiveVal(NULL)
  
  selected <- reactiveValues(
    level = "belgium",  # Start with Belgium view
    id = NULL,
    name = NULL,
    region_code = NULL
  )
  
  darkMode <- reactiveVal(FALSE)
  topNamesCount <- reactiveVal(5)
  
  observeEvent(input$darkModeState, {
    darkMode(input$darkModeState)
  })
  
  observeEvent(input$topNamesCount, {
    topNamesCount(input$topNamesCount)
  })
  
  # ===========================================================================
  # LOAD AND CACHE ALL MUNICIPALITIES ON STARTUP
  # ===========================================================================
  
  observe({
    if (is.null(municipalities_cache())) {
      municipalities_cache(get_municipalities_with_metrics(conn))
    }
  })
  
  # ===========================================================================
  # INITIALIZE REGION DROPDOWN
  # ===========================================================================
  
  observe({
    regions <- get_regions(conn)
    # Capitalize region names properly
    regions$name <- sapply(regions$name, capitalize_first)
    choices <- c("Select Region" = "", setNames(regions$id, regions$name))
    updateSelectInput(session, "region", choices = choices, selected = "")
  })
  
  # ===========================================================================
  # REGION SELECTION HANDLER
  # ===========================================================================
  
  observeEvent(input$region, {
    if (input$region == "") {
      shinyjs::hide("provinceGroup")
      shinyjs::hide("municipalityGroup")
      updateSelectInput(session, "province", choices = c("Select Province" = ""), selected = "")
      updateSelectInput(session, "municipality", choices = c("Select Municipality" = ""), selected = "")
      selected$level <- "belgium"  # Reset to Belgium view
      selected$id <- NULL
      selected$region_code <- NULL
      
      # Use cached municipalities when deselecting
      municipalities_geom <- municipalities_cache()
      if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
        leafletProxy("map") %>%
          render_municipalities_choropleth(municipalities_geom) %>%
          setView(lng = 4.5, lat = 50.5, zoom = 8)
      }
      
    } else {
      region_data <- dbGetQuery(conn, sprintf(
        "SELECT code FROM regions WHERE id = %d", as.integer(input$region)
      ))
      selected$region_code <- region_data$code[1]
      
      # Show choropleth map of all municipalities in region
      municipalities_geom <- get_municipalities_with_metrics(conn, region_id = input$region)
      
      if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
        bbox <- st_bbox(municipalities_geom)
        leafletProxy("map") %>%
          render_municipalities_choropleth(municipalities_geom, bbox, fit_bounds = TRUE)
      }
      
      if (selected$region_code == "BRU") {
        # Brussels: Skip province, show municipalities
        shinyjs::hide("provinceGroup")
        shinyjs::show("municipalityGroup")
        
        municipalities <- get_municipalities(conn, region_id = input$region)
        choices <- c("Select Municipality" = "", setNames(municipalities$id, municipalities$name))
        updateSelectInput(session, "municipality", choices = choices, selected = "")
      } else {
        # Flanders/Wallonia: Show provinces
        shinyjs::show("provinceGroup")
        shinyjs::hide("municipalityGroup")
        
        provinces <- get_provinces(conn, input$region)
        provinces$name <- gsub("^Provincie ", "", provinces$name)
        choices <- c("Select Province" = "", setNames(provinces$id, provinces$name))
        updateSelectInput(session, "province", choices = choices, selected = "")
        updateSelectInput(session, "municipality", choices = c("Select Municipality" = ""), selected = "")
      }
      
      selected$level <- "region"
      selected$id <- input$region
    }
  })
  
  # ===========================================================================
  # PROVINCE SELECTION HANDLER
  # ===========================================================================
  
  observeEvent(input$province, {
    if (input$province != "") {
      shinyjs::show("municipalityGroup")
      
      municipalities <- get_municipalities(conn, province_id = input$province)
      choices <- c("Select Municipality" = "", setNames(municipalities$id, municipalities$name))
      updateSelectInput(session, "municipality", choices = choices, selected = "")
      
      # Show choropleth map of all municipalities in province
      municipalities_geom <- get_municipalities_with_metrics(conn, province_id = input$province)
      
      if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
        bbox <- st_bbox(municipalities_geom)
        leafletProxy("map") %>%
          render_municipalities_choropleth(municipalities_geom, bbox, fit_bounds = TRUE)
      }
      
      selected$level <- "province"
      selected$id <- input$province
    }
  })
  
  # ===========================================================================
  # MUNICIPALITY SELECTION HANDLER
  # ===========================================================================
  
  observeEvent(input$municipality, {
    if (input$municipality != "") {
      # Get the specific municipality with its density metric
      query <- sprintf(
        "SELECT m.id, m.name, 
              ST_AsText(m.geometry) as wkt, 
              mv.value
       FROM municipalities m
       LEFT JOIN metric_values mv ON mv.municipality_id = m.id
       LEFT JOIN metric_definitions md ON mv.metric_id = md.id
       WHERE m.id = %d AND (md.metric_key = 'population_density' OR md.metric_key IS NULL)
       LIMIT 1",
        as.integer(input$municipality)
      )
      
      result <- dbGetQuery(conn, query)
      
      if (nrow(result) > 0) {
        geom <- st_as_sfc(result$wkt, crs = 4326)
        municipality_geom <- st_sf(
          id = result$id,
          name = result$name,
          value = result$value,
          geometry = geom
        )
        
        pal <- get_density_palette()
        bbox <- st_bbox(municipality_geom)
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(
            data = municipality_geom,
            fillColor = ~pal(log10(pmax(value, 1))),
            fillOpacity = 0.7,
            color = "white",
            weight = 3,
            label = ~paste0(name, ": ", round(value, 1), " people/km²")
          ) %>%
          fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
        
        selected$level <- "municipality"
        selected$id <- input$municipality
        selected$name <- geom$name[1]
      }
    }
  })
  
  # ===========================================================================
  # RESET BUTTON
  # ===========================================================================
  
  observeEvent(input$reset, {
    updateSelectInput(session, "region", selected = "")
    updateSelectInput(session, "province", selected = "")
    updateSelectInput(session, "municipality", selected = "")
    shinyjs::hide("provinceGroup")
    shinyjs::hide("municipalityGroup")
    
    # Use cached municipalities
    municipalities_geom <- municipalities_cache()
    if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
      leafletProxy("map") %>%
        render_municipalities_choropleth(municipalities_geom) %>%
        setView(lng = 4.5, lat = 50.5, zoom = 8)
    }
    
    selected$level <- "belgium"  # Reset to Belgium view
    selected$id <- NULL
    selected$name <- NULL
    selected$region_code <- NULL
  })
  
  # ===========================================================================
  # ABOUT MODAL
  # ===========================================================================
  
  observeEvent(input$aboutBtn, {
    # Read the markdown file
    about_content <- tryCatch({
      if (file.exists("about.md")) {
        markdownToHTML(file = "about.md", fragment.only = TRUE)
      } else {
        "<p>About content not found. Please create an <code>about.md</code> file.</p>"
      }
    }, error = function(e) {
      paste("<p>Error loading about content:</p><pre>", e$message, "</pre>")
    })
    
    showModal(modalDialog(
      title = "About This Application",
      tags$div(class = "about-modal-content", HTML(about_content)),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  })
  
  # ===========================================================================
  # MAP RENDERING
  # ===========================================================================
  
  output$map <- renderLeaflet({
    # Load all municipalities on startup and cache them
    municipalities_geom <- get_municipalities_with_metrics(conn)
    municipalities_cache(municipalities_geom)
    
    # Use log scale (base 10) for better visualization of density hotspots
    pal <- get_density_palette()
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 4.5, lat = 50.5, zoom = 8)
    
    # Add legend with log scale labels
    map <- map %>%
      addLegend(
        position = "bottomright",
        colors = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"),
        labels = c("1", "10", "50", "100", "500", "1,000", "5,000", "10,000", "24,000"),
        title = "Population Density<br/>(people/km²)<br/><small>Log scale</small>",
        opacity = 0.7,
        className = "legend",
        layerId = "legend"
      )
    
    if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
      map <- map %>%
        addPolygons(
          data = municipalities_geom,
          fillColor = ~pal(log10(pmax(value, 1))),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          label = ~paste0(name, ": ", round(value, 1), " people/km²")
        )
    }
    
    map
  })
  
  # Dark mode map tiles
  observeEvent(darkMode(), {
    if (darkMode()) {
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles(providers$CartoDB.DarkMatter)
    } else {
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles(providers$CartoDB.Positron)
    }
  })
  
  # ===========================================================================
  # METRICS PANEL
  # ===========================================================================
  
  output$show_metrics <- reactive({
    !is.null(selected$level)
  })
  outputOptions(output, "show_metrics", suspendWhenHidden = FALSE)
  
  output$metrics_display <- renderUI({
    req(selected$level)
    
    # Get the current top N value
    top_n <- topNamesCount()
    
    if (selected$level == "belgium") {
      metric_data <- get_belgium_metric(conn)
      name_data <- get_belgium_top_names(conn, top_n = top_n)
      
      tagList(
        render_density_section(metric_data, show_min_max = TRUE),
        render_names_section(name_data, top_n, "total_frequency"),
        render_belgium_info(metric_data)
      )
      
    } else if (selected$level == "region") {
      metric_data <- get_region_metric(conn, selected$id)
      name_data <- get_region_top_names(conn, selected$id, top_n = top_n)
      
      tagList(
        render_density_section(metric_data, show_min_max = TRUE),
        render_names_section(name_data, top_n, "total_frequency"),
        render_region_info(metric_data)
      )
      
    } else if (selected$level == "province") {
      metric_data <- get_province_metric(conn, selected$id)
      name_data <- get_province_top_names(conn, selected$id, top_n = top_n)
      
      tagList(
        render_density_section(metric_data, show_min_max = TRUE),
        render_names_section(name_data, top_n, "total_frequency"),
        render_province_info(metric_data)
      )
      
    } else if (selected$level == "municipality") {
      metric_data <- get_municipality_metric(conn, selected$id)
      name_data <- get_municipality_top_names(conn, selected$id, top_n = top_n)
      
      tagList(
        render_density_section(metric_data, show_min_max = FALSE),
        render_names_section(name_data, top_n, "frequency"),
        render_municipality_info(metric_data)
      )
    }
  })
}

shinyApp(ui, server)
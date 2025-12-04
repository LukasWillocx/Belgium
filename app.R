# ==============================================================================
# BELGIUM LIVING CONDITIONS MONITOR - MAIN APP
# ==============================================================================

library(shiny)
library(leaflet)
library(shinyjs)

# Source external files
source("database_functions.R")

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  useShinyjs(),
  
  # Include custom CSS using htmlDependency
  htmltools::htmlDependency(
    name = "custom-styles",
    version = "1.0",
    src = "www",
    stylesheet = "styles.css"
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
    )
  ),
  
  # Right Panel
  conditionalPanel(
    condition = "output.show_metrics",
    tags$div(
      class = "right-panel",
      id = "rightPanel",
      tags$button(class = "close-sidebar", id = "closeRightPanel", "×"),
      tags$div(class = "panel-title", "Metrics"),
      uiOutput("metrics_display")
    )
  ),
  
  # Map
  leafletOutput("map", width = "100%", height = "100vh"),
  
  # JavaScript
  tags$script(HTML("
    $(document).ready(function() {
      // Dark mode toggle
      let darkMode = false;
      $('#darkModeToggle').click(function() {
        darkMode = !darkMode;
        $('body').toggleClass('dark-mode');
        Shiny.setInputValue('darkModeState', darkMode);
      });
      
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
    });
  "))
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  conn <- get_db_connection()
  onStop(function() { dbDisconnect(conn) })
  
  selected <- reactiveValues(
    level = NULL,
    id = NULL,
    name = NULL,
    region_code = NULL
  )
  
  darkMode <- reactiveVal(FALSE)
  
  observeEvent(input$darkModeState, {
    darkMode(input$darkModeState)
  })
  
  # ===========================================================================
  # INITIALIZE REGION DROPDOWN
  # ===========================================================================
  
  observe({
    regions <- get_regions(conn)
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
      selected$level <- NULL
      selected$id <- NULL
      selected$region_code <- NULL
      
      leafletProxy("map") %>%
        clearShapes() %>%
        setView(lng = 4.5, lat = 50.5, zoom = 8)
      
    } else {
      region_data <- dbGetQuery(conn, sprintf(
        "SELECT code FROM regions WHERE id = %d", as.integer(input$region)
      ))
      selected$region_code <- region_data$code[1]
      
      # Show choropleth map of all municipalities in region
      municipalities_geom <- get_municipalities_with_metrics(conn, region_id = input$region)
      
      if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = municipalities_geom$value,
          na.color = "#808080"
        )
        
        bbox <- st_bbox(municipalities_geom)
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(
            data = municipalities_geom,
            fillColor = ~pal(value),
            fillOpacity = 0.7,
            color = "white",
            weight = 1,
            label = ~paste0(name, ": ", round(value, 1), " people/km²")
          ) %>%
          fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
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
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = municipalities_geom$value,
          na.color = "#808080"
        )
        
        bbox <- st_bbox(municipalities_geom)
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(
            data = municipalities_geom,
            fillColor = ~pal(value),
            fillOpacity = 0.7,
            color = "white",
            weight = 1,
            label = ~paste0(name, ": ", round(value, 1), " people/km²")
          ) %>%
          fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
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
      geom <- get_area_geometry(conn, "municipality", input$municipality)
      if (!is.null(geom)) {
        bbox <- st_bbox(geom)
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(
            data = geom,
            fillColor = "#e74c3c",
            fillOpacity = 0.5,
            color = "#c0392b",
            weight = 3,
            label = ~name
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
    
    leafletProxy("map") %>%
      clearShapes() %>%
      setView(lng = 4.5, lat = 50.5, zoom = 8)
    
    selected$level <- NULL
    selected$id <- NULL
    selected$name <- NULL
    selected$region_code <- NULL
  })
  
  # ===========================================================================
  # MAP RENDERING
  # ===========================================================================
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 4.5, lat = 50.5, zoom = 8)
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
    req(selected$level, selected$id)
    
    if (selected$level == "region") {
      metric_data <- get_region_metric(conn, selected$id)
      
      if (nrow(metric_data) > 0) {
        tagList(
          tags$div(
            class = "metric-card",
            tags$div(class = "metric-value", sprintf("%.1f", metric_data$value[1])),
            tags$div(class = "metric-label", paste0("Average ", metric_data$unit[1]))
          ),
          tags$div(
            class = "stats-grid",
            tags$div(
              class = "stat-item",
              tags$div(class = "stat-value", sprintf("%.1f", metric_data$min_value[1])),
              tags$div(class = "stat-label", "Minimum")
            ),
            tags$div(
              class = "stat-item",
              tags$div(class = "stat-value", sprintf("%.1f", metric_data$max_value[1])),
              tags$div(class = "stat-label", "Maximum")
            )
          ),
          tags$div(
            class = "info-text",
            tags$strong("Region: "), metric_data$region_name[1], tags$br(),
            tags$strong("Municipalities: "), metric_data$municipality_count[1], tags$br(),
            tags$strong("Date: "), as.character(metric_data$date[1])
          )
        )
      } else {
        tags$p("No data available", class = "info-text")
      }
      
    } else if (selected$level == "province") {
      metric_data <- get_province_metric(conn, selected$id)
      
      if (nrow(metric_data) > 0) {
        tagList(
          tags$div(
            class = "metric-card",
            tags$div(class = "metric-value", sprintf("%.1f", metric_data$value[1])),
            tags$div(class = "metric-label", paste0("Average ", metric_data$unit[1]))
          ),
          tags$div(
            class = "stats-grid",
            tags$div(
              class = "stat-item",
              tags$div(class = "stat-value", sprintf("%.1f", metric_data$min_value[1])),
              tags$div(class = "stat-label", "Minimum")
            ),
            tags$div(
              class = "stat-item",
              tags$div(class = "stat-value", sprintf("%.1f", metric_data$max_value[1])),
              tags$div(class = "stat-label", "Maximum")
            )
          ),
          tags$div(
            class = "info-text",
            tags$strong("Province: "), metric_data$province_name[1], tags$br(),
            tags$strong("Municipalities: "), metric_data$municipality_count[1], tags$br(),
            tags$strong("Date: "), as.character(metric_data$date[1])
          )
        )
      } else {
        tags$p("No data available", class = "info-text")
      }
      
    } else if (selected$level == "municipality") {
      metric_data <- get_municipality_metric(conn, selected$id)
      
      if (nrow(metric_data) > 0) {
        tagList(
          tags$div(
            class = "metric-card",
            tags$div(class = "metric-value", sprintf("%.1f", metric_data$value[1])),
            tags$div(class = "metric-label", paste0(metric_data$unit[1]))
          ),
          tags$div(
            class = "info-text",
            tags$strong("Location: "), metric_data$municipality_name[1], tags$br(),
            tags$strong("Metric: "), metric_data$metric_name[1], tags$br(),
            tags$strong("Date: "), as.character(metric_data$date[1])
          )
        )
      } else {
        tags$p("No data available", class = "info-text")
      }
    }
  })
}

shinyApp(ui, server)
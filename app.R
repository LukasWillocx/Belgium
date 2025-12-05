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
  
  # Cache for municipality data to speed up loading
  municipalities_cache <- reactiveVal(NULL)
  
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
      
      # Use cached municipalities when deselecting
      municipalities_geom <- municipalities_cache()
      
      if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
        # Use log scale for better hotspot visualization
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = c(log10(1), log10(24000)),
          na.color = "#808080"
        )
        
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(
            data = municipalities_geom,
            fillColor = ~pal(log10(pmax(value, 1))),
            fillOpacity = 0.7,
            color = "white",
            weight = 1,
            label = ~paste0(name, ": ", round(value, 1), " people/km²")
          ) %>%
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
        # Use log scale for better hotspot visualization
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = c(log10(1), log10(24000)),
          na.color = "#808080"
        )
        
        bbox <- st_bbox(municipalities_geom)
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(
            data = municipalities_geom,
            fillColor = ~pal(log10(pmax(value, 1))),
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
        # Use log scale for better hotspot visualization
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = c(log10(1), log10(24000)),
          na.color = "#808080"
        )
        
        bbox <- st_bbox(municipalities_geom)
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(
            data = municipalities_geom,
            fillColor = ~pal(log10(pmax(value, 1))),
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
    
    # Use cached municipalities
    municipalities_geom <- municipalities_cache()
    
    if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
      # Use log scale for better hotspot visualization
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = c(log10(1), log10(24000)),
        na.color = "#808080"
      )
      
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(
          data = municipalities_geom,
          fillColor = ~pal(log10(pmax(value, 1))),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          label = ~paste0(name, ": ", round(value, 1), " people/km²")
        ) %>%
        setView(lng = 4.5, lat = 50.5, zoom = 8)
    }
    
    selected$level <- NULL
    selected$id <- NULL
    selected$name <- NULL
    selected$region_code <- NULL
  })
  
  # ===========================================================================
  # MAP RENDERING
  # ===========================================================================
  
  output$map <- renderLeaflet({
    # Load all municipalities on startup and cache them
    municipalities_geom <- get_municipalities_with_metrics(conn)
    municipalities_cache(municipalities_geom)
    
    # Use log scale (base 10) for better visualization of density hotspots
    # This prevents Brussels from washing out other areas
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = c(log10(1), log10(24000)),
      na.color = "#808080"
    )
    
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
        opacity = 0.7
      )
    
    if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
      map <- map %>%
        addPolygons(
          data = municipalities_geom,
          fillColor = ~pal(log10(pmax(value, 1))),  # pmax ensures minimum value of 1 for log
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
    req(selected$level, selected$id)
    
    if (selected$level == "region") {
      metric_data <- get_region_metric(conn, selected$id)
      name_data <- get_region_top_names(conn, selected$id, top_n = 5)
      diversity_data <- get_region_name_diversity(conn, selected$id)
      
      tagList(
        # Population Density
        if (nrow(metric_data) > 0) {
          tagList(
            tags$h4("Population Density", style = "margin-top: 0;"),
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
            tags$hr()
          )
        },
        
        # Top Names
        if (nrow(name_data) > 0) {
          male_names <- name_data[name_data$gender == 'M', ]
          female_names <- name_data[name_data$gender == 'F', ]
          
          tagList(
            tags$h4("Most Popular Names (2025)"),
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
              # Male names
              tags$div(
                tags$h5("👨 Male", style = "margin-top: 0; color: #3498db;"),
                tags$ol(
                  style = "margin: 0; padding-left: 20px;",
                  lapply(1:nrow(male_names), function(i) {
                    tags$li(
                      tags$strong(male_names$name[i]),
                      tags$br(),
                      tags$small(
                        sprintf("%s people", format(male_names$total_frequency[i], big.mark = ","))
                      )
                    )
                  })
                )
              ),
              # Female names
              tags$div(
                tags$h5("👩 Female", style = "margin-top: 0; color: #e74c3c;"),
                tags$ol(
                  style = "margin: 0; padding-left: 20px;",
                  lapply(1:nrow(female_names), function(i) {
                    tags$li(
                      tags$strong(female_names$name[i]),
                      tags$br(),
                      tags$small(
                        sprintf("%s people", format(female_names$total_frequency[i], big.mark = ","))
                      )
                    )
                  })
                )
              )
            ),
            tags$hr()
          )
        },
        
        # Name Diversity
        if (nrow(diversity_data) > 0) {
          tagList(
            tags$h4("Name Diversity"),
            tags$div(
              class = "stats-grid",
              tags$div(
                class = "stat-item",
                tags$div(class = "stat-value", format(diversity_data$unique_names[diversity_data$gender == 'M'], big.mark = ",")),
                tags$div(class = "stat-label", "Unique Male Names")
              ),
              tags$div(
                class = "stat-item",
                tags$div(class = "stat-value", format(diversity_data$unique_names[diversity_data$gender == 'F'], big.mark = ",")),
                tags$div(class = "stat-label", "Unique Female Names")
              )
            )
          )
        },
        
        # Region Info
        tags$div(
          class = "info-text",
          style = "margin-top: 15px;",
          tags$strong("Region: "), if(nrow(metric_data) > 0) metric_data$region_name[1] else "",
          tags$br(),
          tags$strong("Municipalities: "), if(nrow(metric_data) > 0) metric_data$municipality_count[1] else ""
        )
      )
      
    } else if (selected$level == "province") {
      metric_data <- get_province_metric(conn, selected$id)
      name_data <- get_province_top_names(conn, selected$id, top_n = 5)
      diversity_data <- get_province_name_diversity(conn, selected$id)
      
      tagList(
        # Population Density
        if (nrow(metric_data) > 0) {
          tagList(
            tags$h4("Population Density", style = "margin-top: 0;"),
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
            tags$hr()
          )
        },
        
        # Top Names
        if (nrow(name_data) > 0) {
          male_names <- name_data[name_data$gender == 'M', ]
          female_names <- name_data[name_data$gender == 'F', ]
          
          tagList(
            tags$h4("Most Popular Names (2025)"),
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
              # Male names
              tags$div(
                tags$h5("👨 Male", style = "margin-top: 0; color: #3498db;"),
                tags$ol(
                  style = "margin: 0; padding-left: 20px;",
                  lapply(1:nrow(male_names), function(i) {
                    tags$li(
                      tags$strong(male_names$name[i]),
                      tags$br(),
                      tags$small(
                        sprintf("%s people", format(male_names$total_frequency[i], big.mark = ","))
                      )
                    )
                  })
                )
              ),
              # Female names
              tags$div(
                tags$h5("👩 Female", style = "margin-top: 0; color: #e74c3c;"),
                tags$ol(
                  style = "margin: 0; padding-left: 20px;",
                  lapply(1:nrow(female_names), function(i) {
                    tags$li(
                      tags$strong(female_names$name[i]),
                      tags$br(),
                      tags$small(
                        sprintf("%s people", format(female_names$total_frequency[i], big.mark = ","))
                      )
                    )
                  })
                )
              )
            ),
            tags$hr()
          )
        },
        
        # Name Diversity
        if (nrow(diversity_data) > 0) {
          tagList(
            tags$h4("Name Diversity"),
            tags$div(
              class = "stats-grid",
              tags$div(
                class = "stat-item",
                tags$div(class = "stat-value", format(diversity_data$unique_names[diversity_data$gender == 'M'], big.mark = ",")),
                tags$div(class = "stat-label", "Unique Male Names")
              ),
              tags$div(
                class = "stat-item",
                tags$div(class = "stat-value", format(diversity_data$unique_names[diversity_data$gender == 'F'], big.mark = ",")),
                tags$div(class = "stat-label", "Unique Female Names")
              )
            )
          )
        },
        
        # Province Info
        tags$div(
          class = "info-text",
          style = "margin-top: 15px;",
          tags$strong("Province: "), if(nrow(metric_data) > 0) metric_data$province_name[1] else "",
          tags$br(),
          tags$strong("Municipalities: "), if(nrow(metric_data) > 0) metric_data$municipality_count[1] else ""
        )
      )
      
    } else if (selected$level == "municipality") {
      metric_data <- get_municipality_metric(conn, selected$id)
      name_data <- get_municipality_top_names(conn, selected$id, top_n = 5)
      diversity_data <- get_municipality_name_diversity(conn, selected$id)
      
      tagList(
        # Population Density
        if (nrow(metric_data) > 0) {
          tagList(
            tags$h4("Population Density", style = "margin-top: 0;"),
            tags$div(
              class = "metric-card",
              tags$div(class = "metric-value", sprintf("%.1f", metric_data$value[1])),
              tags$div(class = "metric-label", paste0(metric_data$unit[1]))
            ),
            tags$hr()
          )
        },
        
        # Top Names
        if (nrow(name_data) > 0) {
          male_names <- name_data[name_data$gender == 'M', ]
          female_names <- name_data[name_data$gender == 'F', ]
          
          tagList(
            tags$h4("Most Popular Names (2025)"),
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
              # Male names
              tags$div(
                tags$h5("👨 Male", style = "margin-top: 0; color: #3498db;"),
                tags$ol(
                  style = "margin: 0; padding-left: 20px;",
                  lapply(1:nrow(male_names), function(i) {
                    tags$li(
                      tags$strong(male_names$name[i]),
                      tags$br(),
                      tags$small(sprintf("%d people", male_names$frequency[i]))
                    )
                  })
                )
              ),
              # Female names
              tags$div(
                tags$h5("👩 Female", style = "margin-top: 0; color: #e74c3c;"),
                tags$ol(
                  style = "margin: 0; padding-left: 20px;",
                  lapply(1:nrow(female_names), function(i) {
                    tags$li(
                      tags$strong(female_names$name[i]),
                      tags$br(),
                      tags$small(sprintf("%d people", female_names$frequency[i]))
                    )
                  })
                )
              )
            ),
            tags$hr()
          )
        },
        
        # Name Diversity
        if (nrow(diversity_data) > 0) {
          tagList(
            tags$h4("Name Diversity"),
            tags$div(
              class = "stats-grid",
              tags$div(
                class = "stat-item",
                tags$div(class = "stat-value", diversity_data$unique_names[diversity_data$gender == 'M']),
                tags$div(class = "stat-label", "Unique Male Names")
              ),
              tags$div(
                class = "stat-item",
                tags$div(class = "stat-value", diversity_data$unique_names[diversity_data$gender == 'F']),
                tags$div(class = "stat-label", "Unique Female Names")
              )
            ),
            tags$div(
              class = "stats-grid",
              tags$div(
                class = "stat-item",
                tags$div(class = "stat-value", sprintf("%.1f%%", diversity_data$top_name_percentage[diversity_data$gender == 'M'])),
                tags$div(class = "stat-label", "Top Male Name %")
              ),
              tags$div(
                class = "stat-item",
                tags$div(class = "stat-value", sprintf("%.1f%%", diversity_data$top_name_percentage[diversity_data$gender == 'F'])),
                tags$div(class = "stat-label", "Top Female Name %")
              )
            )
          )
        },
        
        # Municipality Info
        tags$div(
          class = "info-text",
          style = "margin-top: 15px;",
          tags$strong("Location: "), if(nrow(metric_data) > 0) metric_data$municipality_name[1] else ""
        )
      )
    }
  })
}

shinyApp(ui, server)
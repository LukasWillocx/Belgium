# ==============================================================================
# DATABASE FUNCTIONS
# ==============================================================================

library(RPostgres)
library(sf)

# ==============================================================================
# CONNECTION
# ==============================================================================

get_db_connection <- function() {
  source("credentials.R", local = TRUE)
  tryCatch({
    conn <- dbConnect(
      Postgres(),
      dbname = DB_CONFIG$dbname,
      host = DB_CONFIG$host,
      port = DB_CONFIG$port,
      user = DB_CONFIG$user,
      password = DB_CONFIG$password
    )
    return(conn)
  }, error = function(e) {
    stop(paste("Database connection failed:", e$message))
  })
}

# ==============================================================================
# BASIC QUERIES
# ==============================================================================

get_regions <- function(conn) {
  dbGetQuery(conn, "SELECT id, name, code FROM regions ORDER BY name")
}

get_provinces <- function(conn, region_id = NULL) {
  if (is.null(region_id) || region_id == "") {
    query <- "SELECT id, name, region_id FROM provinces ORDER BY name"
  } else {
    query <- sprintf(
      "SELECT id, name, region_id FROM provinces WHERE region_id = %d ORDER BY name",
      as.integer(region_id)
    )
  }
  dbGetQuery(conn, query)
}

get_municipalities <- function(conn, province_id = NULL, region_id = NULL) {
  if (!is.null(province_id) && province_id != "") {
    query <- sprintf(
      "SELECT id, name, province_id FROM municipalities WHERE province_id = %d ORDER BY name",
      as.integer(province_id)
    )
  } else if (!is.null(region_id) && region_id != "") {
    query <- sprintf(
      "SELECT m.id, m.name, m.province_id 
       FROM municipalities m
       JOIN provinces p ON m.province_id = p.id
       WHERE p.region_id = %d
       ORDER BY m.name",
      as.integer(region_id)
    )
  } else {
    query <- "SELECT id, name, province_id FROM municipalities ORDER BY name"
  }
  dbGetQuery(conn, query)
}

# ==============================================================================
# GEOMETRY QUERIES
# ==============================================================================

get_area_geometry <- function(conn, level, id) {
  query <- switch(level,
                  "province" = sprintf(
                    "SELECT id, name, ST_AsText(geometry) as wkt FROM provinces WHERE id = %d",
                    as.integer(id)
                  ),
                  "municipality" = sprintf(
                    "SELECT id, name, ST_AsText(geometry) as wkt FROM municipalities WHERE id = %d",
                    as.integer(id)
                  ),
                  stop("Invalid level")
  )
  
  result <- dbGetQuery(conn, query)
  if (nrow(result) == 0) return(NULL)
  
  geom <- st_as_sfc(result$wkt, crs = 4326)
  st_sf(id = result$id, name = result$name, geometry = geom)
}

# OPTIMIZED: Uses spatial index and simplified geometries for faster loading
get_municipalities_with_metrics <- function(conn, province_id = NULL, region_id = NULL) {
  if (!is.null(province_id) && province_id != "") {
    query <- sprintf(
      "SELECT m.id, m.name, 
              COALESCE(ST_AsText(m.geometry_simplified), ST_AsText(m.geometry)) as wkt, 
              mv.value
       FROM municipalities m
       LEFT JOIN metric_values mv ON mv.municipality_id = m.id
       LEFT JOIN metric_definitions md ON mv.metric_id = md.id
       WHERE m.province_id = %d AND (md.metric_key = 'population_density' OR md.metric_key IS NULL)
       ORDER BY m.name",
      as.integer(province_id)
    )
  } else if (!is.null(region_id) && region_id != "") {
    query <- sprintf(
      "SELECT m.id, m.name, 
              COALESCE(ST_AsText(m.geometry_simplified), ST_AsText(m.geometry)) as wkt, 
              mv.value
       FROM municipalities m
       JOIN provinces p ON m.province_id = p.id
       LEFT JOIN metric_values mv ON mv.municipality_id = m.id
       LEFT JOIN metric_definitions md ON mv.metric_id = md.id
       WHERE p.region_id = %d AND (md.metric_key = 'population_density' OR md.metric_key IS NULL)
       ORDER BY m.name",
      as.integer(region_id)
    )
  } else {
    # Return ALL municipalities when no filter specified - uses simplified geometry for speed
    query <- "SELECT m.id, m.name, 
              COALESCE(ST_AsText(m.geometry_simplified), ST_AsText(m.geometry)) as wkt, 
              mv.value
       FROM municipalities m
       LEFT JOIN metric_values mv ON mv.municipality_id = m.id
       LEFT JOIN metric_definitions md ON mv.metric_id = md.id
       WHERE md.metric_key = 'population_density' OR md.metric_key IS NULL
       ORDER BY m.name"
  }
  
  result <- dbGetQuery(conn, query)
  if (nrow(result) == 0) return(NULL)
  
  geom <- st_as_sfc(result$wkt, crs = 4326)
  st_sf(
    id = result$id,
    name = result$name,
    value = result$value,
    geometry = geom
  )
}

# ==============================================================================
# METRIC QUERIES
# ==============================================================================

# NEW: Get Belgium-wide aggregate metrics
get_belgium_metric <- function(conn) {
  # Calculate weighted average by back-calculating population from density * area
  query <- "SELECT 
       'Belgium' as country_name,
       md.name as metric_name,
       md.unit,
       SUM(mv.value * m.area_km2) / NULLIF(SUM(m.area_km2), 0) as value,
       MIN(mv.value) as min_value,
       MAX(mv.value) as max_value,
       COUNT(DISTINCT mv.municipality_id) as municipality_count,
       MAX(mv.date) as date
     FROM metric_values mv
     JOIN metric_definitions md ON mv.metric_id = md.id
     JOIN municipalities m ON mv.municipality_id = m.id
     WHERE md.metric_key = 'population_density'
     GROUP BY md.name, md.unit"
  
  dbGetQuery(conn, query)
}

get_region_metric <- function(conn, region_id) {
  query <- sprintf(
    "SELECT 
       r.name as region_name,
       md.name as metric_name,
       md.unit,
       SUM(mv.value * m.area_km2) / NULLIF(SUM(m.area_km2), 0) as value,
       MIN(mv.value) as min_value,
       MAX(mv.value) as max_value,
       COUNT(DISTINCT mv.municipality_id) as municipality_count,
       MAX(mv.date) as date
     FROM metric_values mv
     JOIN metric_definitions md ON mv.metric_id = md.id
     JOIN municipalities m ON mv.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     JOIN regions r ON p.region_id = r.id
     WHERE r.id = %d AND md.metric_key = 'population_density'
     GROUP BY r.name, md.name, md.unit",
    as.integer(region_id)
  )
  dbGetQuery(conn, query)
}

get_province_metric <- function(conn, province_id) {
  query <- sprintf(
    "SELECT 
       p.name as province_name,
       md.name as metric_name,
       md.unit,
       SUM(mv.value * m.area_km2) / NULLIF(SUM(m.area_km2), 0) as value,
       MIN(mv.value) as min_value,
       MAX(mv.value) as max_value,
       COUNT(DISTINCT mv.municipality_id) as municipality_count,
       MAX(mv.date) as date
     FROM metric_values mv
     JOIN metric_definitions md ON mv.metric_id = md.id
     JOIN municipalities m ON mv.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     WHERE p.id = %d AND md.metric_key = 'population_density'
     GROUP BY p.name, md.name, md.unit",
    as.integer(province_id)
  )
  dbGetQuery(conn, query)
}

get_municipality_metric <- function(conn, municipality_id) {
  query <- sprintf(
    "SELECT mv.value, mv.date, md.name as metric_name, md.unit, m.name as municipality_name
     FROM metric_values mv
     JOIN metric_definitions md ON mv.metric_id = md.id
     JOIN municipalities m ON mv.municipality_id = m.id
     WHERE mv.municipality_id = %d AND md.metric_key = 'population_density'
     ORDER BY mv.date DESC
     LIMIT 1",
    as.integer(municipality_id)
  )
  dbGetQuery(conn, query)
}

# ==============================================================================
# NAME FREQUENCY QUERIES
# ==============================================================================

# NEW: Get top names for all of Belgium
get_belgium_top_names <- function(conn, top_n = 5) {
  query <- sprintf(
    "SELECT 
       nf.name,
       nf.gender,
       SUM(nf.frequency) as total_frequency,
       COUNT(DISTINCT nf.municipality_id) as municipality_count
     FROM name_frequencies nf
     WHERE nf.year = 2025
     GROUP BY nf.name, nf.gender
     ORDER BY nf.gender, total_frequency DESC"
  )
  
  result <- dbGetQuery(conn, query)
  
  # Get top N for each gender
  male <- result[result$gender == 'M', ][1:min(top_n, sum(result$gender == 'M')), ]
  female <- result[result$gender == 'F', ][1:min(top_n, sum(result$gender == 'F')), ]
  
  rbind(male, female)
}

# Get top names for a municipality
get_municipality_top_names <- function(conn, municipality_id, top_n = 5) {
  query <- sprintf(
    "SELECT name, frequency, gender, rank
     FROM top_names_per_municipality
     WHERE municipality_id = %d AND rank <= %d AND year = 2025
     ORDER BY gender, rank",
    as.integer(municipality_id),
    as.integer(top_n)
  )
  dbGetQuery(conn, query)
}

# Get top names for a province (aggregated)
get_province_top_names <- function(conn, province_id, top_n = 5) {
  query <- sprintf(
    "SELECT 
       nf.name,
       nf.gender,
       SUM(nf.frequency) as total_frequency,
       COUNT(DISTINCT nf.municipality_id) as municipality_count
     FROM name_frequencies nf
     JOIN municipalities m ON nf.municipality_id = m.id
     WHERE m.province_id = %d AND nf.year = 2025
     GROUP BY nf.name, nf.gender
     ORDER BY nf.gender, total_frequency DESC",
    as.integer(province_id)
  )
  
  result <- dbGetQuery(conn, query)
  
  # Get top N for each gender
  male <- result[result$gender == 'M', ][1:min(top_n, sum(result$gender == 'M')), ]
  female <- result[result$gender == 'F', ][1:min(top_n, sum(result$gender == 'F')), ]
  
  rbind(male, female)
}

# Get top names for a region (aggregated)
get_region_top_names <- function(conn, region_id, top_n = 5) {
  query <- sprintf(
    "SELECT 
       nf.name,
       nf.gender,
       SUM(nf.frequency) as total_frequency,
       COUNT(DISTINCT nf.municipality_id) as municipality_count
     FROM name_frequencies nf
     JOIN municipalities m ON nf.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     WHERE p.region_id = %d AND nf.year = 2025
     GROUP BY nf.name, nf.gender
     ORDER BY nf.gender, total_frequency DESC",
    as.integer(region_id)
  )
  
  result <- dbGetQuery(conn, query)
  
  # Get top N for each gender
  male <- result[result$gender == 'M', ][1:min(top_n, sum(result$gender == 'M')), ]
  female <- result[result$gender == 'F', ][1:min(top_n, sum(result$gender == 'F')), ]
  
  rbind(male, female)
}

# Get name diversity for a municipality
get_municipality_name_diversity <- function(conn, municipality_id) {
  query <- sprintf(
    "SELECT 
       gender,
       unique_names,
       total_frequency,
       top_name_frequency,
       top_name_percentage
     FROM municipality_name_diversity
     WHERE municipality_id = %d AND year = 2025
     ORDER BY gender",
    as.integer(municipality_id)
  )
  dbGetQuery(conn, query)
}

# Get name diversity for province (aggregated)
get_province_name_diversity <- function(conn, province_id) {
  query <- sprintf(
    "SELECT 
       nf.gender,
       COUNT(DISTINCT nf.name) as unique_names,
       SUM(nf.frequency) as total_frequency,
       COUNT(DISTINCT nf.municipality_id) as municipality_count
     FROM name_frequencies nf
     JOIN municipalities m ON nf.municipality_id = m.id
     WHERE m.province_id = %d AND nf.year = 2025
     GROUP BY nf.gender
     ORDER BY nf.gender",
    as.integer(province_id)
  )
  dbGetQuery(conn, query)
}

# Get name diversity for region (aggregated)
get_region_name_diversity <- function(conn, region_id) {
  query <- sprintf(
    "SELECT 
       nf.gender,
       COUNT(DISTINCT nf.name) as unique_names,
       SUM(nf.frequency) as total_frequency,
       COUNT(DISTINCT nf.municipality_id) as municipality_count
     FROM name_frequencies nf
     JOIN municipalities m ON nf.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     WHERE p.region_id = %d AND nf.year = 2025
     GROUP BY nf.gender
     ORDER BY nf.gender",
    as.integer(region_id)
  )
  dbGetQuery(conn, query)
}

# ==============================================================================
# HELPER FUNCTION FOR RENDERING NAME LISTS WITH SCALED BAR CHARTS
# ==============================================================================

render_name_list <- function(name_data, gender_type, frequency_col = "total_frequency") {
  if (nrow(name_data) == 0) return(NULL)
  
  # Calculate max frequency for scaling
  max_freq <- max(name_data[[frequency_col]], na.rm = TRUE)
  
  tags$div(
    tags$div(
      class = "names-list",
      lapply(1:nrow(name_data), function(i) {
        freq <- name_data[[frequency_col]][i]
        # Calculate percentage for bar width
        bar_width <- (freq / max_freq) * 100
        
        tags$div(
          class = "name-item",
          tags$div(
            class = "name-header",
            tags$span(
              class = "name-text",
              paste0(i, ". ", name_data$name[i])
            ),
            tags$span(
              class = "name-count",
              if(frequency_col == "frequency") {
                sprintf("%s", format(freq, big.mark = ","))
              } else {
                sprintf("%s", format(freq, big.mark = ","))
              }
            )
          ),
          tags$div(
            class = "name-bar-container",
            tags$div(
              class = paste("name-bar", tolower(gender_type)),
              style = sprintf("width: %.1f%%;", bar_width)
            )
          )
        )
      })
    )
  )
}

# ==============================================================================
# MAP RENDERING HELPERS
# ==============================================================================

# Create color palette for population density (log scale)
get_density_palette <- function() {
  colorNumeric(
    palette = "YlOrRd",
    domain = c(log10(1), log10(24000)),
    na.color = "#808080"
  )
}

# Render municipalities with density choropleth
render_municipalities_choropleth <- function(proxy, municipalities_geom, bbox = NULL, fit_bounds = FALSE) {
  if (is.null(municipalities_geom) || nrow(municipalities_geom) == 0) {
    return(proxy)
  }
  
  pal <- get_density_palette()
  
  proxy <- proxy %>%
    clearShapes() %>%
    addPolygons(
      data = municipalities_geom,
      fillColor = ~pal(log10(pmax(value, 1))),
      fillOpacity = 0.7,
      color = "white",
      weight = 1,
      label = ~paste0(name, ": ", round(value, 1), " people/km²")
    )
  
  if (fit_bounds && !is.null(bbox)) {
    proxy <- proxy %>%
      fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
  }
  
  proxy
}

# ==============================================================================
# METRICS PANEL RENDERING HELPERS
# ==============================================================================

# Render population density section
render_density_section <- function(metric_data, show_min_max = TRUE) {
  if (nrow(metric_data) == 0) return(NULL)
  
  density_card <- tags$div(
    class = "metric-card",
    tags$div(class = "metric-value", sprintf("%.1f", metric_data$value[1])),
    tags$div(class = "metric-label", paste0(
      if (show_min_max) "Average " else "", 
      metric_data$unit[1]
    ))
  )
  
  if (show_min_max) {
    tagList(
      tags$h4("Population Density", style = "margin-top: 0;"),
      density_card,
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
  } else {
    tagList(
      tags$h4("Population Density", style = "margin-top: 0;"),
      density_card,
      tags$hr()
    )
  }
}

# Render top names section with slider
render_names_section <- function(name_data, top_n, frequency_col = "total_frequency") {
  if (nrow(name_data) == 0) return(NULL)
  
  male_names <- name_data[name_data$gender == 'M', ]
  female_names <- name_data[name_data$gender == 'F', ]
  
  tagList(
    tags$h4("Most Popular Names (2025)"),
    tags$div(
      class = "names-slider-container",
      tags$div(
        class = "slider-wrapper",
        tags$input(
          type = "range",
          class = "names-slider",
          id = "topNamesSlider",
          min = "1",
          max = "10",
          value = as.character(top_n),
          step = "1"
        )
      )
    ),
    tags$div(
      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
      render_name_list(male_names, "Male", frequency_col),
      render_name_list(female_names, "Female", frequency_col)
    ),
    tags$hr()
  )
}

# NEW: Render Belgium info section
render_belgium_info <- function(metric_data) {
  if (nrow(metric_data) == 0) return(NULL)
  
  tags$div(
    class = "info-text",
    style = "margin-top: 15px;",
    tags$strong("Country: "), "Belgium",
    tags$br(),
    tags$strong("Municipalities: "), metric_data$municipality_count[1]
  )
}

# Render region info section
render_region_info <- function(metric_data) {
  if (nrow(metric_data) == 0) return(NULL)
  
  region_name <- metric_data$region_name[1]
  capitalized_name <- paste0(
    toupper(substr(region_name, 1, 1)), 
    tolower(substr(region_name, 2, nchar(region_name)))
  )
  
  tags$div(
    class = "info-text",
    style = "margin-top: 15px;",
    tags$strong("Region: "), capitalized_name,
    tags$br(),
    tags$strong("Municipalities: "), metric_data$municipality_count[1]
  )
}

# Render province info section
render_province_info <- function(metric_data) {
  if (nrow(metric_data) == 0) return(NULL)
  
  tags$div(
    class = "info-text",
    style = "margin-top: 15px;",
    tags$strong("Province: "), metric_data$province_name[1],
    tags$br(),
    tags$strong("Municipalities: "), metric_data$municipality_count[1]
  )
}

# Render municipality info section
render_municipality_info <- function(metric_data) {
  if (nrow(metric_data) == 0) return(NULL)
  
  tags$div(
    class = "info-text",
    style = "margin-top: 15px;",
    tags$strong("Location: "), metric_data$municipality_name[1]
  )
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Capitalize first letter of string
capitalize_first <- function(text) {
  paste0(toupper(substr(text, 1, 1)), tolower(substr(text, 2, nchar(text))))
}
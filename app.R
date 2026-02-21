# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  Belgium Location Browser — Population Density & Top Names                 ║
# ║  Modernised with luwitemplate theming & dark mode                         ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Libraries ────────────────────────────────────────────────────────────────
library(luwitemplate)
library(bslib)
library(shiny)
library(shinyjs)
library(leaflet)
library(markdown)
library(RPostgres)
library(sf)
library(DBI)

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  DATABASE FUNCTIONS                                                        ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Connection ───────────────────────────────────────────────────────────────
get_db_connection <- function() {
  source("credentials.R", local = TRUE)
  tryCatch({
    dbConnect(
      Postgres(),
      dbname   = DB_CONFIG$dbname,
      host     = DB_CONFIG$host,
      port     = DB_CONFIG$port,
      user     = DB_CONFIG$user,
      password = DB_CONFIG$password
    )
  }, error = function(e) stop(paste("Database connection failed:", e$message)))
}

# ── Basic queries ────────────────────────────────────────────────────────────
get_regions <- function(conn) {
  dbGetQuery(conn, "SELECT id, name, code FROM regions ORDER BY name")
}

get_provinces <- function(conn, region_id = NULL) {
  if (is.null(region_id) || region_id == "") {
    dbGetQuery(conn, "SELECT id, name, region_id FROM provinces ORDER BY name")
  } else {
    dbGetQuery(conn, sprintf(
      "SELECT id, name, region_id FROM provinces WHERE region_id = %d ORDER BY name",
      as.integer(region_id)))
  }
}

get_municipalities <- function(conn, province_id = NULL, region_id = NULL) {
  if (!is.null(province_id) && province_id != "") {
    query <- sprintf(
      "SELECT id, name, province_id FROM municipalities WHERE province_id = %d ORDER BY name",
      as.integer(province_id))
  } else if (!is.null(region_id) && region_id != "") {
    query <- sprintf(
      "SELECT m.id, m.name, m.province_id
       FROM municipalities m JOIN provinces p ON m.province_id = p.id
       WHERE p.region_id = %d ORDER BY m.name",
      as.integer(region_id))
  } else {
    query <- "SELECT id, name, province_id FROM municipalities ORDER BY name"
  }
  dbGetQuery(conn, query)
}

# ── Geometry queries ─────────────────────────────────────────────────────────
get_area_geometry <- function(conn, level, id) {
  query <- switch(level,
                  "province"     = sprintf("SELECT id, name, ST_AsText(geometry) as wkt FROM provinces WHERE id = %d",
                                           as.integer(id)),
                  "municipality" = sprintf("SELECT id, name, ST_AsText(geometry) as wkt FROM municipalities WHERE id = %d",
                                           as.integer(id)),
                  stop("Invalid level"))
  result <- dbGetQuery(conn, query)
  if (nrow(result) == 0) return(NULL)
  st_sf(id = result$id, name = result$name, geometry = st_as_sfc(result$wkt, crs = 4326))
}

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
       ORDER BY m.name", as.integer(province_id))
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
       ORDER BY m.name", as.integer(region_id))
  } else {
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
  st_sf(id = result$id, name = result$name, value = result$value,
        geometry = st_as_sfc(result$wkt, crs = 4326))
}

# ── Metric queries ───────────────────────────────────────────────────────────
get_belgium_metric <- function(conn) {
  dbGetQuery(conn,
             "SELECT 'Belgium' as country_name, md.name as metric_name, md.unit,
       SUM(mv.value * m.area_km2) / NULLIF(SUM(m.area_km2), 0) as value,
       MIN(mv.value) as min_value, MAX(mv.value) as max_value,
       COUNT(DISTINCT mv.municipality_id) as municipality_count, MAX(mv.date) as date
     FROM metric_values mv
     JOIN metric_definitions md ON mv.metric_id = md.id
     JOIN municipalities m ON mv.municipality_id = m.id
     WHERE md.metric_key = 'population_density'
     GROUP BY md.name, md.unit")
}

get_region_metric <- function(conn, region_id) {
  dbGetQuery(conn, sprintf(
    "SELECT r.name as region_name, md.name as metric_name, md.unit,
       SUM(mv.value * m.area_km2) / NULLIF(SUM(m.area_km2), 0) as value,
       MIN(mv.value) as min_value, MAX(mv.value) as max_value,
       COUNT(DISTINCT mv.municipality_id) as municipality_count, MAX(mv.date) as date
     FROM metric_values mv
     JOIN metric_definitions md ON mv.metric_id = md.id
     JOIN municipalities m ON mv.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     JOIN regions r ON p.region_id = r.id
     WHERE r.id = %d AND md.metric_key = 'population_density'
     GROUP BY r.name, md.name, md.unit", as.integer(region_id)))
}

get_province_metric <- function(conn, province_id) {
  dbGetQuery(conn, sprintf(
    "SELECT p.name as province_name, md.name as metric_name, md.unit,
       SUM(mv.value * m.area_km2) / NULLIF(SUM(m.area_km2), 0) as value,
       MIN(mv.value) as min_value, MAX(mv.value) as max_value,
       COUNT(DISTINCT mv.municipality_id) as municipality_count, MAX(mv.date) as date
     FROM metric_values mv
     JOIN metric_definitions md ON mv.metric_id = md.id
     JOIN municipalities m ON mv.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     WHERE p.id = %d AND md.metric_key = 'population_density'
     GROUP BY p.name, md.name, md.unit", as.integer(province_id)))
}

get_municipality_metric <- function(conn, municipality_id) {
  dbGetQuery(conn, sprintf(
    "SELECT mv.value, mv.date, md.name as metric_name, md.unit, m.name as municipality_name
     FROM metric_values mv
     JOIN metric_definitions md ON mv.metric_id = md.id
     JOIN municipalities m ON mv.municipality_id = m.id
     WHERE mv.municipality_id = %d AND md.metric_key = 'population_density'
     ORDER BY mv.date DESC LIMIT 1", as.integer(municipality_id)))
}

# ── Name frequency queries ───────────────────────────────────────────────────
get_belgium_top_names <- function(conn, top_n = 5) {
  result <- dbGetQuery(conn,
                       "SELECT nf.name, nf.gender, SUM(nf.frequency) as total_frequency,
       COUNT(DISTINCT nf.municipality_id) as municipality_count
     FROM name_frequencies nf WHERE nf.year = 2025
     GROUP BY nf.name, nf.gender ORDER BY nf.gender, total_frequency DESC")
  male   <- result[result$gender == "M", ][1:min(top_n, sum(result$gender == "M")), ]
  female <- result[result$gender == "F", ][1:min(top_n, sum(result$gender == "F")), ]
  rbind(male, female)
}

get_municipality_top_names <- function(conn, municipality_id, top_n = 5) {
  dbGetQuery(conn, sprintf(
    "SELECT name, frequency, gender, rank
     FROM top_names_per_municipality
     WHERE municipality_id = %d AND rank <= %d AND year = 2025
     ORDER BY gender, rank",
    as.integer(municipality_id), as.integer(top_n)))
}

get_province_top_names <- function(conn, province_id, top_n = 5) {
  result <- dbGetQuery(conn, sprintf(
    "SELECT nf.name, nf.gender, SUM(nf.frequency) as total_frequency,
       COUNT(DISTINCT nf.municipality_id) as municipality_count
     FROM name_frequencies nf JOIN municipalities m ON nf.municipality_id = m.id
     WHERE m.province_id = %d AND nf.year = 2025
     GROUP BY nf.name, nf.gender ORDER BY nf.gender, total_frequency DESC",
    as.integer(province_id)))
  male   <- result[result$gender == "M", ][1:min(top_n, sum(result$gender == "M")), ]
  female <- result[result$gender == "F", ][1:min(top_n, sum(result$gender == "F")), ]
  rbind(male, female)
}

get_region_top_names <- function(conn, region_id, top_n = 5) {
  result <- dbGetQuery(conn, sprintf(
    "SELECT nf.name, nf.gender, SUM(nf.frequency) as total_frequency,
       COUNT(DISTINCT nf.municipality_id) as municipality_count
     FROM name_frequencies nf JOIN municipalities m ON nf.municipality_id = m.id
     JOIN provinces p ON m.province_id = p.id
     WHERE p.region_id = %d AND nf.year = 2025
     GROUP BY nf.name, nf.gender ORDER BY nf.gender, total_frequency DESC",
    as.integer(region_id)))
  male   <- result[result$gender == "M", ][1:min(top_n, sum(result$gender == "M")), ]
  female <- result[result$gender == "F", ][1:min(top_n, sum(result$gender == "F")), ]
  rbind(male, female)
}

# ── Map helpers ──────────────────────────────────────────────────────────────
get_density_palette <- function() {
  colorNumeric(palette = "YlOrRd", domain = c(log10(1), log10(24000)), na.color = "#808080")
}

render_municipalities_choropleth <- function(proxy, municipalities_geom, bbox = NULL, fit_bounds = FALSE) {
  if (is.null(municipalities_geom) || nrow(municipalities_geom) == 0) return(proxy)
  pal <- get_density_palette()
  proxy <- proxy %>%
    clearShapes() %>%
    addPolygons(
      data = municipalities_geom,
      fillColor = ~pal(log10(pmax(value, 1))), fillOpacity = 0.7,
      color = "white", weight = 1,
      label = ~paste0(name, ": ", round(value, 1), " people/km\u00B2"))
  if (fit_bounds && !is.null(bbox))
    proxy <- proxy %>% fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
  proxy
}

# ── Utility ──────────────────────────────────────────────────────────────────
capitalize_first <- function(text) {
  paste0(toupper(substr(text, 1, 1)), tolower(substr(text, 2, nchar(text))))
}

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  UI RENDERING HELPERS (metrics panel)                                      ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

render_name_list <- function(name_data, gender_type, frequency_col = "total_frequency") {
  if (nrow(name_data) == 0) return(NULL)
  max_freq <- max(name_data[[frequency_col]], na.rm = TRUE)
  
  tags$div(class = "names-list",
           lapply(seq_len(nrow(name_data)), function(i) {
             freq <- name_data[[frequency_col]][i]
             bar_width <- (freq / max_freq) * 100
             tags$div(class = "name-item",
                      tags$div(class = "name-header",
                               tags$span(class = "name-text", paste0(i, ". ", name_data$name[i])),
                               tags$span(class = "name-count", format(freq, big.mark = ","))
                      ),
                      tags$div(class = "name-bar-container",
                               tags$div(class = paste("name-bar", tolower(gender_type)),
                                        style = sprintf("width: %.1f%%;", bar_width))
                      )
             )
           })
  )
}

render_density_section <- function(metric_data, show_min_max = TRUE) {
  if (nrow(metric_data) == 0) return(NULL)
  
  density_card <- tags$div(class = "metric-card",
                           tags$div(class = "metric-value", sprintf("%.1f", metric_data$value[1])),
                           tags$div(class = "metric-label",
                                    paste0(if (show_min_max) "Average " else "", metric_data$unit[1]))
  )
  
  if (show_min_max) {
    tagList(
      tags$h5("Population Density", style = "margin-top: 0;"),
      density_card,
      tags$div(class = "stats-grid",
               tags$div(class = "stat-item",
                        tags$div(class = "stat-value", sprintf("%.1f", metric_data$min_value[1])),
                        tags$div(class = "stat-label", "Minimum")),
               tags$div(class = "stat-item",
                        tags$div(class = "stat-value", sprintf("%.1f", metric_data$max_value[1])),
                        tags$div(class = "stat-label", "Maximum"))
      ),
      tags$hr(style = "border-color: var(--bs-border-color);")
    )
  } else {
    tagList(
      tags$h5("Population Density", style = "margin-top: 0;"),
      density_card,
      tags$hr(style = "border-color: var(--bs-border-color);")
    )
  }
}

render_names_section <- function(name_data, top_n, frequency_col = "total_frequency") {
  if (nrow(name_data) == 0) return(NULL)
  male_names   <- name_data[name_data$gender == "M", ]
  female_names <- name_data[name_data$gender == "F", ]
  
  tagList(
    tags$h5("Most Popular Names (2025)"),
    tags$div(class = "names-slider-container",
             tags$div(class = "slider-wrapper",
                      tags$input(type = "range", class = "names-slider", id = "topNamesSlider",
                                 min = "1", max = "10", value = as.character(top_n), step = "1")
             )
    ),
    tags$div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
             render_name_list(male_names, "Male", frequency_col),
             render_name_list(female_names, "Female", frequency_col)
    ),
    tags$hr(style = "border-color: var(--bs-border-color);")
  )
}

render_belgium_info <- function(metric_data) {
  if (nrow(metric_data) == 0) return(NULL)
  tags$div(class = "info-text", style = "margin-top: 15px;",
           tags$strong("Country: "), "Belgium", tags$br(),
           tags$strong("Municipalities: "), metric_data$municipality_count[1])
}

render_region_info <- function(metric_data) {
  if (nrow(metric_data) == 0) return(NULL)
  tags$div(class = "info-text", style = "margin-top: 15px;",
           tags$strong("Region: "), capitalize_first(metric_data$region_name[1]), tags$br(),
           tags$strong("Municipalities: "), metric_data$municipality_count[1])
}

render_province_info <- function(metric_data) {
  if (nrow(metric_data) == 0) return(NULL)
  tags$div(class = "info-text", style = "margin-top: 15px;",
           tags$strong("Province: "), metric_data$province_name[1], tags$br(),
           tags$strong("Municipalities: "), metric_data$municipality_count[1])
}

render_municipality_info <- function(metric_data) {
  if (nrow(metric_data) == 0) return(NULL)
  tags$div(class = "info-text", style = "margin-top: 15px;",
           tags$strong("Location: "), metric_data$municipality_name[1])
}

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  CUSTOM CSS — replaces www/styles.css                                      ║
# ║  Uses bslib CSS variables for automatic dark mode reactivity               ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

panel_css <- tags$style(HTML("
  /* ── Full-viewport map ─────────────────────────────────────────────── */
  .map-container { position: relative; width: 100%; height: 100vh; }
  .map-container .leaflet-container { width: 100%; height: 100%; }

  /* ── Floating hover panels — vertically centered ───────────────────── */
  .floating-panel {
    position: absolute;
    top: 50%;
    transform: translateY(-50%);
    width: 320px;
    max-height: calc(100vh - 40px);
    overflow-y: auto;
    z-index: 1000;
    background: var(--bs-body-bg);
    color: var(--bs-body-color);
    border: 1px solid var(--bs-border-color);
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 4px 24px rgba(0, 0, 0, 0.12);
    transition: transform 0.35s cubic-bezier(0.4, 0, 0.2, 1),
                opacity 0.35s cubic-bezier(0.4, 0, 0.2, 1);
  }
  .left-panel  { left: 12px; }
  .right-panel { right: 12px; width: 385px; }

  .floating-panel.closed-left  { transform: translate(-360px, -50%); opacity: 0; pointer-events: none; }
  .floating-panel.closed-right { transform: translate(420px, -50%);  opacity: 0; pointer-events: none; }

  /* ── Toggle buttons (show when panel hidden) — vertically centered ── */
  .panel-toggle-btn {
    position: absolute;
    top: 50%;
    transform: translateY(-50%);
    z-index: 1001;
    width: 40px; height: 40px;
    border: 1px solid var(--bs-border-color);
    border-radius: 10px;
    background: var(--bs-body-bg);
    color: var(--bs-body-color);
    font-size: 18px;
    cursor: pointer;
    display: flex; align-items: center; justify-content: center;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    transition: opacity 0.25s, transform 0.25s;
  }
  .panel-toggle-btn.hidden { opacity: 0; pointer-events: none; transform: translateY(-50%) scale(0.8); }
  .toggle-left  { left: 12px; }
  .toggle-right { right: 12px; }

  /* ── Panel header row — close + dark mode toggle ────────────────────── */
  .panel-header-row {
    display: flex; align-items: center; justify-content: space-between;
    margin-bottom: 4px;
  }
  .panel-header-actions {
    display: flex; align-items: center; gap: 6px;
    margin-left: auto;
  }
  .panel-header-actions .form-check.form-switch {
    margin: 0; padding-left: 2.5em;
  }

  /* ── Close button inside panels ────────────────────────────────────── */
  .close-panel-btn {
    background: none; border: none;
    font-size: 22px; line-height: 1; cursor: pointer;
    color: var(--bs-body-color);
    opacity: 0.6; padding: 0;
  }
  .close-panel-btn:hover { opacity: 1; }

  /* ── Panel typography ──────────────────────────────────────────────── */
  .panel-title {
    font-size: 1.15rem; font-weight: 700;
    margin-bottom: 16px; padding-bottom: 8px;
    border-bottom: 2px solid var(--bs-primary);
  }
  .filter-group { margin-bottom: 14px; }
  .filter-group label { font-size: 0.85rem; font-weight: 600; margin-bottom: 4px; }

  /* ── Metric card ───────────────────────────────────────────────────── */
  .metric-card {
    text-align: center; padding: 16px; border-radius: 10px;
    background: var(--bs-tertiary-bg);
    border: 1px solid var(--bs-border-color);
    margin-bottom: 12px;
  }
  .metric-value { font-size: 2rem; font-weight: 700; color: var(--bs-primary); }
  .metric-label { font-size: 0.82rem; opacity: 0.7; margin-top: 2px; }

  /* ── Stats grid (min / max) ────────────────────────────────────────── */
  .stats-grid {
    display: grid; grid-template-columns: 1fr 1fr; gap: 10px;
    margin-bottom: 12px;
  }
  .stat-item {
    text-align: center; padding: 10px; border-radius: 8px;
    background: var(--bs-tertiary-bg);
    border: 1px solid var(--bs-border-color);
  }
  .stat-value { font-size: 1.15rem; font-weight: 700; }
  .stat-label { font-size: 0.75rem; opacity: 0.6; }

  /* ── Name bars ─────────────────────────────────────────────────────── */
  .name-item { margin-bottom: 6px; }
  .name-header { display: flex; justify-content: space-between; font-size: 0.82rem; margin-bottom: 2px; }
  .name-text { font-weight: 600; }
  .name-count { opacity: 0.7; font-size: 0.78rem; }
  .name-bar-container {
    height: 6px; border-radius: 3px;
    background: var(--bs-tertiary-bg);
    overflow: hidden;
  }
  .name-bar {
    height: 100%; border-radius: 3px;
    transition: width 0.4s ease;
  }
  .name-bar.male   { background: #3498db; }
  .name-bar.female { background: #e74c8b; }

  /* ── Top-N slider ──────────────────────────────────────────────────── */
  .names-slider-container { margin-bottom: 14px; }
  .names-slider {
    -webkit-appearance: none; appearance: none;
    width: 100%; height: 6px; border-radius: 3px;
    background: var(--bs-tertiary-bg);
    outline: none; cursor: pointer;
  }
  .names-slider::-webkit-slider-thumb {
    -webkit-appearance: none; appearance: none;
    width: 18px; height: 18px; border-radius: 50%;
    background: var(--bs-primary);
    cursor: pointer; border: 2px solid var(--bs-body-bg);
    box-shadow: 0 1px 4px rgba(0,0,0,0.2);
  }
  .names-slider::-moz-range-thumb {
    width: 18px; height: 18px; border-radius: 50%;
    background: var(--bs-primary);
    cursor: pointer; border: 2px solid var(--bs-body-bg);
  }

  /* ── Info text ─────────────────────────────────────────────────────── */
  .info-text { font-size: 0.85rem; line-height: 1.6; }

  /* ── Select inputs — inherit theme ─────────────────────────────────── */
  .floating-panel select.form-select,
  .floating-panel .selectize-input,
  .floating-panel .selectize-dropdown {
    background: var(--bs-body-bg) !important;
    color: var(--bs-body-color) !important;
    border-color: var(--bs-border-color) !important;
  }

  /* ── Leaflet legend — transparent bg, themed text ──────────────────── */
  .leaflet-control.legend,
  .info.legend {
    background: transparent !important;
    border: none !important;
    box-shadow: none !important;
    color: var(--bs-body-color);
  }

  /* ── Custom scrollbar for panels ───────────────────────────────────── */
  .floating-panel::-webkit-scrollbar { width: 6px; }
  .floating-panel::-webkit-scrollbar-track { background: transparent; }
  .floating-panel::-webkit-scrollbar-thumb { background: var(--bs-border-color); border-radius: 3px; }

  /* ── No-navbar: body margin reset ──────────────────────────────────── */
  body { margin: 0; padding: 0; overflow: hidden; }

  /* ── About button — ensure visibility in dark mode ─────────────────── */
  [data-bs-theme='dark'] .btn-outline-primary {
    color: var(--bs-primary);
    border-color: var(--bs-primary);
  }

  /* ── About modal inherits theme ────────────────────────────────────── */
  .about-modal-content img { max-width: 100%; height: auto; display: block; margin: 15px auto; }
  .about-modal-content     { line-height: 1.6; }
  .about-modal-content h1  { margin-top: 0; }
  .about-modal-content h2  { margin-top: 20px; border-bottom: 1px solid var(--bs-border-color); padding-bottom: 5px; }
"))

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  UI                                                                        ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

ui <- bslib::page_fillable(
  theme = my_theme(),
  dark_mode_css(),
  panel_css,
  title = "Belgium Location Browser",
  padding = 0,
  fillable_mobile = TRUE,
  
  useShinyjs(),
  
  tags$div(
    class = "map-container",
    
    # ── Toggle buttons (visible when panel is closed) ──
    tags$button(class = "panel-toggle-btn toggle-left hidden",
                id = "sidebarToggle", "\u2630"),
    tags$button(class = "panel-toggle-btn toggle-right hidden",
                id = "metricsToggle", "\U0001F4CA"),
    
    # ── Left floating panel — location selection ──
    tags$div(
      class = "floating-panel left-panel", id = "leftPanel",
      tags$div(class = "panel-header-row",
               tags$div(class = "panel-title", style = "margin-bottom: 0; padding-bottom: 0; border: none;",
                        "Location Selection"),
               tags$div(class = "panel-header-actions",
                        tags$button(class = "close-panel-btn", id = "closeLeftPanel", "\u00D7"))
      ),
      tags$hr(style = "margin: 8px 0 14px; border-color: var(--bs-primary); border-width: 2px;"),
      
      tags$div(class = "filter-group",
               selectInput("region", "Region", choices = NULL)),
      
      shinyjs::hidden(
        tags$div(class = "filter-group", id = "provinceGroup",
                 selectInput("province", "Province", choices = NULL))
      ),
      
      shinyjs::hidden(
        tags$div(class = "filter-group", id = "municipalityGroup",
                 selectInput("municipality", "Municipality", choices = NULL))
      ),
      
      tags$div(class = "filter-group",
               actionButton("reset", "Reset Selection", class = "btn-secondary btn-block w-100")),
      tags$div(class = "filter-group",
               actionButton("aboutBtn", "About", class = "btn-outline-primary btn-block w-100",
                            icon = icon("info-circle")))
    ),
    
    # ── Right floating panel — information ──
    tags$div(
      class = "floating-panel right-panel", id = "rightPanel",
      tags$div(class = "panel-header-row",
               tags$div(class = "panel-title", style = "margin-bottom: 0; padding-bottom: 0; border: none;",
                        "Information"),
               tags$div(class = "panel-header-actions",
                        input_dark_mode(id = "dark_mode"),
                        tags$button(class = "close-panel-btn", id = "closeRightPanel", "\u00D7"))
      ),
      tags$hr(style = "margin: 8px 0 14px; border-color: var(--bs-primary); border-width: 2px;"),
      
      conditionalPanel(
        condition = "output.show_metrics",
        uiOutput("metrics_display")
      )
    ),
    
    # ── Map ──
    leafletOutput("map", width = "100%", height = "100%")
  ),
  
  # ── Panel toggle JS + slider handler ──
  tags$script(HTML("
    $(document).ready(function() {
      $('#closeLeftPanel').click(function() {
        $('#leftPanel').addClass('closed-left');
        $('#sidebarToggle').removeClass('hidden');
      });
      $('#sidebarToggle').click(function() {
        $('#leftPanel').removeClass('closed-left');
        $('#sidebarToggle').addClass('hidden');
      });
      $('#closeRightPanel').click(function() {
        $('#rightPanel').addClass('closed-right');
        $('#metricsToggle').removeClass('hidden');
      });
      $('#metricsToggle').click(function() {
        $('#rightPanel').removeClass('closed-right');
        $('#metricsToggle').addClass('hidden');
      });

      // Top-N slider: send value to Shiny
      $(document).on('change', '#topNamesSlider', function() {
        Shiny.setInputValue('topNamesCount', parseInt($(this).val()));
      });

      // Force legend transparency (leaflet legend widget)
      function cleanLegend() {
        $('.info.legend, .leaflet-control.legend, div.legend').css({
          'background': 'transparent', 'background-color': 'transparent',
          'border': 'none', 'box-shadow': 'none'
        });
      }
      setTimeout(cleanLegend, 200);
      setTimeout(cleanLegend, 1000);
      setInterval(cleanLegend, 2000);
    });
  "))
)

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  SERVER                                                                    ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

server <- function(input, output, session) {
  
  # ── Database connection ──
  conn <- get_db_connection()
  onStop(function() dbDisconnect(conn))
  
  # ── Dark mode: luwitemplate reactive ──
  dm <- use_dark_mode(input, session)
  
  # ── Reactive state ──
  municipalities_cache <- reactiveVal(NULL)
  selected <- reactiveValues(level = "belgium", id = NULL, name = NULL, region_code = NULL)
  topNamesCount <- reactiveVal(5)
  
  observeEvent(input$topNamesCount, { topNamesCount(input$topNamesCount) })
  
  # ══════════════════════════════════════════════════════════════════════════
  # CACHE MUNICIPALITIES ON STARTUP
  # ══════════════════════════════════════════════════════════════════════════
  
  observe({
    if (is.null(municipalities_cache()))
      municipalities_cache(get_municipalities_with_metrics(conn))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # INITIALIZE REGION DROPDOWN
  # ══════════════════════════════════════════════════════════════════════════
  
  observe({
    regions <- get_regions(conn)
    regions$name <- sapply(regions$name, capitalize_first)
    choices <- c("Select Region" = "", setNames(regions$id, regions$name))
    updateSelectInput(session, "region", choices = choices, selected = "")
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REGION SELECTION
  # ══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$region, {
    if (input$region == "") {
      shinyjs::hide("provinceGroup")
      shinyjs::hide("municipalityGroup")
      updateSelectInput(session, "province",     choices = c("Select Province" = ""),     selected = "")
      updateSelectInput(session, "municipality", choices = c("Select Municipality" = ""), selected = "")
      selected$level <- "belgium"; selected$id <- NULL; selected$region_code <- NULL
      
      municipalities_geom <- municipalities_cache()
      if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
        leafletProxy("map") %>%
          render_municipalities_choropleth(municipalities_geom) %>%
          setView(lng = 4.5, lat = 50.5, zoom = 8)
      }
    } else {
      region_data <- dbGetQuery(conn, sprintf("SELECT code FROM regions WHERE id = %d", as.integer(input$region)))
      selected$region_code <- region_data$code[1]
      
      municipalities_geom <- get_municipalities_with_metrics(conn, region_id = input$region)
      if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
        bbox <- st_bbox(municipalities_geom)
        leafletProxy("map") %>%
          render_municipalities_choropleth(municipalities_geom, bbox, fit_bounds = TRUE)
      }
      
      if (selected$region_code == "BRU") {
        shinyjs::hide("provinceGroup")
        shinyjs::show("municipalityGroup")
        municipalities <- get_municipalities(conn, region_id = input$region)
        choices <- c("Select Municipality" = "", setNames(municipalities$id, municipalities$name))
        updateSelectInput(session, "municipality", choices = choices, selected = "")
      } else {
        shinyjs::show("provinceGroup")
        shinyjs::hide("municipalityGroup")
        provinces <- get_provinces(conn, input$region)
        provinces$name <- gsub("^Provincie ", "", provinces$name)
        choices <- c("Select Province" = "", setNames(provinces$id, provinces$name))
        updateSelectInput(session, "province", choices = choices, selected = "")
        updateSelectInput(session, "municipality", choices = c("Select Municipality" = ""), selected = "")
      }
      
      selected$level <- "region"; selected$id <- input$region
    }
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # PROVINCE SELECTION
  # ══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$province, {
    if (input$province != "") {
      shinyjs::show("municipalityGroup")
      municipalities <- get_municipalities(conn, province_id = input$province)
      choices <- c("Select Municipality" = "", setNames(municipalities$id, municipalities$name))
      updateSelectInput(session, "municipality", choices = choices, selected = "")
      
      municipalities_geom <- get_municipalities_with_metrics(conn, province_id = input$province)
      if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
        bbox <- st_bbox(municipalities_geom)
        leafletProxy("map") %>%
          render_municipalities_choropleth(municipalities_geom, bbox, fit_bounds = TRUE)
      }
      
      selected$level <- "province"; selected$id <- input$province
    }
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # MUNICIPALITY SELECTION
  # ══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$municipality, {
    if (input$municipality != "") {
      result <- dbGetQuery(conn, sprintf(
        "SELECT m.id, m.name, ST_AsText(m.geometry) as wkt, mv.value
         FROM municipalities m
         LEFT JOIN metric_values mv ON mv.municipality_id = m.id
         LEFT JOIN metric_definitions md ON mv.metric_id = md.id
         WHERE m.id = %d AND (md.metric_key = 'population_density' OR md.metric_key IS NULL)
         LIMIT 1", as.integer(input$municipality)))
      
      if (nrow(result) > 0) {
        municipality_geom <- st_sf(
          id = result$id, name = result$name, value = result$value,
          geometry = st_as_sfc(result$wkt, crs = 4326))
        
        pal  <- get_density_palette()
        bbox <- st_bbox(municipality_geom)
        leafletProxy("map") %>%
          clearShapes() %>%
          addPolygons(
            data = municipality_geom,
            fillColor = ~pal(log10(pmax(value, 1))), fillOpacity = 0.7,
            color = "white", weight = 3,
            label = ~paste0(name, ": ", round(value, 1), " people/km\u00B2")) %>%
          fitBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
        
        selected$level <- "municipality"; selected$id <- input$municipality
        selected$name <- result$name[1]
      }
    }
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # RESET
  # ══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$reset, {
    updateSelectInput(session, "region",       selected = "")
    updateSelectInput(session, "province",     selected = "")
    updateSelectInput(session, "municipality", selected = "")
    shinyjs::hide("provinceGroup")
    shinyjs::hide("municipalityGroup")
    
    municipalities_geom <- municipalities_cache()
    if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
      leafletProxy("map") %>%
        render_municipalities_choropleth(municipalities_geom) %>%
        setView(lng = 4.5, lat = 50.5, zoom = 8)
    }
    
    selected$level <- "belgium"; selected$id <- NULL
    selected$name <- NULL; selected$region_code <- NULL
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # ABOUT MODAL
  # ══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$aboutBtn, {
    about_content <- tryCatch({
      if (file.exists("about.md")) markdownToHTML(file = "about.md", fragment.only = TRUE)
      else "<p>About content not found. Please create an <code>about.md</code> file.</p>"
    }, error = function(e) paste("<p>Error loading about content:</p><pre>", e$message, "</pre>"))
    
    showModal(modalDialog(
      title = "About This Application",
      tags$div(class = "about-modal-content", HTML(about_content)),
      easyClose = TRUE, footer = modalButton("Close"), size = "l"))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # MAP
  # ══════════════════════════════════════════════════════════════════════════
  
  output$map <- renderLeaflet({
    municipalities_geom <- get_municipalities_with_metrics(conn)
    municipalities_cache(municipalities_geom)
    
    pal <- get_density_palette()
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 4.5, lat = 50.5, zoom = 8)
    
    if (!is.null(municipalities_geom) && nrow(municipalities_geom) > 0) {
      map <- map %>%
        addPolygons(
          data = municipalities_geom,
          fillColor = ~pal(log10(pmax(value, 1))), fillOpacity = 0.7,
          color = "white", weight = 1,
          label = ~paste0(name, ": ", round(value, 1), " people/km\u00B2"))
    }
    map
  })
  
  # ── Swap tiles on dark mode toggle ──
  observe({
    tile <- if (dm$mode() == "dark") providers$CartoDB.DarkMatter else providers$CartoDB.Positron
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(tile)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # METRICS PANEL
  # ══════════════════════════════════════════════════════════════════════════
  
  output$show_metrics <- reactive({ !is.null(selected$level) })
  outputOptions(output, "show_metrics", suspendWhenHidden = FALSE)
  
  output$metrics_display <- renderUI({
    req(selected$level)
    top_n <- topNamesCount()
    
    if (selected$level == "belgium") {
      metric_data <- get_belgium_metric(conn)
      name_data   <- get_belgium_top_names(conn, top_n = top_n)
      tagList(render_density_section(metric_data, show_min_max = TRUE),
              render_names_section(name_data, top_n, "total_frequency"),
              render_belgium_info(metric_data))
      
    } else if (selected$level == "region") {
      metric_data <- get_region_metric(conn, selected$id)
      name_data   <- get_region_top_names(conn, selected$id, top_n = top_n)
      tagList(render_density_section(metric_data, show_min_max = TRUE),
              render_names_section(name_data, top_n, "total_frequency"),
              render_region_info(metric_data))
      
    } else if (selected$level == "province") {
      metric_data <- get_province_metric(conn, selected$id)
      name_data   <- get_province_top_names(conn, selected$id, top_n = top_n)
      tagList(render_density_section(metric_data, show_min_max = TRUE),
              render_names_section(name_data, top_n, "total_frequency"),
              render_province_info(metric_data))
      
    } else if (selected$level == "municipality") {
      metric_data <- get_municipality_metric(conn, selected$id)
      name_data   <- get_municipality_top_names(conn, selected$id, top_n = top_n)
      tagList(render_density_section(metric_data, show_min_max = FALSE),
              render_names_section(name_data, top_n, "frequency"),
              render_municipality_info(metric_data))
    }
  })
}

# ── Run ──────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
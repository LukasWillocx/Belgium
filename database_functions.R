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

get_municipalities_with_metrics <- function(conn, province_id = NULL, region_id = NULL) {
  if (!is.null(province_id) && province_id != "") {
    query <- sprintf(
      "SELECT m.id, m.name, ST_AsText(m.geometry) as wkt, mv.value
       FROM municipalities m
       LEFT JOIN metric_values mv ON mv.municipality_id = m.id
       LEFT JOIN metric_definitions md ON mv.metric_id = md.id
       WHERE m.province_id = %d AND md.metric_key = 'population_density'
       ORDER BY m.name",
      as.integer(province_id)
    )
  } else if (!is.null(region_id) && region_id != "") {
    query <- sprintf(
      "SELECT m.id, m.name, ST_AsText(m.geometry) as wkt, mv.value
       FROM municipalities m
       JOIN provinces p ON m.province_id = p.id
       LEFT JOIN metric_values mv ON mv.municipality_id = m.id
       LEFT JOIN metric_definitions md ON mv.metric_id = md.id
       WHERE p.region_id = %d AND md.metric_key = 'population_density'
       ORDER BY m.name",
      as.integer(region_id)
    )
  } else {
    return(NULL)
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

get_region_metric <- function(conn, region_id) {
  query <- sprintf(
    "SELECT 
       r.name as region_name,
       md.name as metric_name,
       md.unit,
       rm.avg_value as value,
       rm.min_value,
       rm.max_value,
       rm.municipality_count,
       rm.date
     FROM region_metrics rm
     JOIN regions r ON rm.region_id = r.id
     JOIN metric_definitions md ON rm.metric_id = md.id
     WHERE rm.region_id = %d AND md.metric_key = 'population_density'
     ORDER BY rm.date DESC
     LIMIT 1",
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
       pm.avg_value as value,
       pm.min_value,
       pm.max_value,
       pm.municipality_count,
       pm.date
     FROM province_metrics pm
     JOIN provinces p ON pm.province_id = p.id
     JOIN metric_definitions md ON pm.metric_id = md.id
     WHERE pm.province_id = %d AND md.metric_key = 'population_density'
     ORDER BY pm.date DESC
     LIMIT 1",
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
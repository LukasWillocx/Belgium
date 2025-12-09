# Belgium Demographics Explorer

An interactive R Shiny application for exploring population density and name distributions across Belgian municipalities, provinces, and regions for the year of 2025.

## Features

- **Interactive choropleth map** showing population density across 565 Belgian municipalities
- **Hierarchical navigation** through Belgium's administrative structure (Country → Region → Province → Municipality)
- **Top names** for 2025 with adjustable rankings (1-10 names)
- **Dark mode** toggle with automatic map tile switching
- **Responsive panels** that collapse to maximize map viewing area
- **Modern layout** with full screen interactive mapping and minimalist hover panels

## How It Works

The application connects to a PostgreSQL database containing:
- Administrative boundaries (regions, provinces, municipalities) with PostGIS geometries
- Population density metrics at the municipality level
- Name frequency data across the entire population by municipality and gender

Users can drill down from the country level to individual municipalities. The map updates dynamically to show the selected area, and the right panel displays relevant statistics including population density metrics and the most popular names for that geographic area.

## Tech Stack

- **R Shiny** - Web application framework
- **PostgreSQL + PostGIS** - Spatial database
- **Leaflet** - Interactive mapping
- **Simple Features (sf)** - Spatial data handling

## Required R Packages

```r
install.packages(c(
  "shiny",
  "leaflet",
  "shinyjs",
  "markdown",
  "RPostgres",
  "sf"
))
```

## File Structure

```
belgium-demographics-explorer/
│
├── app.R                      # Main application (UI + Server logic)
├── database_functions.R       # Database queries and rendering helpers
├── credentials.R              # Database credentials (not in repo)
├── about.md                   # Content for About modal
│
└── www/
    └── styles.css            # Custom CSS for panels and dark mode
```

### File Descriptions

- **app.R**: Contains the Shiny UI definition and server logic. Handles user interactions, map rendering, and reactive updates based on selections.

- **database_functions.R**: Defines all database queries for fetching regions, provinces, municipalities, metrics, and name data. Also includes helper functions for rendering UI components and map layers.

- **credentials.R**: Stores database connection parameters (dbname, host, port, user, password). This file should be created locally and added to `.gitignore`.

- **about.md**: Markdown file containing information about the application, displayed in a modal dialog.

- **www/styles.css**: Custom styling for the left/right panels, dark mode, metric cards, and name lists with bar charts.

## Database Schema

Key tables:
- `regions` - Three Belgian regions
- `provinces` - Ten provinces with geometries
- `municipalities` - 565 municipalities with MultiPolygon geometries
- `metric_values` - Population density data
- `name_frequencies` - Name frequency across the entire population by municipality

Materialized views provide pre-aggregated data at region and province levels for performance.

## Performance Optimizations

- Geometry simplification for faster map rendering
- Client-side caching of all municipalities on startup
- Materialized views for aggregated metrics
- Spatial indexes on geometry columns

## Hardware requirements
- Very low
- Currently runs on a modern low-power quad core mini computer (Minisforum UN100P)
- Completely single-threaded (no parallelization)

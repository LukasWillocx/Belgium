library(shiny)
library(leaflet)

ui <- fluidPage(
  # Link to external CSS file
  htmltools::htmlDependency(
    name = "custom-styles",
    version = "1.0",
    src = "www",
    stylesheet = "styles.css"
  ),
  
  # Sidebar toggle button
  tags$div(
    class = "sidebar-toggle hidden",
    id = "sidebarToggle",
    icon("bars")
  ),
  
  # Sidebar panel
  tags$div(
    class = "sidebar",
    id = "sidebar",
    tags$button(
      class = "dark-mode-toggle",
      id = "darkModeToggle",
      "🌓"
    ),
    tags$button(
      class = "close-sidebar",
      id = "closeSidebar",
      "×"
    ),
    tags$div(
      class = "dashboard-title",
      "Belgium Living Conditions Monitor"
    ),
    tags$div(
      class = "filter-section",
      tags$h4("Data Layers"),
      checkboxGroupInput(
        "layers",
        NULL,
        choices = c(
          "Demographics" = "demo",
          "Environment" = "env",
          "Public Health" = "health"
        ),
        selected = c()
      )
    ),
    tags$div(
      class = "filter-section",
      tags$h4("Region"),
      selectInput(
        "region",
        NULL,
        choices = c("All Regions", "Flanders", "Wallonia", "Brussels"),
        selected = "All Regions"
      )
    ),
    tags$div(
      class = "filter-section",
      tags$h4("About"),
      tags$p("This dashboard visualizes demographic, environmental, and public health data for Belgium.", 
             style = "font-size: 14px; color: #666;")
    )
  ),
  
  # Full screen map
  leafletOutput("map", width = "100%", height = "100vh"),
  
  # JavaScript for sidebar toggle and dark mode
  tags$script(HTML("
    $(document).ready(function() {
      // Sidebar controls
      $('#closeSidebar').click(function() {
        $('#sidebar').addClass('closed');
        $('#sidebarToggle').removeClass('hidden');
      });
      
      $('#sidebarToggle').click(function() {
        $('#sidebar').removeClass('closed');
        $('#sidebarToggle').addClass('hidden');
      });
      
      // Dark mode toggle
      let darkMode = false;
      $('#darkModeToggle').click(function() {
        darkMode = !darkMode;
        $('body').toggleClass('dark-mode');
        
        // Send dark mode state to Shiny
        Shiny.setInputValue('darkModeState', darkMode);
      });
    });
  "))
)

server <- function(input, output, session) {
  
  # Reactive value to track dark mode
  darkMode <- reactiveVal(FALSE)
  
  # Observe dark mode changes from JavaScript
  observeEvent(input$darkModeState, {
    darkMode(input$darkModeState)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 4.5, lat = 50.5, zoom = 9)
  })
  
  # Update map tiles when dark mode changes
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
}

shinyApp(ui, server)
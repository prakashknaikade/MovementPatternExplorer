# ShinyModule.R
# This is the entry point for your MoveApps R Shiny app.
# It defines the UI and server logic following the MoveApps SDK template.

library("shiny")
library("move2")   # Provides functions for MoveStack I/O.
library("sf")
library("leaflet")
library("ggplot2")
library("plotly")
library("dplyr")
library("units")   # To handle unitized numeric values


# Determine the identifier column: try "individual_id" then "individual_name_deployment_id".
get_id_column <- function(df) {
  possible_ids <- c("individual_id", "individual_name_deployment_id")
  found_col <- possible_ids[possible_ids %in% colnames(df)]
  if (length(found_col) == 0) {
    stop("No valid individual identifier column found. Check your data structure.")
  }
  return(found_col[1])
}

#### UI Function ####

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  tagList(
    titlePanel("Movement Pattern Explorer"),
    sidebarLayout(
      sidebarPanel(
        # Dynamically populated individual selector.
        uiOutput(ns("uiIndiv")),
        # Date range input; will be updated based on data.
        dateRangeInput(ns("date_range"), "Select Date Range",
                       start = Sys.Date() - 30, end = Sys.Date()),
        actionButton(ns("analyze"), "Analyze Patterns")
      ),
      mainPanel(
        leafletOutput(ns("map"), height = "700px"),
        plotlyOutput(ns("speed_plot")),
        plotlyOutput(ns("altitude_plot")),
        verbatimTextOutput(ns("summary"))
      )
    )
  )
}

#### Server Function ####

# 'data' is the MoveStack input passed by MoveApps.
# For local testing, if data is NULL, we simulate it.
shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  
  # Preserve the sf object so that geometry remains intact.
  df_all <- data
  df_all_df <- as.data.frame(df_all)
  id_col <- get_id_column(df_all_df)
  
  # Update the date range input based on the data's timestamp range.
  observe({
    if ("timestamp" %in% colnames(df_all_df)) {
      ts <- as.POSIXct(df_all_df$timestamp)
      updateDateRangeInput(session, "date_range",
                           start = as.Date(min(ts, na.rm = TRUE)),
                           end = as.Date(max(ts, na.rm = TRUE)))
    }
  })
  
  # Render dropdown for individual selection.
  output$uiIndiv <- renderUI({
    selectInput(ns("individual"), "Select Individual",
                choices = unique(df_all_df[[id_col]]),
                selected = unique(df_all_df[[id_col]])[1])
  })
  
  # Reactive expression: Filter the sf object based on selected individual and date range.
  filtered_data <- reactive({
    req(input$individual, input$date_range)
    df <- df_all %>% 
      filter((!!as.name(id_col)) == input$individual,
             as.POSIXct(timestamp) >= as.POSIXct(input$date_range[1]),
             as.POSIXct(timestamp) <= as.POSIXct(input$date_range[2]))
    if (nrow(df) == 0) return(NULL)
    return(df)
  })
  
  #### Map Output ####
  output$map <- renderLeaflet({
    df <- filtered_data()
    req(!is.null(df))
    # Create a LINESTRING from points if more than one exists.
    df_line <- NULL
    if (nrow(df) > 1) {
      df_ordered <- df %>% arrange(as.POSIXct(timestamp))
      df_line <- st_combine(df_ordered$geometry) %>% st_cast("LINESTRING")
    }
    leaflet(df) %>%
      addTiles() %>%
      addMarkers(popup = ~paste("Time:", timestamp)) %>%
      { if (!is.null(df_line)) addPolylines(., data = df_line, color = "blue", weight = 2) else . }
  })
  
  #### Speed Plot Output ####
  output$speed_plot <- renderPlotly({
    df <- filtered_data()
    req(!is.null(df))
    df_df <- as.data.frame(df)
    
    # If ground_speed is all NA, compute speed from consecutive points.
    if ("ground_speed" %in% colnames(df_df) && all(is.na(df_df$ground_speed))) {
      df_ordered <- df %>% arrange(as.POSIXct(timestamp))
      n <- nrow(df_ordered)
      computed_speed <- rep(NA, n)
      if (n > 1) {
        geoms <- st_geometry(df_ordered)
        times <- as.POSIXct(df_ordered$timestamp)
        computed_speed[1] <- NA
        for(i in 2:n){
          d <- as.numeric(st_distance(geoms[i], geoms[i-1]))  # distance in meters
          dt <- as.numeric(difftime(times[i], times[i-1], units="secs"))
          computed_speed[i] <- ifelse(dt > 0, d / dt, NA)
        }
      }
      df_ordered$computed_speed <- computed_speed
      df_df <- as.data.frame(df_ordered)
      ycol <- "computed_speed"
      ylab <- "Computed Ground Speed (m/s)"
    } else if("ground_speed" %in% colnames(df_df)) {
      ycol <- "ground_speed"
      ylab <- "Ground Speed (m/s)"
    } else {
      stop("No ground speed information available.")
    }
    
    p <- ggplot(df_df, aes(x = as.POSIXct(timestamp), y = .data[[ycol]])) +
      geom_line(color = "red") +
      labs(title = ylab, x = "Time", y = ylab)
    ggplotly(p)
  })
  
  #### Altitude Plot Output ####
  output$altitude_plot <- renderPlotly({
    df <- filtered_data()
    req(!is.null(df))
    df_df <- as.data.frame(df)
    # If altitude data are missing or all NA, show a message.
    if("height_above_ellipsoid" %in% colnames(df_df) && all(is.na(df_df$height_above_ellipsoid))){
      # Create dummy data with a midpoint timestamp.
      mid_time <- as.POSIXct(mean(as.numeric(input$date_range)), origin = "1970-01-01")
      dummy <- data.frame(x = mid_time, y = 0)
      p <- ggplot(dummy, aes(x = x, y = y)) +
        geom_text(label = "No altitude data available", size = 6, hjust = 0.5) +
        scale_x_datetime(expand = expansion(mult = 0.1)) +
        theme_void()
    } else if("height_above_ellipsoid" %in% colnames(df_df)) {
      p <- ggplot(df_df, aes(x = as.POSIXct(timestamp), y = height_above_ellipsoid)) +
        geom_line(color = "green") +
        labs(title = "Altitude Over Time", x = "Time", y = "Altitude (m)")
    } else {
      stop("No altitude data available.")
    }
    ggplotly(p)
  })
  
  #### Summary Output ####
  output$summary <- renderPrint({
    df <- filtered_data()
    req(!is.null(df))
    summary(as.data.frame(df))
  })
  
  
  # Return the unmodified input data (required by the template).
  return(reactive({ data }))
}

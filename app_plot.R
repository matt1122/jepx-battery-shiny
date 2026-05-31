# app_plot.R

library(shiny)
library(jsonlite)
library(httr2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DT)
library(bslib)
library(plotly)

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

# Public read API for general users
API_BASE_URL <- "http://167.172.64.188:9000/JEPX_forecast"

# ------------------------------------------------------------
# Data loading
# ------------------------------------------------------------

fetch_forecast <- function(api_key) {
  api_key <- trimws(api_key)

  if (!nzchar(api_key)) {
    stop("API key is required.")
  }

  resp <- request(API_BASE_URL) |>
    req_url_query(api_key = api_key) |>
    req_timeout(30) |>
    req_perform()

  status <- resp_status(resp)
  body <- resp_body_string(resp)

  if (status != 200) {
    msg <- tryCatch({
      parsed <- jsonlite::fromJSON(body)
      if (!is.null(parsed$error)) parsed$error else body
    }, error = function(e) body)

    stop("API request failed. HTTP status: ", status, ". ", msg)
  }

  payload <- jsonlite::fromJSON(body, flatten = TRUE)

  updated_at <- payload$updated_at
  if (is.null(updated_at)) {
    updated_at <- NA_character_
  } else {
    updated_at <- as.character(unlist(updated_at))[1]
  }

  df_raw <- payload$data

  # Remove bad-name columns if any.
  bad_names <- is.na(names(df_raw)) | !nzchar(names(df_raw))
  if (any(bad_names)) {
    df_raw <- df_raw[, !bad_names, drop = FALSE]
  }

  df <- as_tibble(df_raw)

  if (!"DateTime" %in% names(df)) {
    stop("DateTime column was not found in API response.")
  }

  df <- df |>
    mutate(DateTime = ymd_hms(DateTime, tz = "Asia/Tokyo", quiet = TRUE))

  price_cols <- setdiff(names(df), "DateTime")

  df <- df |>
    mutate(across(all_of(price_cols), as.numeric))

  list(
    updated_at = updated_at,
    data = df
  )
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- page_sidebar(
  title = "JEPX Price Forecast Viewer",

  sidebar = sidebar(
    width = 360,

    passwordInput(
      "api_key",
      "API key",
      placeholder = "Enter your API key"
    ),

    actionButton(
      "load_data",
      "Load forecast data",
      class = "btn-primary"
    ),

    tags$div(style = "height: 10px;"),

    uiOutput("area_ui"),
    uiOutput("date_range_ui"),

    checkboxInput(
      "show_points",
      "Show points",
      value = FALSE
    ),

    checkboxInput(
      "free_y",
      "Use free y-axis range",
      value = TRUE
    )
  ),

  tags$div(
    style = "
      font-size: 0.90rem;
      color: #555;
      line-height: 1.35;
      margin-top: -6px;
      margin-bottom: 14px;
      max-width: 1100px;
    ",
    tags$p(
      style = "margin-bottom: 4px;",
      "This viewer plots the latest JEPX day-ahead electricity price forecasts."
    ),
    tags$p(
      style = "margin-bottom: 0;",
      "Select areas and a date range to inspect the forecast series. The plot can also be zoomed interactively."
    )
  ),

  layout_columns(
    col_widths = c(8, 4),

    card(
      card_header("Forecast price series"),
      plotlyOutput("forecast_plot", height = "560px")
    ),

    card(
      card_header("Summary"),
      verbatimTextOutput("summary_info")
    )
  ),

  card(
    card_header("Data table"),
    DTOutput("data_table")
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  forecast <- reactiveVal(NULL)
  load_error <- reactiveVal(NULL)

  observeEvent(input$load_data, {
    tryCatch({
      x <- fetch_forecast(input$api_key)

      forecast(x)
      load_error(NULL)

      showNotification("Forecast data loaded.", type = "message")
    }, error = function(e) {
      forecast(NULL)
      load_error(conditionMessage(e))
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  output$area_ui <- renderUI({
    x <- forecast()
    req(x)

    areas <- setdiff(names(x$data), "DateTime")

    checkboxGroupInput(
      "areas",
      "Areas to display",
      choices = areas,
      selected = areas
    )
  })

  output$date_range_ui <- renderUI({
    x <- forecast()
    req(x)

    dates <- sort(unique(as.Date(x$data$DateTime, tz = "Asia/Tokyo")))

    dateRangeInput(
      "date_range",
      "Date range",
      start = min(dates, na.rm = TRUE),
      end = max(dates, na.rm = TRUE),
      min = min(dates, na.rm = TRUE),
      max = max(dates, na.rm = TRUE),
      format = "yyyy-mm-dd"
    )
  })

  filtered_wide <- reactive({
    x <- forecast()
    req(x, input$areas, input$date_range)

    if (length(input$areas) == 0) {
      return(x$data[0, , drop = FALSE])
    }

    start_date <- as.Date(input$date_range[1])
    end_date <- as.Date(input$date_range[2])

    x$data |>
      filter(
        as.Date(DateTime, tz = "Asia/Tokyo") >= start_date,
        as.Date(DateTime, tz = "Asia/Tokyo") <= end_date
      ) |>
      select(DateTime, all_of(input$areas))
  })

  filtered_long <- reactive({
    df <- filtered_wide()
    req(nrow(df) > 0)

    df |>
      pivot_longer(
        cols = -DateTime,
        names_to = "area",
        values_to = "price"
      ) |>
      filter(!is.na(price))
  })

  output$summary_info <- renderPrint({
    if (!is.null(load_error())) {
      cat("Error:", load_error(), "\n")
      return()
    }

    x <- forecast()
    if (is.null(x)) {
      cat("Data not loaded.\n")
      return()
    }

    df <- filtered_wide()

    cat("Forecasted at:", x$updated_at, "\n")

    if (!is.null(input$date_range)) {
      cat("Date range:", as.character(input$date_range[1]), "to", as.character(input$date_range[2]), "\n")
    }

    if (!is.null(input$areas) && length(input$areas) > 0) {
      cat("Areas:", paste(input$areas, collapse = ", "), "\n")
    } else {
      cat("Areas: None\n")
    }

    cat("Rows:", nrow(df), "\n")
  })

  output$forecast_plot <- renderPlotly({
    df <- filtered_long()
    req(nrow(df) > 0)

    y_limits <- NULL
    if (!isTRUE(input$free_y)) {
      y_limits <- range(df$price, na.rm = TRUE)
    }

    p <- ggplot(
      df,
      aes(
        x = DateTime,
        y = price,
        color = area,
        text = paste0(
          "Time: ", format(DateTime, "%Y-%m-%d %H:%M"), "<br>",
          "Area: ", area, "<br>",
          "Price: ", round(price, 2)
        )
      )
    ) +
      geom_line(linewidth = 0.9)

    if (isTRUE(input$show_points)) {
      p <- p + geom_point(size = 1.4, alpha = 0.85)
    }

    p <- p +
      labs(
        title = "JEPX price forecast",
        subtitle = paste0("Forecasted at: ", forecast()$updated_at),
        x = "Time",
        y = "Forecast price",
        color = "Area",
        caption = "Drag to zoom. Double-click to reset the view."
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )

    if (!is.null(y_limits) && all(is.finite(y_limits))) {
      p <- p + coord_cartesian(ylim = y_limits)
    }

    ggplotly(p, tooltip = "text") |>
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.2
        ),
        margin = list(b = 90)
      )
  })

  output$data_table <- renderDT({
    df <- filtered_wide()
    req(nrow(df) > 0)

    df |>
      mutate(
        DateTime = format(DateTime, "%Y-%m-%d %H:%M:%S")
      ) |>
      mutate(across(-DateTime, ~ round(.x, 2))) |>
      datatable(
        options = list(
          pageLength = 12,
          scrollX = TRUE
        )
      )
  })
}

shinyApp(ui, server)

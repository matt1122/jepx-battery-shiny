# app.R

library(shiny)
library(jsonlite)
library(httr2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DT)
library(bslib)

API_BASE_URL <- "http://167.172.64.188:9000/JEPX_forecast"

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

  df <- as_tibble(payload$data)

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

optimize_binary_soc <- function(df, area, efficiency = 0.90, capacity = 1) {
  x <- df |>
    select(DateTime, price = all_of(area)) |>
    filter(!is.na(DateTime), !is.na(price)) |>
    arrange(DateTime)

  n <- nrow(x)
  if (n == 0) {
    stop("No valid price data for the selected area and date.")
  }

  # State:
  #   0 = empty
  #   1 = full
  #
  # Assumptions:
  #   Start SoC = 0
  #   End SoC   = 0
  #   SoC is binary: 0 or 1
  #   Multiple cycles per day are allowed
  #   One time slot can either charge, discharge, or idle
  #
  # Profit:
  #   charge:    - price * capacity
  #   discharge: + price * efficiency * capacity
  #
  # efficiency is treated as round-trip efficiency.

  neg_inf <- -1e100

  v0 <- rep(neg_inf, n + 1)
  v1 <- rep(neg_inf, n + 1)

  prev0 <- rep(NA_integer_, n + 1)
  prev1 <- rep(NA_integer_, n + 1)

  act0 <- rep(NA_character_, n + 1)
  act1 <- rep(NA_character_, n + 1)

  v0[1] <- 0
  v1[1] <- neg_inf

  for (t in seq_len(n)) {
    p <- x$price[t]

    # End this slot empty:
    stay_empty <- v0[t]
    discharge <- v1[t] + efficiency * p * capacity

    if (discharge > stay_empty) {
      v0[t + 1] <- discharge
      prev0[t + 1] <- 1L
      act0[t + 1] <- "discharge"
    } else {
      v0[t + 1] <- stay_empty
      prev0[t + 1] <- 0L
      act0[t + 1] <- "idle"
    }

    # End this slot full:
    hold_full <- v1[t]
    charge <- v0[t] - p * capacity

    if (charge > hold_full) {
      v1[t + 1] <- charge
      prev1[t + 1] <- 0L
      act1[t + 1] <- "charge"
    } else {
      v1[t + 1] <- hold_full
      prev1[t + 1] <- 1L
      act1[t + 1] <- "hold"
    }
  }

  # Force final SoC = 0
  state <- 0L
  actions <- rep("idle", n)

  for (i in seq(n + 1, 2)) {
    t <- i - 1

    if (state == 0L) {
      actions[t] <- act0[i]
      state <- prev0[i]
    } else {
      actions[t] <- act1[i]
      state <- prev1[i]
    }
  }

  out <- x |>
    mutate(
      action = actions,
      cash_flow = case_when(
        action == "charge" ~ -price * capacity,
        action == "discharge" ~ efficiency * price * capacity,
        TRUE ~ 0
      ),
      cumulative_profit = cumsum(cash_flow)
    )

  soc <- numeric(n)
  current_soc <- 0

  for (t in seq_len(n)) {
    if (out$action[t] == "charge") {
      current_soc <- 1
    } else if (out$action[t] == "discharge") {
      current_soc <- 0
    }
    soc[t] <- current_soc
  }

  out$soc <- soc

  list(
    schedule = out,
    total_profit = sum(out$cash_flow),
    cycles = sum(out$action == "discharge")
  )
}

ui <- page_sidebar(
  title = "JEPX Forecast Battery Simulation",

  sidebar = sidebar(
    width = 340,

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

    hr(),

    textOutput("updated_at"),

    uiOutput("area_ui"),
    uiOutput("date_ui"),

    sliderInput(
      "efficiency",
      "Round-trip efficiency",
      min = 0.50,
      max = 1.00,
      value = 0.90,
      step = 0.01
    ),

    numericInput(
      "capacity",
      "Capacity",
      value = 1,
      min = 0.01,
      step = 0.1
    ),

    helpText(
      "Assumptions: binary SoC, start SoC = 0, end SoC = 0, multiple cycles per day allowed."
    )
  ),

  layout_columns(
    col_widths = c(8, 4),

    card(
      card_header("Optimal charge/discharge timing"),
      plotOutput("dispatch_plot", height = "540px")
    ),

    card(
      card_header("Summary"),
      verbatimTextOutput("summary_profit")
    )
  ),

  card(
    card_header("Schedule table"),
    DTOutput("schedule_table")
  )
)

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

  output$updated_at <- renderText({
    if (!is.null(load_error())) {
      return(paste("Error:", load_error()))
    }

    x <- forecast()
    if (is.null(x)) {
      return("Data not loaded.")
    }

    paste("updated_at:", x$updated_at)
  })

  output$area_ui <- renderUI({
    x <- forecast()
    req(x)

    areas <- setdiff(names(x$data), "DateTime")

    selectInput(
      "area",
      "Area",
      choices = areas,
      selected = if ("Tokyo" %in% areas) "Tokyo" else areas[1]
    )
  })

  output$date_ui <- renderUI({
    x <- forecast()
    req(x)

    available_dates <- sort(unique(as.Date(x$data$DateTime, tz = "Asia/Tokyo")))

    selectInput(
      "target_date",
      "Target date",
      choices = as.character(available_dates),
      selected = as.character(available_dates[1])
    )
  })

  day_data <- reactive({
    x <- forecast()
    req(x, input$area, input$target_date)

    target_date <- as.Date(input$target_date)

    x$data |>
      filter(as.Date(DateTime, tz = "Asia/Tokyo") == target_date)
  })

  optimization <- reactive({
    df <- day_data()
    req(nrow(df) > 0)

    optimize_binary_soc(
      df = df,
      area = input$area,
      efficiency = input$efficiency,
      capacity = input$capacity
    )
  })

  output$summary_profit <- renderPrint({
    opt <- optimization()

    cat("Area:", input$area, "\n")
    cat("Date:", input$target_date, "\n")
    cat("Efficiency:", input$efficiency, "\n")
    cat("Capacity:", input$capacity, "\n")
    cat("Cycles:", opt$cycles, "\n")
    cat("Profit:", round(opt$total_profit, 2), "\n")
  })

  output$dispatch_plot <- renderPlot({
    opt <- optimization()
    sch <- opt$schedule

    charge_points <- sch |> filter(action == "charge")
    discharge_points <- sch |> filter(action == "discharge")

    max_price <- max(sch$price, na.rm = TRUE)
    min_price <- min(sch$price, na.rm = TRUE)

    ggplot(sch, aes(x = DateTime)) +
      geom_line(aes(y = price), linewidth = 0.9) +
      geom_point(
        data = charge_points,
        aes(y = price),
        size = 3,
        shape = 25,
        fill = "blue"
      ) +
      geom_point(
        data = discharge_points,
        aes(y = price),
        size = 3,
        shape = 24,
        fill = "red"
      ) +
      geom_step(
        aes(y = min_price + soc * (max_price - min_price)),
        linewidth = 0.6,
        alpha = 0.6,
        linetype = "dashed"
      ) +
      labs(
        title = "Optimal charge/discharge timing",
        subtitle = paste0(
          "updated_at: ", forecast()$updated_at,
          " | efficiency: ", input$efficiency,
          " | profit: ", round(opt$total_profit, 2)
        ),
        x = "Time",
        y = "Forecast price",
        caption = "Blue downward triangles: charge. Red upward triangles: discharge. Dashed line: SoC scaled to the price axis."
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold")
      )
  })

  output$schedule_table <- renderDT({
    opt <- optimization()

    opt$schedule |>
      mutate(
        DateTime = format(DateTime, "%Y-%m-%d %H:%M:%S"),
        price = round(price, 2),
        cash_flow = round(cash_flow, 2),
        cumulative_profit = round(cumulative_profit, 2)
      ) |>
      datatable(
        options = list(
          pageLength = 12,
          scrollX = TRUE
        )
      )
  })
}

shinyApp(ui, server)

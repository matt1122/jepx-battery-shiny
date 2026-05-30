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

PERCENTILE_SHEET_ID <- "1-qiaLnJ-7rH9yutJKIw29C_lDv5bF_bhsatCYAyNTTI"

PERCENTILE_SHEET_NAME <- "percentile"

read_percentile_sheet <- function() {
  csv_url <- paste0(
    "https://docs.google.com/spreadsheets/d/",
    PERCENTILE_SHEET_ID,
    "/gviz/tq?tqx=out:csv&sheet=",
    URLencode(PERCENTILE_SHEET_NAME, reserved = TRUE),
    "&range=A:M"
  )
  
  pct <- read.csv(
    csv_url,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  required_cols <- c(
    "Area", "Hour",
    "0", "0.1", "0.2", "0.3", "0.4", "0.5",
    "0.6", "0.7", "0.8", "0.9", "1"
  )
  
  # A:M の13列を使う前提で、列名をR側で強制的に修正する
  if (ncol(pct) < length(required_cols)) {
    stop(
      "Percentile sheet has fewer columns than expected. Expected 13 columns A:M, got ",
      ncol(pct), "."
    )
  }
  
  pct <- pct[, seq_along(required_cols), drop = FALSE]
  names(pct) <- required_cols
  
  # 完全な空行があれば除去
  pct <- pct |>
    filter(
      !is.na(Area),
      nzchar(trimws(as.character(Area))),
      !is.na(Hour)
    )
  
  pct <- pct |>
    mutate(
      Area = trimws(as.character(Area)),
      Hour = as.integer(Hour)
    )
  
  quantile_cols <- setdiff(names(pct), c("Area", "Hour"))
  
  pct <- pct |>
    mutate(across(all_of(quantile_cols), as.numeric))
  
  # 確認：4エリア × 24時間 = 96行が期待値
  if (nrow(pct) != 96) {
    warning(
      "Percentile sheet has ", nrow(pct),
      " data rows. Expected 96 rows for 4 areas x 24 hours."
    )
  }
  
  pct
}

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

q_col <- function(x) {
  if (x == 0) return("0")
  if (x == 1) return("1")
  sprintf("%.1f", x)
}

make_guardband_prices <- function(df, area, percentile_df, strategy) {
  x <- df |>
    select(DateTime, forecast_price = all_of(area)) |>
    filter(!is.na(DateTime), !is.na(forecast_price)) |>
    arrange(DateTime) |>
    mutate(
      Area = area,
      Hour = hour(DateTime)
    )
  
  if (strategy == "base") {
    return(
      x |>
        mutate(
          buy_price = forecast_price,
          sell_price = forecast_price,
          guard_band = "Base"
        )
    )
  }
  
  p <- as.numeric(strategy) / 100
  
  lower_col <- q_col(0.5 - p)
  median_col <- q_col(0.5)
  upper_col <- q_col(0.5 + p)
  
  needed <- c("Area", "Hour", lower_col, median_col, upper_col)
  missing_cols <- setdiff(needed, names(percentile_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Percentile sheet is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  pct_sub <- percentile_df |>
    select(
      Area,
      Hour,
      q_lower = all_of(lower_col),
      q_median = all_of(median_col),
      q_upper = all_of(upper_col)
    )
  
  out <- x |>
    left_join(pct_sub, by = c("Area", "Hour"))
  
  if (any(is.na(out$q_lower) | is.na(out$q_median) | is.na(out$q_upper))) {
    stop("Some percentile values are missing after joining by Area and Hour.")
  }
  
  # User-specified conservative band:
  # P10:
  #   sell = forecast - (q50 - q40)
  #   buy  = forecast + (q60 - q50)
  #
  # P20/P30/P40 are defined analogously.
  out |>
    mutate(
      sell_adjustment = q_median - q_lower,
      buy_adjustment = q_upper - q_median,
      sell_price = forecast_price - sell_adjustment,
      buy_price = forecast_price + buy_adjustment,
      guard_band = paste0("P", strategy)
    )
}

optimize_binary_soc <- function(price_df, efficiency = 0.90, capacity = 1) {
  x <- price_df |>
    filter(
      !is.na(DateTime),
      !is.na(forecast_price),
      !is.na(buy_price),
      !is.na(sell_price)
    ) |>
    arrange(DateTime)
  
  n <- nrow(x)
  if (n == 0) {
    stop("No valid price data for the selected area/date.")
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
  #   Charge uses conservative buy_price
  #   Discharge uses conservative sell_price
  #
  # efficiency is treated as round-trip efficiency on the discharge side.
  
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
    buy_t <- x$buy_price[t]
    sell_t <- x$sell_price[t]
    
    # End this slot empty:
    stay_empty <- v0[t]
    discharge <- v1[t] + efficiency * sell_t * capacity
    
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
    charge <- v0[t] - buy_t * capacity
    
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
        action == "charge" ~ -buy_price * capacity,
        action == "discharge" ~ efficiency * sell_price * capacity,
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
    width = 350,
    
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
    
    selectInput(
      "strategy",
      "Guard band strategy",
      choices = c(
        "Base" = "base",
        "P10" = "10",
        "P20" = "20",
        "P30" = "30",
        "P40" = "40"
      ),
      selected = "base"
    ),
    
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
      "Guard bands use recent forecast-error percentile differences by area and hour. Assumptions: binary SoC, start SoC = 0, end SoC = 0, multiple cycles per day allowed."
    )
  ),
  
  layout_columns(
    col_widths = c(8, 4),
    
    card(
      card_header("Optimal charge/discharge timing"),
      plotOutput("dispatch_plot", height = "560px")
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
  percentile_data <- reactiveVal(NULL)
  load_error <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    tryCatch({
      x <- fetch_forecast(input$api_key)
      pct <- read_percentile_sheet()
      
      forecast(x)
      percentile_data(pct)
      load_error(NULL)
      
      showNotification("Forecast and percentile data loaded.", type = "message")
    }, error = function(e) {
      forecast(NULL)
      percentile_data(NULL)
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
  
  day_forecast <- reactive({
    x <- forecast()
    req(x, input$area, input$target_date)
    
    target_date <- as.Date(input$target_date)
    
    x$data |>
      filter(as.Date(DateTime, tz = "Asia/Tokyo") == target_date)
  })
  
  guardband_prices <- reactive({
    df <- day_forecast()
    pct <- percentile_data()
    req(df, pct, input$area, input$strategy)
    
    make_guardband_prices(
      df = df,
      area = input$area,
      percentile_df = pct,
      strategy = input$strategy
    )
  })
  
  optimization <- reactive({
    price_df <- guardband_prices()
    req(nrow(price_df) > 0)
    
    optimize_binary_soc(
      price_df = price_df,
      efficiency = input$efficiency,
      capacity = input$capacity
    )
  })
  
  output$summary_profit <- renderPrint({
    opt <- optimization()
    
    cat("Area:", input$area, "\n")
    cat("Date:", input$target_date, "\n")
    cat("Guard band:", ifelse(input$strategy == "base", "Base", paste0("P", input$strategy)), "\n")
    cat("Efficiency:", input$efficiency, "\n")
    cat("Capacity:", input$capacity, "\n")
    cat("Cycles:", opt$cycles, "\n")
    cat("Profit based on scheduling inputs:", round(opt$total_profit, 2), "\n")
  })
  
  output$dispatch_plot <- renderPlot({
    opt <- optimization()
    sch <- opt$schedule
    
    charge_points <- sch |> filter(action == "charge")
    discharge_points <- sch |> filter(action == "discharge")
    
    max_y <- max(c(sch$forecast_price, sch$buy_price, sch$sell_price), na.rm = TRUE)
    min_y <- min(c(sch$forecast_price, sch$buy_price, sch$sell_price), na.rm = TRUE)
    
    ggplot(sch, aes(x = DateTime)) +
      geom_ribbon(
        aes(ymin = sell_price, ymax = buy_price),
        alpha = 0.18
      ) +
      geom_line(aes(y = forecast_price), linewidth = 0.9) +
      geom_line(aes(y = buy_price), linewidth = 0.5, linetype = "dashed") +
      geom_line(aes(y = sell_price), linewidth = 0.5, linetype = "dashed") +
      geom_point(
        data = charge_points,
        aes(y = buy_price),
        size = 3,
        shape = 25,
        fill = "blue"
      ) +
      geom_point(
        data = discharge_points,
        aes(y = sell_price),
        size = 3,
        shape = 24,
        fill = "red"
      ) +
      geom_step(
        aes(y = min_y + soc * (max_y - min_y)),
        linewidth = 0.6,
        alpha = 0.6,
        linetype = "dotted"
      ) +
      labs(
        title = "Optimal charge/discharge timing with guard bands",
        subtitle = paste0(
          "updated_at: ", forecast()$updated_at,
          " | strategy: ", ifelse(input$strategy == "base", "Base", paste0("P", input$strategy)),
          " | efficiency: ", input$efficiency,
          " | profit: ", round(opt$total_profit, 2)
        ),
        x = "Time",
        y = "Price",
        caption = "Solid line: point forecast. Dashed lines/ribbon: conservative buy/sell inputs. Blue: charge. Red: discharge. Dotted line: SoC scaled to price axis."
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
        forecast_price = round(forecast_price, 2),
        buy_price = round(buy_price, 2),
        sell_price = round(sell_price, 2),
        cash_flow = round(cash_flow, 2),
        cumulative_profit = round(cumulative_profit, 2)
      ) |>
      select(
        DateTime,
        forecast_price,
        buy_price,
        sell_price,
        action,
        soc,
        cash_flow,
        cumulative_profit
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

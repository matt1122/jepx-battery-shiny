# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- page_sidebar(
  title = tags$div(
    tags$div(
      "JEPX Forecast Battery Simulation",
      style = "font-size: 1.75rem; font-weight: 600; margin-bottom: 6px;"
    ),
    tags$div(
      "Select an area, date, round-trip efficiency, and guard-band strategy to compute an optimal charge/discharge schedule based on JEPX price forecasts.",
      style = "font-size: 0.90rem; font-weight: 400; color: #555; line-height: 1.35; margin-bottom: 4px;"
    ),
    tags$div(
      "Guard bands make the buy-side price and sell-side price more conservative in order to reduce excessive trades caused by forecast errors.",
      style = "font-size: 0.90rem; font-weight: 400; color: #555; line-height: 1.35;"
    )
  ),

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

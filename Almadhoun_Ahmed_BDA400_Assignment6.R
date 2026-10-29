# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase
# Student: Ahmed Almadhoun
# Description: Interactive stock portfolio dashboard using Yahoo Finance data,
# ggplot2 visualizations, technical indicators, and moving-average trading signals.
#
# Before running for the first time, install the required packages:
# install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "dplyr"))
#
# Run this file in RStudio, then click "Run App" or use shiny::runApp().

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)

# ---- Helper: retrieve and prepare stock data ----
get_stock_data <- function(symbol, start_date, end_date, timeframe) {
  raw_data <- tryCatch(
    getSymbols(
      Symbols = toupper(symbol),
      src = "yahoo",
      from = as.Date(start_date),
      to = as.Date(end_date) + 1,
      auto.assign = FALSE,
      warnings = FALSE
    ),
    error = function(e) {
      stop(paste(
        "Could not download stock data. Check the ticker symbol, dates,",
        "and internet connection. Yahoo Finance message:", e$message
      ))
    }
  )

  if (NROW(raw_data) < 2) {
    stop("Not enough stock data for the selected date range.")
  }

  # Convert daily observations to weekly or monthly OHLC data when selected.
  if (timeframe == "Weekly") {
    raw_data <- to.weekly(raw_data, indexAt = "lastof", name = NULL)
  } else if (timeframe == "Monthly") {
    raw_data <- to.monthly(raw_data, indexAt = "lastof", name = NULL)
  }

  data.frame(
    Date = as.Date(index(raw_data)),
    Open = as.numeric(Op(raw_data)),
    High = as.numeric(Hi(raw_data)),
    Low = as.numeric(Lo(raw_data)),
    Close = as.numeric(Cl(raw_data)),
    Volume = as.numeric(Vo(raw_data))
  ) |>
    filter(!is.na(Date), !is.na(Close)) |>
    arrange(Date)
}

# ---- User interface ----
ui <- fluidPage(
  titlePanel("Stock Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock ticker:", value = "AAPL"),
      dateRangeInput(
        "date_range",
        "Select date range:",
        start = Sys.Date() - 365,
        end = Sys.Date(),
        max = Sys.Date()
      ),
      selectInput(
        "time_frame",
        "Select time frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      selectInput(
        "chart_type",
        "Stock chart type:",
        choices = c("Line", "Area", "Candlestick"),
        selected = "Line"
      ),
      checkboxGroupInput(
        "technical_indicators",
        "Technical indicators:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = c("Moving Averages", "RSI", "MACD")
      ),
      checkboxInput("show_signals", "Show Buy/Sell signals", value = TRUE),
      helpText("Example ticker symbols: AAPL, MSFT, GOOGL, TSLA.")
    ),
    mainPanel(
      h4(textOutput("chart_title")),
      textOutput("status"),
      plotOutput("stock_chart", height = "430px"),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('RSI') >= 0",
        h4("Relative Strength Index (RSI)"),
        plotOutput("rsi_chart", height = "220px")
      ),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('MACD') >= 0",
        h4("Moving Average Convergence Divergence (MACD)"),
        plotOutput("macd_chart", height = "220px")
      ),
      h4("Latest trading signal"),
      tableOutput("latest_signal")
    )
  )
)

# ---- Server logic ----
server <- function(input, output, session) {

  stock_data <- reactive({
    req(input$symbol, input$date_range)
    validate(
      need(input$date_range[1] <= input$date_range[2],
           "The start date must be on or before the end date.")
    )
    get_stock_data(
      symbol = input$symbol,
      start_date = input$date_range[1],
      end_date = input$date_range[2],
      timeframe = input$time_frame
    )
  })

  analysis_data <- reactive({
    df <- stock_data()
    validate(need(nrow(df) >= 2, "Not enough observations to calculate indicators."))

    df$MA20 <- as.numeric(SMA(df$Close, n = 20))
    df$MA50 <- as.numeric(SMA(df$Close, n = 50))
    df$RSI <- as.numeric(RSI(df$Close, n = 14))

    macd_result <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9)
    df$MACD <- as.numeric(macd_result[, 1])
    df$MACD_Signal <- as.numeric(macd_result[, 2])
    df$MACD_Histogram <- df$MACD - df$MACD_Signal

    # A signal is generated only when the short MA crosses the long MA.
    previous_short <- dplyr::lag(df$MA20)
    previous_long <- dplyr::lag(df$MA50)
    df$Signal <- "Hold"
    buy <- !is.na(df$MA20) & !is.na(df$MA50) &
      !is.na(previous_short) & !is.na(previous_long) &
      previous_short <= previous_long & df$MA20 > df$MA50
    sell <- !is.na(df$MA20) & !is.na(df$MA50) &
      !is.na(previous_short) & !is.na(previous_long) &
      previous_short >= previous_long & df$MA20 < df$MA50
    df$Signal[buy] <- "Buy"
    df$Signal[sell] <- "Sell"
    df
  })

  output$chart_title <- renderText({
    paste(toupper(input$symbol), "- Stock Price and Trading Signals")
  })

  output$status <- renderText({
    paste("Data source: Yahoo Finance | Time frame:", input$time_frame)
  })

  output$stock_chart <- renderPlot({
    df <- analysis_data()
    validate(need(nrow(df) > 0, "No data available for this selection."))

    p <- ggplot(df, aes(x = Date))

    if (input$chart_type == "Candlestick") {
      p <- p +
        geom_segment(aes(y = Low, yend = High, xend = Date), color = "grey35") +
        geom_rect(
          aes(
            xmin = Date - 0.35, xmax = Date + 0.35,
            ymin = pmin(Open, Close), ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          color = "grey25",
          na.rm = TRUE
        ) +
        scale_fill_manual(
          values = c("TRUE" = "darkseagreen3", "FALSE" = "indianred2"),
          labels = c("TRUE" = "Up", "FALSE" = "Down"),
          name = "Session"
        )
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), fill = "steelblue", alpha = 0.25) +
        geom_line(aes(y = Close), color = "steelblue", linewidth = 0.6)
    } else {
      p <- p + geom_line(aes(y = Close), color = "steelblue", linewidth = 0.7)
    }

    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = MA20, color = "MA 20"), linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = MA50, color = "MA 50"), linewidth = 0.8, na.rm = TRUE) +
        scale_color_manual(
          values = c("MA 20" = "darkorange", "MA 50" = "purple"),
          name = "Indicator"
        )
    }

    if (isTRUE(input$show_signals)) {
      signal_points <- df %>% filter(Signal %in% c("Buy", "Sell"))
      if (nrow(signal_points) > 0) {
        p <- p +
          geom_point(
            data = signal_points,
            aes(y = Close, color = Signal, shape = Signal),
            size = 3,
            inherit.aes = TRUE
          ) +
          scale_shape_manual(values = c("Buy" = 24, "Sell" = 25)) +
          scale_color_manual(
            values = c("Buy" = "darkgreen", "Sell" = "red3",
                       "MA 20" = "darkorange", "MA 50" = "purple"),
            name = "Signal / Indicator"
          )
      }
    }

    p +
      labs(x = "Date", y = "Price", title = "Stock Price") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$technical_indicators)
    df <- analysis_data()
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(color = "darkorange", linewidth = 0.7, na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed",
                 color = "grey40") +
      scale_y_continuous(limits = c(0, 100)) +
      labs(x = "Date", y = "RSI", title = "RSI (14 periods)") +
      theme_minimal(base_size = 12)
  })

  output$macd_chart <- renderPlot({
    req("MACD" %in% input$technical_indicators)
    df <- analysis_data()
    ggplot(df, aes(x = Date)) +
      geom_col(aes(y = MACD_Histogram), fill = "grey65", na.rm = TRUE) +
      geom_line(aes(y = MACD, color = "MACD"), linewidth = 0.7, na.rm = TRUE) +
      geom_line(aes(y = MACD_Signal, color = "Signal"), linewidth = 0.7, na.rm = TRUE) +
      scale_color_manual(values = c("MACD" = "steelblue", "Signal" = "darkorange")) +
      labs(x = "Date", y = "Value", color = "Line", title = "MACD (12, 26, 9)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$latest_signal <- renderTable({
    df <- analysis_data()
    latest <- tail(df, 1)
    data.frame(
      Date = latest$Date,
      Ticker = toupper(input$symbol),
      Closing_Price = round(latest$Close, 2),
      MA20 = round(latest$MA20, 2),
      MA50 = round(latest$MA50, 2),
      Signal = latest$Signal
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

# ---- Launch the Shiny application ----
shinyApp(ui = ui, server = server)

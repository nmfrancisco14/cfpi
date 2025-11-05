# Load libraries
library(plotly)
library(dplyr)
library(tidyverse)

# Load data and compute CFPI
df <- readRDS("cfpi_computed.rds") %>%
  mutate(cfpi = 0.1 * (
    RetGap -
      FarmDev -
      ProfitDev -
      RetDev +
      Area +
      FertDev +
      FuelDev +
      RainDev +
      ImportDev -
      GlobalPriceDev
  ))

df$date <- as.Date(df$date)

# === Plot ===
p <- plot_ly()

# ---- y1: CFPI (filled area) ----
p <- p %>%
  add_lines(
    data = df,
    x = ~date, y = ~cfpi,
    name = "Composite Floor Price Index",
    fill = "tozeroy",
    fillcolor = "rgba(70, 130, 180, 0.3)",  # steelblue fill
    line = list(color = "steelblue", width = 2),
    hovertemplate = "CFPI<br>%{y:.2f}<extra></extra>",
    yaxis = "y1"
  )

# ---- y2: Prices (lines) ----
p <- p %>%
  add_lines(
    data = df,
    x = ~date, y = ~retail_real,
    name = "Retail Price",
    line = list(color = "darkorange", width = 2),
    hovertemplate = "Retail Price<br>₱ %{y:.2f}<extra></extra>",
    yaxis = "y2"
  ) %>%
  add_lines(
    data = df,
    x = ~date, y = ~fgate_real,
    name = "Farmgate Price",
    line = list(color = "black", width = 2),
    hovertemplate = "Farmgate Price<br>₱ %{y:.2f}<extra></extra>",
    yaxis = "y2"
  )

# === Layout ===
p <- p %>%
  layout(
    title = "Composite Floor Price Index and Actual Prices",
    hovermode = "x unified",
    legend = list(orientation = 'h', xanchor = "center", y = -0.3, x = 0.5),
    xaxis = list(
      title = "Date",
      rangeselector = list(
        buttons = list(
          list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
          list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
          list(count = 1, label = "1 yr", step = "year", stepmode = "backward"),
          list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
          list(step = "all")
        )
      ),
      rangeslider = list(type = "date")
    ),
    yaxis = list(
      title = "CFPI (Index Value)",
      side = "left",
      titlefont = list(size = 14),
      tickfont = list(size = 12)
    ),
    yaxis2 = list(
      title = "Prices (₱/kg)",
      overlaying = "y",
      side = "right",
      showgrid = FALSE,
      titlefont = list(size = 14),
      tickfont = list(size = 12)
    ),
    margin = list(l = 60, r = 60, t = 50, b = 50),
    plot_bgcolor = '#FFFFFF',
    paper_bgcolor = "#E3EADE"
  )

p
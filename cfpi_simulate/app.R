library(shiny)
library(bslib)
library(shinyWidgets)



# load data ---------------------------------------------------------------

cfpi_data <- readRDS("cfpidata.rds")

cfpi_com <-  readRDS("cfpi_computed.rds")

# ----- THEME -----
agri_theme <- bs_theme(
  bootswatch = "minty",
  primary = "#3b7a57",    # PhilRice green
  success = "#6dbb6d",
  base_font = font_google("Inter")
)

# ----- UI -----
ui <- page_sidebar(
  theme = agri_theme,
  title = div(icon("seedling", class = "me-2"), "Composite Floor Price Index (CFPI) Simulator"),

  tags$script(HTML("
  Shiny.addCustomMessageHandler('bindCompute', function(x) {
    Shiny.addCustomMessageHandler('triggerCompute', function(trigger) {
      if (trigger) {
        $('#compute').click();  // simulate compute button click
      }
    });
  });
")),

  # SIDEBAR
  sidebar = sidebar(
    open = TRUE,
    h4("⚖️ Weight Assignments"),
    helpText("Adjust weights for each indicator (default = 0.1)."),
    checkboxInput("normalize", "Auto-normalize weights (Σw = 1)", value = TRUE),
    br(),

    # Indicator groups
    h5("A. Farm-level"),
    sliderInput("w1", "w₁: Farmgate Price Deviation (↓)", 0, 1, 0.1, step = 0.01),
    sliderInput("w2", "w₂: Profit Deviation (↓)", 0, 1, 0.1, step = 0.01),

    h5("B. Retail-level"),
    sliderInput("w3", "w₃: Retail Price Deviation (↓)", 0, 1, 0.1, step = 0.01),
    sliderInput("w4", "w₄: Retail–Farmgate Gap (↑)", 0, 1, 0.1, step = 0.01),

    h5("C. Supply and Cost Factors"),
    sliderInput("w5", "w₅: Area Harvested Ratio (↑)", 0, 1, 0.1, step = 0.01),
    sliderInput("w6", "w₆: Fertilizer Cost (↑)", 0, 1, 0.1, step = 0.01),
    sliderInput("w7", "w₇: Fuel Price (↑)", 0, 1, 0.1, step = 0.01),

    h5("D. Climate and External Factors"),
    sliderInput("w8", "w₈: Rainfall Deviation (↑)", 0, 1, 0.1, step = 0.01),
    sliderInput("w9", "w₉: Import Arrivals (↑)", 0, 1, 0.1, step = 0.01),
    sliderInput("w10", "w₁₀: Global Rice Price (↓)", 0, 1, 0.1, step = 0.01),

    br(),
    actionButton("compute", "Compute CFPI", icon = icon("calculator"), class = "btn-success w-100")
  ),

  # MAIN CONTENT
  layout_column_wrap(
    width = 1,
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("📊 Simulation Result"),
        card_body(
          h4("Composite Floor Price Index (CFPI)"),
          uiOutput("cfpi_value"),
          br(),
          p("Interpretation: A higher CFPI indicates rising farmgate price risk —
          suggesting the need for earlier floor price intervention.")
        )
      ),

      card(
        card_header("🔢 Indicator Deviations"),
        card_body(
          helpText("Enter simulated deviations (Δ) for each indicator:"),
          fluidRow(
            column(6, numericInput("ind1", "FarmDev (Δ₁)", 0.02, step = 0.01)),
            column(6, numericInput("ind2", "ProfitDev (Δ₂)", -0.03, step = 0.01)),
            column(6, numericInput("ind3", "RetDev (Δ₃)", 0.01, step = 0.01)),
            column(6, numericInput("ind4", "RetGap (Δ₄)", 0.05, step = 0.01)),
            column(6, numericInput("ind5", "Area (Δ₅)", -0.02, step = 0.01)),
            column(6, numericInput("ind6", "FertCost (Δ₆)", 0.04, step = 0.01)),
            column(6, numericInput("ind7", "FuelPrice (Δ₇)", 0.03, step = 0.01)),
            column(6, numericInput("ind8", "Rain (Δ₈)", 0.01, step = 0.01)),
            column(6, numericInput("ind9", "Imports (Δ₉)", 0.06, step = 0.01)),
            column(6, numericInput("ind10", "GlobalPrice (Δ₁₀)", -0.02, step = 0.01))
          )
        )
      )

    ),

    card(
      full_screen = TRUE,
      card_header("🧾 Sample Data for Simulation"),
      card_body(
        DTOutput("data_table"),
        br(),
        p("Click a row to load indicator values into the simulation.", class = "text-muted small")
      )
    )


  )
)

# ----- SERVER -----
server <- function(input, output, session) {

  dtData <-
  cfpi_data|>
    filter(date>=ymd(paste0("2024","01","01"))) |>
    select(date,
           fgate,
           fgate_real,
           cost_per_kg,
           cost_per_kg_real,
           retail,
           retail_real,
           area_harv,
           damage_area,
           area_sem,
           monthly_ave_margin,
           urea,
           urea_real,
           fuel,
           fuel_real,
           imports,
           rain,
           fob) |>
    arrange(desc(date))




  output$data_table <- renderDT({
    num_cols <- names(dtData)[sapply(dtData, is.numeric)]

    datatable(dtData,
              selection = "single",
              colnames = c("Date",
                           "Farm gate price",
                           "Farm gate price (Real)",
                           "Cost per kg",
                           "Cost per kg (real)",
                           "Retail price",
                           "Retail price (real)",
                           "Expected area harvsted",
                           "Total area damaged",
                           "Total area for the whole sem",
                           "Average Margin (Ret - Farmgate)",
                           "Global urea price",
                           "Global urea price(real)",
                           "Fuel cost",
                           "Fuel cost (real)",
                           "Monthly Imports",
                           "Monthly average rainfall",
                           "Global FOB Rice Price"),
              options = list(pageLength = 10)) |>
      formatRound(columns = num_cols, digits = 1) %>%
      formatCurrency(columns = num_cols, currency = "", mark = ",", digits = 1)
  })
  #
  # observeEvent(input$data_table_rows_selected, {
  #   idx <- input$data_table_rows_selected
  #   if (is.null(idx)) return()
  #
  #   d <- df[idx, ]
  #
  #   # Historical or moving averages
  #   FP_hist <- mean(df$fgate_real, na.rm = TRUE)
  #   RP_hist <- mean(df$retail_real, na.rm = TRUE)
  #   Profit_hist <- mean(df$net_returns, na.rm = TRUE)
  #   FC_hist <- mean(df$urea_real, na.rm = TRUE)
  #   Rain_hist <- mean(df$rain, na.rm = TRUE)
  #   Margin_hist <- mean(df$retail_real - df$fgate_real, na.rm = TRUE)
  #
  #   Fuel_MA3 <- zoo::rollmean(df$fuel_real, 3, fill = NA, align = "right")[idx]
  #   Import_MA3 <- zoo::rollmean(df$imports, 3, fill = NA, align = "right")[idx]
  #   FOB_MA3 <- zoo::rollmean(df$fob, 3, fill = NA, align = "right")[idx]
  #
  #   # Compute indicators
  #   ind <- list(
  #     (d$fgate_real - FP_hist) / FP_hist,
  #     (d$net_returns - Profit_hist) / Profit_hist,
  #     (d$retail_real - RP_hist) / RP_hist,
  #     ((d$retail_real - d$fgate_real) - Margin_hist) / Margin_hist,
  #     (d$area_harv - d$damage_area) / d$area_sem,
  #     (d$urea_real - FC_hist) / FC_hist,
  #     (d$fuel_real - Fuel_MA3) / Fuel_MA3,
  #     (d$rain - Rain_hist) / Rain_hist,
  #     (d$imports - Import_MA3) / Import_MA3,
  #     (d$fob - FOB_MA3) / FOB_MA3
  #   )
  #
  #   # Update numeric inputs automatically
  #   lapply(1:10, function(i) {
  #     updateNumericInput(session, paste0("ind", i), value = round(ind[[i]], 4))
  #   })
  # })

  # When a row is clicked
  observe({
    req(input$data_table_rows_selected)
    selected_row <- dtData[input$data_table_rows_selected, ]
    sel_date <- selected_row$date

    # Match date in cfpi_data
    dev_row <- cfpi_com |> dplyr::filter(date == sel_date)

    if (nrow(dev_row) == 1) {
      updateNumericInput(session, "ind1", value = dev_row$FarmDev)
      updateNumericInput(session, "ind2", value = dev_row$ProfitDev)
      updateNumericInput(session, "ind3", value = dev_row$RetDev)
      updateNumericInput(session, "ind4", value = dev_row$RetGap)
      updateNumericInput(session, "ind5", value = dev_row$Area)
      updateNumericInput(session, "ind6", value = dev_row$FertDev)
      updateNumericInput(session, "ind7", value = dev_row$FuelDev)
      updateNumericInput(session, "ind8", value = dev_row$RainDev)
      updateNumericInput(session, "ind9", value = dev_row$ImportDev)
      updateNumericInput(session, "ind10", value = dev_row$GlobalPriceDev)


      session$sendCustomMessage("triggerCompute", TRUE)
    }
  }) |>
    bindEvent(input$data_table_rows_selected)






  cfpi_calc <- eventReactive(input$compute, {
    w <- sapply(1:10, function(i) input[[paste0("w", i)]])
    ind <- sapply(1:10, function(i) input[[paste0("ind", i)]])

    if (input$normalize) w <- w / sum(w)

    pos_idx <- c(4, 5, 6, 7, 8, 9)
    neg_idx <- c(1, 2, 3, 10)

    cfpi <- sum(w[pos_idx] * ind[pos_idx]) - sum(w[neg_idx] * ind[neg_idx])
    list(cfpi = cfpi)
  })

  output$cfpi_value <- renderUI({
    req(cfpi_calc())
    val <- cfpi_calc()$cfpi
    color <- ifelse(val > 0.05, "danger",
                    ifelse(val < -0.05, "success", "warning"))

    div(
      style = paste("font-size:1.6rem; font-weight:bold; color:",
                    ifelse(color == "danger", "#b30000",
                           ifelse(color == "success", "#2e7d32", "#ff8c00"))),
      sprintf("CFPIₜ = %.3f", val)
    )
  })




  # ---- JS listener to call the compute button ----
  observe({
    session$sendCustomMessage("bindCompute", TRUE)
  })

  # ---- Add this line at the end of server ----
  session$onFlushed(function() {
    session$sendCustomMessage("bindCompute", TRUE)
  }, once = TRUE)

}


shinyApp(ui, server)

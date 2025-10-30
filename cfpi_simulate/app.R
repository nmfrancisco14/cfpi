library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(tidyverse)

# Load data ---------------------------------------------------------------
cfpi_data <- readRDS("cfpidata.rds")
cfpi_com <- readRDS("cfpi_computed.rds")

# Indicator definitions ---------------------------------------------------
indicators <- c(
  "Farmgate Price Deviation",
  "Farmer Profit Deviation",
  "Retail Price Deviation",
  "Retail–Farmgate Price Gap",
  "Area Harvested Ratio",
  "Fertilizer Cost Deviation",
  "Fuel Price Deviation",
  "Monthly Rainfall Deviation",
  "Import Arrival Deviation",
  "Global Rice Price Deviation"
)

indicator_icons <- c("dollar-sign", "chart-line", "shopping-cart", "arrows-alt-h",
                     "layer-group", "flask", "gas-pump", "cloud-rain",
                     "ship", "globe")

# Create indicator cards --------------------------------------------------
cards <- lapply(1:10, function(i) {
  card(
    class = "indicator-card",
    card_header(
      class = "indicator-header",
      div(
        class = "d-flex align-items-center gap-2",
        div(class = "indicator-icon", icon(indicator_icons[[i]])),
        div(class = "indicator-title", indicators[[i]])
      )
    ),
    card_body(
      class = "p-3",
      numericInput(
        inputId = paste0("ind", i),
        label = NULL,
        value = 0,
        step = 0.01,
        width = "100%"
      )
    )
  )
})

# Modern theme ------------------------------------------------------------
modern_theme <- bs_theme(
  version = 5,
  preset = "shiny",
  primary = "#2e7d32",
  secondary = "#558b2f",
  success = "#66bb6a",
  info = "#29b6f6",
  warning = "#ffa726",
  danger = "#ef5350",
  base_font = font_google("Inter"),
  heading_font = font_google("Plus Jakarta Sans"),
  code_font = font_google("JetBrains Mono"),
  bg = "#fafafa",
  fg = "#212121",
  "card-border-radius" = "16px",
  "btn-border-radius" = "12px"
)

# UI ----------------------------------------------------------------------
ui <- page_fillable(
  theme = modern_theme,
  title = "CFPI Simulator",

  tags$head(
    tags$style(HTML("
      /* Global styles */
      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #e8f5e9 100%);
        font-size: 13px;
      }

      /* Header styling */
      .app-header {
        background: linear-gradient(135deg, #2e7d32 0%, #558b2f 100%);
        color: white;
        padding: 1.25rem 1.5rem 1rem;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 20px rgba(46, 125, 50, 0.2);
        margin-bottom: 1.25rem;
      }

      .app-header h1 {
        font-size: 1.5rem;
        font-weight: 700;
        margin: 0;
        display: flex;
        align-items: center;
        gap: 0.75rem;
      }

      .app-header p {
        margin: 0.35rem 0 0;
        opacity: 0.95;
        font-size: 0.85rem;
      }

      /* Sidebar styling */
      .bslib-sidebar-layout > .sidebar {
        background: white;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        padding: 1rem;
        max-height: calc(100vh - 180px);
        overflow-y: auto;
      }

      .sidebar h4, .sidebar h5 {
        color: #2e7d32;
        font-weight: 600;
        margin-top: 0.75rem;
        margin-bottom: 0.5rem;
      }

      .sidebar h4 {
        font-size: 1rem;
        border-bottom: 2px solid #e8f5e9;
        padding-bottom: 0.35rem;
      }

      .sidebar h5 {
        font-size: 0.85rem;
        margin-top: 0.75rem;
      }

      .sidebar .help-text {
        font-size: 0.75rem;
        margin-bottom: 0.5rem;
      }

      .sidebar .form-group {
        margin-bottom: 0.65rem;
      }

      .sidebar label {
        font-size: 0.8rem;
        margin-bottom: 0.25rem;
      }

      .sidebar .irs {
        height: 40px;
      }

      /* Slider styling */
      .irs--shiny .irs-bar {
        background: linear-gradient(90deg, #66bb6a, #2e7d32);
      }

      .irs--shiny .irs-handle {
        background: white;
        border: 3px solid #2e7d32;
        box-shadow: 0 2px 8px rgba(46, 125, 50, 0.3);
      }

      /* Card styling */
      .card {
        border: none;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        transition: all 0.3s ease;
        background: white;
      }

      .card:hover {
        box-shadow: 0 4px 16px rgba(0,0,0,0.1);
        transform: translateY(-1px);
      }

      .card-header {
        background: linear-gradient(135deg, #f5f5f5 0%, #fafafa 100%);
        border-bottom: 2px solid #e8f5e9;
        font-weight: 600;
        padding: 0.75rem 1rem;
        font-size: 0.9rem;
        color: #2e7d32;
      }

      /* Indicator cards */
      .indicator-card {
        border: 1px solid #e0e0e0;
        background: white;
        overflow: hidden;
      }

      .indicator-card .indicator-header {
        background: linear-gradient(135deg, #2e7d32 0%, #558b2f 100%);
        color: white;
        padding: 0.6rem 0.75rem;
        border: none;
        font-size: 0.75rem;
        font-weight: 600;
        line-height: 1.2;
      }

      .indicator-icon {
        width: 24px;
        height: 24px;
        background: rgba(255, 255, 255, 0.2);
        border-radius: 6px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 0.8rem;
        flex-shrink: 0;
      }

      .indicator-title {
        line-height: 1.2;
        flex: 1;
      }

      .indicator-card .card-body {
        padding: 0.6rem;
        background: #fafafa;
      }

      .indicator-card input[type='number'] {
        font-size: 1rem;
        font-weight: 700;
        border: 2px solid #e0e0e0;
        border-radius: 6px;
        padding: 0.5rem;
        transition: all 0.2s ease;
        background: white;
        text-align: center;
        color: #2e7d32;
      }

      .indicator-card input[type='number']:focus {
        border-color: #2e7d32;
        box-shadow: 0 0 0 3px rgba(46, 125, 50, 0.1);
        outline: none;
      }

      /* CFPI result card */
      .cfpi-result-card {
        background: linear-gradient(135deg, #ffffff 0%, #f5f5f5 100%);
        border: 2px solid #e8f5e9;
      }

      .cfpi-result-card .card-body {
        padding: 1rem;
      }

      .cfpi-result-card h4 {
        font-size: 1rem;
        margin-bottom: 0.5rem;
      }

      .cfpi-value {
        font-size: 2rem;
        font-weight: 800;
        padding: 1rem;
        text-align: center;
        border-radius: 10px;
        margin: 0.75rem 0;
        animation: fadeIn 0.5s ease;
      }

      @keyframes fadeIn {
        from { opacity: 0; transform: scale(0.95); }
        to { opacity: 1; transform: scale(1); }
      }

      .cfpi-interpretation {
        background: #f5f5f5;
        padding: 0.75rem;
        border-radius: 6px;
        border-left: 3px solid #2e7d32;
        margin-top: 0.75rem;
        font-size: 0.8rem;
        line-height: 1.4;
      }

      /* Button styling */
      .btn-success {
        background: linear-gradient(135deg, #66bb6a 0%, #2e7d32 100%);
        border: none;
        padding: 0.65rem 1.25rem;
        font-weight: 600;
        font-size: 0.9rem;
        box-shadow: 0 3px 10px rgba(46, 125, 50, 0.3);
        transition: all 0.3s ease;
      }

      .btn-success:hover {
        transform: translateY(-2px);
        box-shadow: 0 5px 16px rgba(46, 125, 50, 0.4);
      }

      /* Data table styling */
      .dataTables_wrapper {
        padding: 1rem 0;
      }

      table.dataTable tbody tr {
        cursor: pointer;
        transition: all 0.2s ease;
      }

      table.dataTable tbody tr:hover {
        background-color: #e8f5e9 !important;
      }

      table.dataTable tbody tr.selected {
        background-color: #c8e6c9 !important;
      }

      /* Status badges */
      .status-badge {
        display: inline-block;
        padding: 0.3rem 0.65rem;
        border-radius: 16px;
        font-weight: 600;
        font-size: 0.8rem;
      }

      .status-high {
        background: #ffebee;
        color: #c62828;
      }

      .status-normal {
        background: #fff3e0;
        color: #ef6c00;
      }

      .status-low {
        background: #e8f5e9;
        color: #2e7d32;
      }

      /* Tooltip styling */
      .help-text {
        color: #757575;
        font-size: 0.75rem;
        font-style: italic;
      }

      /* Modal styling */
      .modal-content {
        border: none;
        border-radius: 16px;
        box-shadow: 0 10px 40px rgba(0,0,0,0.2);
      }

      .modal-body h4 {
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }

      .modal-body ul, .modal-body ol {
        margin-bottom: 0;
      }

      .modal-body li {
        margin-bottom: 0.5rem;
      }

      /* Loading state */
      .shiny-output-error {
        display: none;
      }
    "))
  ),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('bindCompute', function(x) {
      Shiny.addCustomMessageHandler('triggerCompute', function(trigger) {
        if (trigger) {
          $('#compute').click();
        }
      });
    });

    // Show welcome modal on page load
    $(document).ready(function() {
      $('#welcomeModal').modal('show');
    });
  ")),

  # Welcome Modal
  tags$div(
    class = "modal fade",
    id = "welcomeModal",
    tabindex = "-1",
    role = "dialog",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      style = "max-width: 900px; max-height: 90vh;",
      role = "document",
      tags$div(
        class = "modal-content",
        style = "max-height: 90vh;",
        tags$div(
          class = "modal-header",
          style = "background: linear-gradient(135deg, #2e7d32 0%, #558b2f 100%); color: white; border: none; padding: 1rem 1.5rem;",
          tags$h4(
            class = "modal-title",
            style = "margin: 0; font-size: 1.4rem;",
            icon("seedling"), " CFPI Simulator - PhilRice Data Analytics Center"
          ),
          tags$button(
            type = "button",
            class = "btn-close btn-close-white",
            `data-bs-dismiss` = "modal",
            `aria-label` = "Close"
          )
        ),
        tags$div(
          class = "modal-body compact-modal",
          style = "padding: 1.25rem 1.5rem; overflow-y: auto; max-height: calc(90vh - 140px);",

          tags$p(
            style = "font-size: 0.85rem; line-height: 1.4; margin-bottom: 0.75rem;",
            "The ", tags$strong("Composite Floor Price Index (CFPI)"), " provides early warning signals for farmgate price declines, supporting ", tags$strong("EO 100 (2025)"), " for timely interventions."
          ),

          tags$div(
            style = "background: #e8f5e9; padding: 0.75rem; border-radius: 6px; border-left: 3px solid #2e7d32; margin-bottom: 0.75rem;",
            tags$p(
              style = "margin: 0; font-size: 0.75rem; line-height: 1.3;",
              icon("quote-left", style = "font-size: 0.7rem;"), " ",
              tags$em("A floor price shall guarantee fair returns to farmers while ensuring rice supply stability."),
              " ", tags$small(style = "color: #555;", "— EO 100, 2025")
            )
          ),

          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; margin-bottom: 0.75rem;",

            # Left column
            tags$div(
              tags$h5(
                style = "color: #2e7d32; margin-bottom: 0.5rem; font-size: 0.95rem;",
                icon("calculator"), " 10 Key Indicators"
              ),
              tags$ul(
                style = "font-size: 0.75rem; line-height: 1.4; margin: 0; padding-left: 1.2rem;",
                tags$li(tags$strong("Farm:"), " Prices & profits"),
                tags$li(tags$strong("Retail:"), " Prices & gaps"),
                tags$li(tags$strong("Supply:"), " Area, fertilizer, fuel"),
                tags$li(tags$strong("External:"), " Rain, imports, global prices")
              ),

              tags$h5(
                style = "color: #2e7d32; margin-top: 0.75rem; margin-bottom: 0.5rem; font-size: 0.95rem;",
                icon("chart-line"), " Result Interpretation"
              ),
              tags$div(
                style = "display: flex; flex-direction: column; gap: 0.4rem;",
                tags$div(
                  style = "background: #e8f5e9; padding: 0.4rem 0.6rem; border-radius: 4px; font-size: 0.72rem;",
                  tags$strong("< -0.05:"), " Stable (no action)"
                ),
                tags$div(
                  style = "background: #fff3e0; padding: 0.4rem 0.6rem; border-radius: 4px; font-size: 0.72rem;",
                  tags$strong("-0.05 to 0.05:"), " Watch closely"
                ),
                tags$div(
                  style = "background: #ffebee; padding: 0.4rem 0.6rem; border-radius: 4px; font-size: 0.72rem;",
                  tags$strong("> 0.05:"), " Trigger intervention"
                )
              )
            ),

            # Right column
            tags$div(
              tags$h5(
                style = "color: #2e7d32; margin-bottom: 0.5rem; font-size: 0.95rem;",
                icon("users"), " How to Use"
              ),
              tags$ol(
                style = "font-size: 0.75rem; line-height: 1.4; margin: 0; padding-left: 1.2rem;",
                tags$li(tags$strong("Select:"), " Click a row in Historical Data table"),
                tags$li(tags$strong("Adjust:"), " Set weights in sidebar (auto-normalized)"),
                tags$li(tags$strong("Modify:"), " Fine-tune indicator values"),
                tags$li(tags$strong("Calculate:"), " Click ", tags$span(style = "color: #2e7d32;", icon("calculator")), " button")
              ),

              tags$div(
                style = "background: #fff3e0; padding: 0.75rem; border-radius: 6px; margin-top: 0.75rem; border-left: 3px solid #ef6c00;",
                tags$p(
                  style = "margin: 0; font-size: 0.75rem; line-height: 1.3;",
                  icon("exclamation-circle"), " ",
                  tags$strong("Higher CFPI"), " = Rising risk → Earlier intervention needed"
                )
              )
            )
          )
        ),
        tags$div(
          class = "modal-footer",
          style = "border-top: 1px solid #e0e0e0; padding: 0.75rem 1.5rem;",
          tags$button(
            type = "button",
            class = "btn btn-success",
            style = "padding: 0.4rem 1.5rem; font-size: 0.9rem;",
            `data-bs-dismiss` = "modal",
            icon("check"), " Got it!"
          )
        )
      )
    )
  ),

  # Header
  div(
    class = "app-header",
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      div(
        h1(icon("seedling"), "Composite Floor Price Index (CFPI) Simulator"),
        p("Advanced analytics for rice price intervention forecasting")
      ),
      actionButton(
        "showWelcome",
        label = NULL,
        icon = icon("info-circle"),
        class = "btn-sm",
        style = "background: rgba(255,255,255,0.2); border: 1px solid rgba(255,255,255,0.4); color: white; padding: 0.5rem 0.75rem; border-radius: 8px; font-size: 1.1rem;",
        title = "Show Welcome Guide",
        onclick = "$('#welcomeModal').modal('show');"
      )
    )
  ),

  # Main layout
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      open = TRUE,

      h4(icon("sliders-h"), " Weight Assignments"),
      p("Adjust weights for each indicator (default = 0.1)", class = "help-text"),

      checkboxInput("normalize", "Auto-normalize weights (Σw = 1)", value = TRUE),

      hr(),

      h5(icon("tractor"), " A. Farm-level Indicators"),
      sliderInput("w1", "w₁: Farmgate Price Dev. (↓)", 0, 1, 0.1, 0.01),
      sliderInput("w2", "w₂: Profit Dev. (↓)", 0, 1, 0.1, 0.01),

      h5(icon("store"), " B. Retail-level Indicators"),
      sliderInput("w3", "w₃: Retail Price Dev. (↓)", 0, 1, 0.1, 0.01),
      sliderInput("w4", "w₄: Retail–Farmgate Gap (↑)", 0, 1, 0.1, 0.01),

      h5(icon("industry"), " C. Supply & Cost Factors"),
      sliderInput("w5", "w₅: Area Harvested Ratio (↑)", 0, 1, 0.1, 0.01),
      sliderInput("w6", "w₆: Fertilizer Cost (↑)", 0, 1, 0.1, 0.01),
      sliderInput("w7", "w₇: Fuel Price (↑)", 0, 1, 0.1, 0.01),

      h5(icon("cloud-sun"), " D. Climate & External"),
      sliderInput("w8", "w₈: Rainfall Dev. (↑)", 0, 1, 0.1, 0.01),
      sliderInput("w9", "w₉: Import Arrivals (↑)", 0, 1, 0.1, 0.01),
      sliderInput("w10", "w₁₀: Global Rice Price (↓)", 0, 1, 0.1, 0.01),

      br(),
      actionButton("compute", "Calculate CFPI",
                   icon = icon("calculator"),
                   class = "btn-success w-100")
    ),

    # Main content
    layout_column_wrap(
      width = 1,

      # Indicators and CFPI result
      layout_columns(
        col_widths = c(8, 4),

        # Indicator grid
        card(
          card_header(icon("chart-bar"), " Indicator Values"),
          card_body(
            layout_column_wrap(
              width = 1/5,
              cards[[1]], cards[[2]], cards[[3]], cards[[4]], cards[[5]],
              cards[[6]], cards[[7]], cards[[8]], cards[[9]], cards[[10]]
            )
          )
        ),

        # CFPI result card
        card(
          class = "cfpi-result-card",
          card_header(icon("chart-line"), " CFPI Result"),
          card_body(
            h4("Composite Floor Price Index"),
            uiOutput("cfpi_value"),
            div(
              class = "cfpi-interpretation",
              icon("info-circle"),
              strong(" Interpretation: "),
              "A higher CFPI indicates rising farmgate price risk — suggesting earlier floor price intervention may be needed."
            )
          )
        )
      ),

      # Data table
      card(
        full_screen = TRUE,
        card_header(icon("table"), " Historical Data for Simulation"),
        card_body(
          DTOutput("data_table"),
          br(),
          p(icon("hand-pointer"), " Click any row to load its values into the simulator",
            class = "help-text")
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {

  # Prepare data table
  dtData <- cfpi_data |>
    filter(date >= ymd(paste0("2024", "01", "01"))) |>
    select(date, fgate, fgate_real, cost_per_kg, cost_per_kg_real,
           retail, retail_real, area_harv, damage_area, area_sem,
           monthly_ave_margin, urea, urea_real, fuel, fuel_real,
           imports, rain, fob) |>
    arrange(desc(date))

  # Render data table
  output$data_table <- renderDT({
    num_cols <- names(dtData)[sapply(dtData, is.numeric)]

    datatable(
      dtData,
      selection = "single",
      colnames = c("Date", "Farm gate price", "Farm gate price (Real)",
                   "Cost per kg", "Cost per kg (real)", "Retail price",
                   "Retail price (real)", "Expected area harvested",
                   "Total area damaged", "Total area for the whole sem",
                   "Average Margin (Ret - Farmgate)", "Global urea price",
                   "Global urea price(real)", "Fuel cost", "Fuel cost (real)",
                   "Monthly Imports", "Monthly average rainfall",
                   "Global FOB Rice Price"),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      )
    ) |>
      formatRound(columns = num_cols, digits = 1) %>%
      formatCurrency(columns = num_cols, currency = "", mark = ",", digits = 1)
  })

  # Handle row selection
  observe({
    req(input$data_table_rows_selected)
    selected_row <- dtData[input$data_table_rows_selected, ]
    sel_date <- selected_row$date

    dev_row <- cfpi_com |> dplyr::filter(date == sel_date)


    dev_row2 <-  dev_row |> mutate_if(is.numeric,round,digits=3)

    if (nrow(dev_row) == 1) {
      updateNumericInput(session, "ind1", value = dev_row2$FarmDev)
      updateNumericInput(session, "ind2", value = dev_row2$ProfitDev)
      updateNumericInput(session, "ind3", value = dev_row2$RetDev)
      updateNumericInput(session, "ind4", value = dev_row2$RetGap)
      updateNumericInput(session, "ind5", value = dev_row2$Area)
      updateNumericInput(session, "ind6", value = dev_row2$FertDev)
      updateNumericInput(session, "ind7", value = dev_row2$FuelDev)
      updateNumericInput(session, "ind8", value = dev_row2$RainDev)
      updateNumericInput(session, "ind9", value = dev_row2$ImportDev)
      updateNumericInput(session, "ind10", value = dev_row2$GlobalPriceDev)

      session$sendCustomMessage("triggerCompute", TRUE)
    }
  }) |>
    bindEvent(input$data_table_rows_selected)

  # Calculate CFPI
  cfpi_calc <- eventReactive(input$compute, {
    w <- sapply(1:10, function(i) input[[paste0("w", i)]])
    ind <- sapply(1:10, function(i) input[[paste0("ind", i)]])

    if (input$normalize) w <- w / sum(w)

    pos_idx <- c(4, 5, 6, 7, 8, 9)
    neg_idx <- c(1, 2, 3, 10)

    cfpi <- sum(w[pos_idx] * ind[pos_idx]) - sum(w[neg_idx] * ind[neg_idx])
    list(cfpi = cfpi)
  })

  # Display CFPI value
  output$cfpi_value <- renderUI({
    req(cfpi_calc())
    val <- cfpi_calc()$cfpi

    status <- if (val > 0.05) {
      list(class = "status-high", bg = "#ffebee", color = "#c62828",
           label = "High Risk", icon = "exclamation-triangle")
    } else if (val < -0.05) {
      list(class = "status-low", bg = "#e8f5e9", color = "#2e7d32",
           label = "Low Risk", icon = "check-circle")
    } else {
      list(class = "status-normal", bg = "#fff3e0", color = "#ef6c00",
           label = "Moderate", icon = "info-circle")
    }

    div(
      div(
        class = "cfpi-value",
        style = sprintf("background: %s; color: %s;", status$bg, status$color),
        sprintf("%.4f", val)
      ),
      div(
        class = paste("status-badge", status$class),
        icon(status$icon), " ", status$label
      )
    )
  })

  # Display indicator values
  # (Removed - no longer needed with new card design)

  # JS message handlers
  observe({
    session$sendCustomMessage("bindCompute", TRUE)
  })

  session$onFlushed(function() {
    session$sendCustomMessage("bindCompute", TRUE)
  }, once = TRUE)
}

shinyApp(ui, server)

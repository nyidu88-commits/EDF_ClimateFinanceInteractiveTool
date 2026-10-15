# ==============================================================================
# CLIMATE FINANCE GAP EXPLORER
# Based on: Kerr & Hu (2025) - Filling the Climate Finance Gap
# ==============================================================================

library(shiny)
library(shinyjs)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

# ==============================================================================
# DEFAULT PARAMETERS (matching the Python Streamlit version)
# ==============================================================================

# BAU (Business as Usual) parameters - starting point with remaining gap
BAU_PARAMS <- list(
  # Public + Private Finance
  CF = 8400.0,           # Total Climate Finance Need (B USD/year)
  PUB_2022 = 660.0,      # Public Finance in 2022 (B USD)
  r = 0.1437,            # Public Finance Growth Rate
  PRIV_2022 = 438.0,     # Private Finance in 2022 (B USD)
  g = 0.0954,            # Private Finance Growth Rate
  Lhighpub = 0.30,       # Public Leverage (BAU = 0.30)
  
  # Domestic Carbon Market (BAU - 2026 World Bank report levels)
  E_2030 = 36.5,         # Global CO2 Emissions (GtCO2)
  epsilon = 0.0028,      # Carbon Price Elasticity
  P_Adv = 10.0,          # Advanced Economies Carbon Price ($/tCO2) - World Bank 2026
  C_Adv = 0.33,          # Advanced Economies Coverage Rate - World Bank 2026
  P_EMDEs = 0.0,         # EMDEs Carbon Price ($/tCO2) - World Bank 2026
  C_EMDEs = 0.0,         # EMDEs Coverage Rate (no EMDEs pricing in BAU)
  Lcp_dom = 5.0,         # Domestic Carbon Market Leverage Ratio (BAU; doubles to 10 under HLR)
  
  # International Carbon Market (BAU = 0, not developed yet)
  ICMS_2030 = 0.0,       # International Credits Market Size (B USD)
  Lcp_int = 5.0,         # International Carbon Market Leverage Ratio (BAU; doubles to 10 under HLR)
  
  # Voluntary Carbon Market (BAU = low estimate of USD 10-40B range, Kerr et al. 2025)
  VMS_2030 = 10.0,       # Voluntary Market Size (B USD)
  Lcp_vol = 5.0          # Voluntary Carbon Market Leverage Ratio (BAU; doubles to 10 under HLR)
)

# Default params for sliders (same as BAU initially)
DEFAULT_PARAMS <- BAU_PARAMS

# ICM+HLR Solution (Paper's optimal scenario - fills the gap)
ICM_HLR_PARAMS <- list(
  CF = 8400.0,
  PUB_2022 = 660.0,
  r = 0.1437,            # Public growth rate
  PRIV_2022 = 438.0,
  g = 0.0954,            # Private growth rate
  Lhighpub = 0.60,       # High leverage ratio (HLR)
  E_2030 = 36.5,
  epsilon = 0.0028,
  P_Adv = 80.0,          # capped at the slider max of 80 (was 80.9)
  C_Adv = 0.30,
  P_EMDEs = 35.0,
  C_EMDEs = 0.30,
  Lcp_dom = 10.0,
  ICMS_2030 = 210.0,     # International Carbon Market (ICM)
  Lcp_int = 10.0,
  VMS_2030 = 10.0,
  Lcp_vol = 10.0
)


# ==============================================================================
# CALCULATION FUNCTIONS (matching Python version)
# ==============================================================================

#' Calculate finance values for a given year with parameters
calculate_finance <- function(params, year = 2030) {
  years_from_2022 <- year - 2022
  
  # Project public and private finance
  PUB <- params$PUB_2022 * (1 + params$r)^years_from_2022
  PRIV <- params$PRIV_2022 * (1 + params$g)^years_from_2022
  
  # Calculate leverage components
  PRIVpub <- PUB * params$Lhighpub
  
  # Calculate domestic carbon pricing mobilization (with domestic leverage)
  PRIVdom <- params$E_2030 * params$epsilon * (
    params$P_Adv * params$C_Adv * params$P_Adv +
      params$P_EMDEs * params$C_EMDEs * params$P_EMDEs
  ) * params$Lcp_dom
  
  # Voluntary markets (with voluntary leverage)
  PRIVvol <- params$VMS_2030 * params$Lcp_vol
  
  # International markets (with international leverage)
  PRIVint <- params$ICMS_2030 * params$Lcp_int
  
  # Calculate remaining gap
  total_available <- PUB + PRIV + PRIVpub + PRIVdom + PRIVvol + PRIVint
  CFG <- params$CF - total_available
  
  list(
    CF = params$CF,
    PUB = PUB,
    PRIV = PRIV,
    PRIVpub = PRIVpub,
    PRIVdom = PRIVdom,
    PRIVvol = PRIVvol,
    PRIVint = PRIVint,
    CFG = CFG
  )
}

#' Calculate 2022 baseline values
calculate_2022_baseline <- function(params) {
  # 2022 domestic carbon pricing (World Bank, State and Trends of Carbon Pricing 2023:
  # 73 carbon taxes/ETSs covering ~23% of global GHG emissions).
  # Advanced economies: coverage 0.23, price $18.5/tCO2 (price not stated in the
  # World Bank report itself - source to be confirmed). EMDEs: 0 coverage, 0 price.
  # Leverage ratio 5 (Kerr et al. 2025).
  P_Adv_2022   <- 18.5
  C_Adv_2022   <- 0.23
  P_EMDEs_2022 <- 0
  C_EMDEs_2022 <- 0
  PRIVdom_2022 <- params$E_2030 * params$epsilon * (
    P_Adv_2022 * C_Adv_2022 * P_Adv_2022 +
      P_EMDEs_2022 * C_EMDEs_2022 * P_EMDEs_2022
  ) * 5
  
  list(
    CF = params$CF,
    PUB = params$PUB_2022,
    PRIV = params$PRIV_2022,
    PRIVpub = params$PUB_2022 * params$Lhighpub,
    PRIVdom = PRIVdom_2022,
    
    PRIVvol = 0,
    PRIVint = 0,
    CFG = params$CF - (params$PUB_2022 + params$PRIV_2022 +
                         params$PUB_2022 * params$Lhighpub + PRIVdom_2022)
  )
}

# ==============================================================================
# TIME SERIES CALCULATION (En-ROADS style)
# ==============================================================================

#' Calculate finance time series from 2022 to 2100
calculate_timeseries <- function(params, years = 2022:2100) {
  results <- lapply(years, function(year) {
    res <- calculate_finance(params, year)
    res$year <- year
    res$total_supply <- res$PUB + res$PRIV + res$PRIVpub +
      res$PRIVdom + res$PRIVvol + res$PRIVint
    res
  })
  
  data.frame(
    year = sapply(results, function(x) x$year),
    CF = sapply(results, function(x) x$CF),
    PUB = sapply(results, function(x) x$PUB),
    PRIV = sapply(results, function(x) x$PRIV),
    PRIVpub = sapply(results, function(x) x$PRIVpub),
    PRIVdom = sapply(results, function(x) x$PRIVdom),
    PRIVvol = sapply(results, function(x) x$PRIVvol),
    PRIVint = sapply(results, function(x) x$PRIVint),
    total_supply = sapply(results, function(x) x$total_supply),
    gap = sapply(results, function(x) x$CFG)
  )
}

# ==============================================================================
# VISUALIZATION: Waterfall chart (matching Python/Matplotlib version)
# ==============================================================================

# Labels and colors matching the Python version
# Order: Total -> Public -> Private -> Public Leverage ->
#        Domestic -> International -> Voluntary -> Remaining Gap
WATERFALL_LABELS <- c(
  "Total Finance\nNeeded",
  "Public Finance",
  "Private Finance",
  "Public Leverage",
  "Domestic Carbon Price",
  "International\nCarbon Price",
  "Voluntary Carbon Price",
  "<b>Remaining Gap</b>"
)

WATERFALL_COLORS <- c(
  "#1E90FF",   # dodgerblue - CF
  "#FFA07A",   # lightsalmon - PUB
  "#F08080",   # lightcoral - PRIV
  "#FFD700",   # gold - PRIVpub
  "#32CD32",   # limegreen - PRIVdom
  "#00BFFF",   # deepskyblue - PRIVint
  "#DA70D6",   # orchid - PRIVvol
  "#FF6347"    # tomato - CFG
)

#' Compute de-collided y positions for the value labels above each waterfall
#' bar. Two-pass algorithm:
#'  1) push: walk left to right and nudge a label up, but only when its own
#'     NATURAL height sits close to the previous bar's natural height. Using
#'     the natural (unmodified) heights to detect a collision, rather than
#'     the already-pushed position, keeps each push local instead of
#'     cascading down the rest of a descending staircase whenever two
#'     adjacent bars happen to collide early on.
#'  2) pull back: clamp anything pushed above the visible chart area, or a
#'     bar tall enough to leave its own label below 0 (an extreme slider
#'     combination can do this), back inside the plot, nudging the
#'     neighbour on the side it was pulled toward so the pull-back doesn't
#'     reintroduce an overlap.
compute_label_ypos <- function(top_y, yaxis_max) {
  n <- length(top_y)
  if (n == 0) return(numeric(0))
  min_gap <- yaxis_max * 0.045
  top_margin <- yaxis_max * 0.06
  visible_top <- yaxis_max - top_margin
  visible_bottom <- yaxis_max * 0.03
  
  pos <- top_y
  if (n > 1) {
    for (i in 2:n) {
      if (abs(top_y[i] - top_y[i - 1]) < min_gap && pos[i] < pos[i - 1] + min_gap) {
        pos[i] <- pos[i - 1] + min_gap
      }
    }
  }
  
  for (i in seq_len(n)) {
    if (pos[i] > visible_top) {
      pos[i] <- visible_top
      if (i > 1 && pos[i - 1] > pos[i] - min_gap) {
        pos[i - 1] <- pos[i] - min_gap
      }
    } else if (pos[i] < visible_bottom) {
      pos[i] <- visible_bottom
      if (i < n && pos[i + 1] < pos[i] + min_gap) {
        pos[i + 1] <- pos[i] + min_gap
      }
    }
  }
  pos
}

#' Build the value-label annotations (skips zero-value bars entirely) for a
#' waterfall data frame produced below, with overlap avoided.
waterfall_label_annotations <- function(df, yaxis_max = 9500) {
  keep <- df$abs_value > 0.5
  if (!any(keep)) return(list())
  sub <- df[keep, , drop = FALSE]
  top_y <- pmax(sub$start, sub$end)
  label_y <- compute_label_ypos(top_y, yaxis_max)
  
  # x must be a plain character string, not a factor: the initial static
  # render tolerates a factor here because plotly's own R->JSON conversion
  # maps it to its label, but the live plotlyProxy relayout path serializes
  # through Shiny's generic JSON conversion instead, which turns a factor
  # into its underlying integer code -- silently pointing the annotation
  # at the wrong x category once the chart is live-updated post-render.
  lapply(seq_len(nrow(sub)), function(i) {
    list(
      x = as.character(sub$label[i]),
      y = label_y[i],
      text = sprintf("%.0f B", sub$abs_value[i]),
      showarrow = FALSE,
      yanchor = "bottom",
      font = list(color = "black", size = 11, family = "Arial")
    )
  })
}

# Shared white background / black border hover box style
WATERFALL_HOVERLABEL <- list(
  bgcolor = "white",
  bordercolor = "black",
  font = list(color = "black", size = 12, family = "Arial")
)

# Colors for time series (En-ROADS style)
TS_COLORS <- list(
  need = "#e74c3c",       # red - Climate Finance Need
  supply = "#27ae60",     # green - Total Supply
  gap = "#f39c12",        # orange - Gap
  pub = "#3498db",        # blue - Public
  
  priv = "#9b59b6",       # purple - Private
  leverage = "#1abc9c",   # teal - Leverage
  carbon = "#34495e"      # dark - Carbon Markets
)

#' Build the waterfall data frame (label/value/start/end/color/abs_value)
#' shared by the static render and the live plotlyProxy update.
waterfall_dataframe <- function(finance_result) {
  # Extract values (CFG is negative to show gap pointing downward)
  values <- c(
    finance_result$CF,
    -finance_result$PUB,
    -finance_result$PRIV,
    -finance_result$PRIVpub,
    -finance_result$PRIVdom,
    -finance_result$PRIVint,
    -finance_result$PRIVvol,
    -finance_result$CFG
  )
  
  # Calculate waterfall positions (cumulative starts)
  starts <- numeric(length(values))
  starts[1] <- 0
  for (i in 2:length(values)) {
    starts[i] <- starts[i - 1] + values[i - 1]
  }
  
  data.frame(
    label = factor(WATERFALL_LABELS, levels = WATERFALL_LABELS),
    value = values,
    start = starts,
    end = starts + values,
    color = WATERFALL_COLORS,
    abs_value = abs(values)
  )
}

#' Create waterfall chart matching the Python/Matplotlib style
create_waterfall_chart <- function(finance_result, chart_title) {
  df <- waterfall_dataframe(finance_result)
  
  # Create plotly chart with gradient overlay effect
  plot_ly(df, x = ~label) %>%
    add_bars(
      y = ~value,
      base = ~start,
      marker = list(
        color = ~color,
        line = list(color = "black", width = 1)
      ),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Value: %{customdata:.0f} B USD<extra></extra>"
      ),
      customdata = ~abs_value
    ) %>%
    layout(
      title = list(
        text = chart_title,
        font = list(size = 14, color = "#1a5f4a", family = "Montserrat")
      ),
      xaxis = list(
        title = "",
        tickangle = -35,
        tickfont = list(size = 14),
        automargin = TRUE
      ),
      yaxis = list(
        title = "Finance (Billion USD)",
        titlefont = list(size = 11),
        gridcolor = "#e0e0e0",
        zerolinecolor = "#333",
        range = c(0, 9500),
        dtick = 2000
      ),
      showlegend = FALSE,
      margin = list(b = 140, t = 80, l = 60, r = 20),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hoverlabel = WATERFALL_HOVERLABEL,
      annotations = waterfall_label_annotations(df, yaxis_max = 9500)
    ) %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
}

#' Create comparison waterfall chart (User Scenario vs Our Solution)
#' Uses same waterfall style as the main charts
create_comparison_waterfall <- function(user_result, icm_result) {
  df <- waterfall_dataframe(icm_result)
  
  # Create plotly chart (same style as main waterfall)
  plot_ly(df, x = ~label) %>%
    add_bars(
      y = ~value,
      base = ~start,
      marker = list(
        color = ~color,
        line = list(color = "black", width = 1)
      ),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Value: %{customdata:.0f} B USD<extra></extra>"
      ),
      customdata = ~abs_value
    ) %>%
    layout(
      title = list(
        text = "International Carbon Market with High Leverage Ratio",
        font = list(size = 16, color = "#1a5f4a", family = "Quicksand"),
        x = 0,
        xanchor = "left"
      ),
      xaxis = list(
        title = "",
        tickangle = -35,
        tickfont = list(size = 14),
        automargin = TRUE
      ),
      yaxis = list(
        title = "Finance (Billion USD)",
        titlefont = list(size = 11),
        gridcolor = "#e0e0e0",
        zerolinecolor = "#333",
        range = c(0, 9500),
        dtick = 2000
      ),
      showlegend = FALSE,
      margin = list(b = 140, t = 80, l = 60, r = 20),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hoverlabel = WATERFALL_HOVERLABEL,
      annotations = waterfall_label_annotations(df, yaxis_max = 9500)
    ) %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
}

#' Create En-ROADS style time series chart (Gap over time)
create_gap_timeseries <- function(bau_ts, custom_ts) {
  plot_ly() %>%
    # BAU Gap line
    add_trace(
      data = bau_ts,
      x = ~year, y = ~gap,
      type = "scatter", mode = "lines",
      name = "BAU Gap",
      line = list(color = "#95a5a6", width = 2, dash = "dash"),
      hovertemplate = "BAU %{x}: $%{y:.0f}B<extra></extra>"
    ) %>%
    # Custom scenario Gap line
    add_trace(
      data = custom_ts,
      x = ~year, y = ~gap,
      type = "scatter", mode = "lines+markers",
      name = "Your Scenario Gap",
      line = list(color = TS_COLORS$gap, width = 3),
      marker = list(size = 4),
      hovertemplate = "Your Scenario %{x}: $%{y:.0f}B<extra></extra>"
    ) %>%
    # Climate Finance Need reference line
    add_trace(
      data = custom_ts,
      x = ~year, y = ~CF,
      type = "scatter", mode = "lines",
      name = "Finance Need",
      line = list(color = TS_COLORS$need, width = 2, dash = "dot"),
      hovertemplate = "Need: $%{y:.0f}B<extra></extra>"
    ) %>%
    layout(
      title = list(
        text = "Climate Finance Gap Over Time",
        font = list(size = 16, color = "#2c3e50", family = "Quicksand")
      ),
      xaxis = list(
        title = "Year",
        range = c(2022, 2050),
        dtick = 5,
        gridcolor = "#ecf0f1"
      ),
      yaxis = list(
        title = "Billion USD",
        gridcolor = "#ecf0f1",
        zeroline = TRUE,
        zerolinecolor = "#27ae60",
        zerolinewidth = 2
      ),
      legend = list(
        orientation = "h", x = 0.5, xanchor = "center", y = -0.15
      ),
      hovermode = "x unified",
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    ) %>%
    config(displaylogo = FALSE)
}

#' Create En-ROADS style stacked area chart (Finance Sources) with smooth curves
create_supply_timeseries <- function(custom_ts) {
  # Use spline interpolation for smoother curves
  years_smooth <- seq(min(custom_ts$year), max(custom_ts$year), by = 0.5)
  
  smooth_data <- function(y_vals) {
    spline(custom_ts$year, y_vals, xout = years_smooth)$y
  }
  
  # Create smoothed data
  df_smooth <- data.frame(
    year = years_smooth,
    PUB = smooth_data(custom_ts$PUB),
    PRIV = smooth_data(custom_ts$PRIV),
    PRIVpub = smooth_data(custom_ts$PRIVpub),
    PRIVdom = smooth_data(custom_ts$PRIVdom),
    PRIVvol = smooth_data(custom_ts$PRIVvol),
    PRIVint = smooth_data(custom_ts$PRIVint),
    CF = smooth_data(custom_ts$CF)
  )
  
  # Ensure no negative values from spline
  
  df_smooth[df_smooth < 0] <- 0
  
  plot_ly(df_smooth, x = ~year) %>%
    add_trace(
      y = ~PUB, type = "scatter", mode = "none",
      stackgroup = "one", name = "Public Finance",
      fillcolor = "rgba(52, 152, 219, 0.85)",
      line = list(shape = "spline", smoothing = 1.3),
      hovertemplate = "Public: $%{y:.0f}B<extra></extra>"
    ) %>%
    add_trace(
      y = ~PRIV, type = "scatter", mode = "none",
      stackgroup = "one", name = "Private Finance",
      fillcolor = "rgba(155, 89, 182, 0.85)",
      line = list(shape = "spline", smoothing = 1.3),
      hovertemplate = "Private: $%{y:.0f}B<extra></extra>"
    ) %>%
    add_trace(
      y = ~PRIVpub, type = "scatter", mode = "none",
      stackgroup = "one", name = "Public Leverage",
      fillcolor = "rgba(241, 196, 15, 0.85)",
      line = list(shape = "spline", smoothing = 1.3),
      hovertemplate = "Leverage: $%{y:.0f}B<extra></extra>"
    ) %>%
    add_trace(
      y = ~PRIVdom, type = "scatter", mode = "none",
      stackgroup = "one", name = "Domestic Carbon Price",
      fillcolor = "rgba(46, 204, 113, 0.85)",
      line = list(shape = "spline", smoothing = 1.3),
      hovertemplate = "Dom Carbon Price: $%{y:.0f}B<extra></extra>"
    ) %>%
    add_trace(
      y = ~PRIVvol, type = "scatter", mode = "none",
      stackgroup = "one", name = "Voluntary Carbon Price",
      fillcolor = "rgba(230, 126, 34, 0.85)",
      line = list(shape = "spline", smoothing = 1.3),
      hovertemplate = "Vol Carbon Price: $%{y:.0f}B<extra></extra>"
    ) %>%
    add_trace(
      y = ~PRIVint, type = "scatter", mode = "none",
      stackgroup = "one", name = "International Carbon Price",
      fillcolor = "rgba(52, 73, 94, 0.85)",
      line = list(shape = "spline", smoothing = 1.3),
      hovertemplate = "Intl Carbon Market: $%{y:.0f}B<extra></extra>"
    ) %>%
    # Finance Need line on top
    add_trace(
      y = ~CF, type = "scatter", mode = "lines",
      name = "Finance Need",
      line = list(color = "#e74c3c", width = 3, dash = "dot"),
      hovertemplate = "Need: $%{y:.0f}B<extra></extra>"
    ) %>%
    layout(
      xaxis = list(
        title = "",
        range = c(2022, 2050),
        dtick = 5,
        gridcolor = "#ecf0f1",
        showgrid = TRUE
      ),
      yaxis = list(
        title = "Billion USD",
        gridcolor = "#ecf0f1",
        showgrid = TRUE
      ),
      legend = list(
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = -0.12,
        font = list(size = 10)
      ),
      hovermode = "x unified",
      margin = list(t = 20, b = 80),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    ) %>%
    config(displaylogo = FALSE)
}

# ==============================================================================
# USER INTERFACE
# ==============================================================================

app_theme <- bs_theme(
  bootswatch = "slate",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Quicksand"),
  primary = "#2ecc71",
  success = "#27ae60",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  bg = "#ffffff",
  fg = "#2c3e50"
)

custom_css <- "
  body { background: #f5f9f7; }
  .card { border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.08);
          background: #ffffff; }
  .card-header { border-radius: 12px 12px 0 0 !important; font-weight: 600; }
  .control-panel { background: #ffffff; border-radius: 12px; padding: 20px; }
  .slider-label { font-weight: 600; color: #27ae60; margin-bottom: 5px; }
  .form-label, .control-label { font-size: 12px !important; font-weight: 600 !important; margin-bottom: 0px !important; }
  .form-group { margin-bottom: 5px !important; }
  .irs { margin-top: 2px !important; margin-bottom: 8px !important; }
  .irs--shiny .irs-bar {
    background: linear-gradient(90deg, #f1c40f 0%, #27ae60 100%);
    height: 8px;
  }
  .irs--shiny .irs-line { background: #bdc3c7; height: 8px; }
  .irs--shiny .irs-handle {
    background: #3498db; border: 3px solid white;
    box-shadow: 0 2px 5px rgba(0,0,0,0.3); width: 22px; height: 22px; top: 21px;
  }
  .irs--shiny .irs-single {
    background: #27ae60; color: white; font-weight: 600;
    padding: 4px 10px; border-radius: 4px; font-size: 13px;
  }
  .irs--shiny .irs-min, .irs--shiny .irs-max {
    background: #ecf0f1; color: #7f8c8d; font-size: 12px;
    padding: 3px 8px; border-radius: 4px; visibility: visible !important;
  }
  @keyframes pulse {
    0%, 100% { transform: scale(1); }
    50% { transform: scale(1.03); }
  }
  /* !important plus a doubled selector: bslib's compiled bootswatch theme
     CSS for btn-default can otherwise win over this even though it is
     declared later, since shiny actionButton() always keeps btn-default
     in the class list alongside whatever we pass via class=. */
  .btn.btn-step-idle {
    background: #ffffff !important;
    color: #1e5631 !important;
    border: 2px solid #27ae60 !important;
  }
  .btn.btn-step-idle:hover, .btn.btn-step-idle:focus {
    background: #f0f8f5 !important;
    color: #1e5631 !important;
  }
  .about-box {
    background: #ffffff;
    border-left: 4px solid #27ae60;
    border-radius: 8px;
    padding: 20px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.05);
    height: 100%;
  }
  .bau-box {
    background: #ffffff;
    border-left: 4px solid #3498db;
    border-radius: 8px;
    padding: 15px 20px;
    margin-bottom: 15px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.05);
  }
  .guided-question-card {
    background: #ffffff;
    border-radius: 8px;
    padding: 12px 15px;
    margin-bottom: 10px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.05);
    font-size: 16px;
    color: #555;
    line-height: 1.6;
  }
  .params-col { display: flex; flex-direction: column; }
  .params-col > .card { flex: 0 0 auto; }
  .reflect-card {
    margin-top: 15px;
    flex: 1 1 auto;
    display: flex;
    flex-direction: column;
  }
  .reflect-title {
    display: flex; align-items: baseline; gap: 10px;
    margin: 0 2px 10px 2px;
  }
  .reflect-title strong { font-size: 16px; color: #1e5631; }
  .reflect-title span { font-size: 13px; color: #777; }
  .reflect-grid {
    flex: 1 1 auto;
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 12px;
  }
  .reflect-item {
    --accent: #27ae60; --tint: #f1faf5;
    position: relative;
    background: var(--tint);
    border-radius: 12px;
    border-top: 4px solid var(--accent);
    padding: 16px 16px 14px 16px;
    display: flex; flex-direction: column;
    box-shadow: 0 4px 15px rgba(0,0,0,0.06);
    transition: transform .2s ease, box-shadow .2s ease;
  }
  .reflect-item:hover {
    transform: translateY(-3px);
    box-shadow: 0 8px 20px rgba(0,0,0,0.12);
  }
  .reflect-item.q-intl { --accent: #00838f; --tint: #e0f7fa; }
  .reflect-item.q-vol  { --accent: #7b1fa2; --tint: #f3e5f5; }
  .reflect-item.q-pp   { --accent: #e65100; --tint: #fff3e0; }
  .reflect-icon {
    width: 36px; height: 36px; border-radius: 50%;
    background: var(--accent); color: #fff;
    display: flex; align-items: center; justify-content: center;
    font-size: 15px; margin-bottom: 12px;
  }
  .reflect-q {
    font-weight: 700; font-size: 15px; line-height: 1.4;
    color: #2c3e50; margin-bottom: 8px;
  }
  .reflect-hint {
    font-size: 13.5px; line-height: 1.5; color: #555;
    font-style: italic; margin-bottom: 12px;
  }
  .reflect-try {
    margin-top: auto;
    display: flex; flex-wrap: wrap; gap: 6px; align-items: center;
    font-size: 12px; color: #777;
  }
  .reflect-chip {
    background: #fff; color: var(--accent);
    border: 1px solid var(--accent);
    border-radius: 999px; padding: 2px 10px;
    font-weight: 600; font-size: 12px;
  }
  a.reflect-chip { text-decoration: none; cursor: pointer; transition: all .15s ease; }
  a.reflect-chip:hover { background: var(--accent); color: #fff; }
  .reflect-arrow { font-size: 10px; margin-left: 4px; }
  @keyframes slider-flash {
    0%   { box-shadow: 0 0 0 0 rgba(39,174,96,0.0); background: transparent; }
    15%  { box-shadow: 0 0 0 4px rgba(39,174,96,0.55); background: #fffde7; }
    100% { box-shadow: 0 0 0 0 rgba(39,174,96,0.0); background: transparent; }
  }
  .slider-flash {
    animation: slider-flash 2s ease-out;
    border-radius: 8px;
  }
  @media (max-width: 991px) {
    .reflect-grid { grid-template-columns: 1fr; }
  }
  .intro-box {
    background: linear-gradient(135deg, #e8f8f0 0%, #d4efdf 100%);
    border-left: 5px solid #27ae60;
    border-radius: 8px;
    padding: 25px 30px;
    font-size: 16px;
    line-height: 1.8;
    color: #2c3e50;
  }
  /* Tooltip icon styles */
  .tooltip-icon {
    cursor: pointer;
    color: #3498db;
    margin-left: 4px;
  }
  .tooltip-icon:hover {
    color: #2980b9;
  }
  /* Roboto font for all headings */
  h1, h2, h3, h4, h5, h6 {
    font-family: 'Roboto', sans-serif !important;
  }
  /* Landing Page Styles */
  .landing-page {
    background: #ffffff;
  }
  .navbar-top {
    background: white;
    padding: 20px 40px;
    border-bottom: 1px solid #f0f0f0;
    display: flex;
    align-items: center;
    justify-content: space-between;
  }
  .navbar-top .logo {
    font-size: 1.3rem;
    font-weight: 600;
    color: #333;
  }
  .navbar-top .instruction-btn {
    background: #333;
    color: white;
    padding: 10px 20px;
    border-radius: 6px;
    text-decoration: none;
    font-weight: 500;
    font-size: 0.95rem;
    transition: all 0.3s ease;
    border: none;
    cursor: pointer;
  }
  .navbar-top .instruction-btn:hover {
    background: #555;
    text-decoration: none;
    color: white;
  }
  .hero {
    padding: 80px 40px;
    max-width: 1200px;
    margin: 0 auto;
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 60px;
    align-items: center;
  }
  .hero-text h1 {
    font-size: 2.2rem;
    font-weight: 600;
    color: #1a1a1a;
    margin-bottom: 20px;
    line-height: 1.3;
  }
  .hero-text .subtitle {
    font-size: 1.1rem;
    color: #666;
    margin-bottom: 30px;
    line-height: 1.7;
  }
  .hero-image img {
    width: 100%;
    border-radius: 12px;
    box-shadow: 0 10px 40px rgba(0,0,0,0.08);
  }
  .cta-btn {
    display: inline-block;
    background: #333;
    color: white;
    padding: 14px 40px;
    border-radius: 8px;
    text-decoration: none;
    font-weight: 500;
    font-size: 1rem;
    transition: all 0.3s ease;
    border: none;
    cursor: pointer;
  }
  .cta-btn:hover {
    background: #555;
    transform: translateY(-2px);
    box-shadow: 0 8px 20px rgba(0, 0, 0, 0.15);
  }
  .features {
    background: #ffffff;
    padding: 80px 40px;
  }
  .features-container {
    max-width: 1200px;
    margin: 0 auto;
  }
  .features h2 {
    font-size: 1.8rem;
    font-weight: 600;
    color: #1a1a1a;
    margin-bottom: 50px;
    text-align: center;
  }
  .features-grid {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 40px;
  }
  .feature-item {
    background: white;
    padding: 40px;
    border-radius: 12px;
    box-shadow: 0 4px 15px rgba(0,0,0,0.05);
    transition: all 0.3s ease;
  }
  .feature-item:hover {
    transform: translateY(-5px);
    box-shadow: 0 12px 30px rgba(0,0,0,0.1);
  }
  .feature-icon {
    font-size: 2.5rem;
    color: #666;
    margin-bottom: 20px;
  }
  .feature-item h3 {
    font-size: 1.2rem;
    font-weight: 600;
    color: #1a1a1a;
    margin-bottom: 15px;
  }
  .feature-item p {
    color: #666;
    font-size: 0.95rem;
    line-height: 1.6;
  }
  .landing-footer {
    background: #f0f0f0;
    padding: 40px;
    display: flex;
    justify-content: space-between;
    align-items: center;
    color: #666;
    font-size: 0.9rem;
  }
  .footer-left {
    flex: 1;
    text-align: left;
  }
  .footer-right {
    flex: 1;
    text-align: right;
  }
  .contact-label {
    font-weight: 600;
    color: #333;
    margin-right: 20px;
  }
  .contact-email a {
    color: #27ae60;
    text-decoration: none;
    font-weight: 500;
  }
  .contact-email a:hover {
    text-decoration: underline;
  }
  /* ===== MOBILE MODE (only for QR code users: ?mode=mobile) ===== */

  /* Landing page responsive (always applies on small screens) */
  @media (max-width: 768px) {
    .hero { grid-template-columns: 1fr; gap: 30px; padding: 40px 20px; }
    .hero-text h1 { font-size: 1.6rem; }
    .hero-text .subtitle { font-size: 0.95rem; }
    .features { padding: 40px 20px; }
    .features-grid { grid-template-columns: 1fr; }
    .navbar-top { padding: 12px 16px; }
    .landing-footer { flex-direction: column; gap: 10px;
      text-align: center; padding: 20px; }
    .footer-left, .footer-right { text-align: center; }
  }

  /* ===== Mobile-mode-only styles (QR scan) ===== */
  .mobile-mode .col-sm-3, .mobile-mode .col-sm-6,
  .mobile-mode .col-sm-9 {
    flex: 0 0 100% !important;
    max-width: 100% !important;
  }
  .mobile-mode .container-fluid.py-4 { padding: 8px !important; }
  .mobile-mode h3 { font-size: 1.1rem !important; }
  .mobile-mode h4 { font-size: 1rem !important; }
  .mobile-mode .card { margin-bottom: 8px; }
  .mobile-mode .card-body { padding: 10px !important; }
  .mobile-mode .card-header { padding: 8px !important;
    font-size: 0.85rem; }
  .mobile-mode .form-label,
  .mobile-mode .control-label { font-size: 11px !important; }
  .mobile-mode .irs--shiny .irs-single {
    font-size: 11px; padding: 2px 5px; }
  .mobile-mode .irs--shiny .irs-min,
  .mobile-mode .irs--shiny .irs-max { font-size: 10px; }
  .mobile-mode .form-group { margin-bottom: 3px !important; }
  .mobile-mode .irs {
    margin-top: 0 !important; margin-bottom: 5px !important; }
  .mobile-mode .btn {
    font-size: 0.85rem !important; padding: 8px 12px !important; }
  .mobile-mode .card-body p {
    font-size: 13px !important; line-height: 1.5 !important; }

  /* Floating chart window */
  .mobile-mode #scenario-interactive .chart-col {
    position: fixed !important;
    bottom: 36px !important;
    left: 0 !important;
    right: 0 !important;
    width: 100% !important;
    z-index: 9999;
    background: #fff;
    box-shadow: 0 -4px 20px rgba(0,0,0,0.25);
    border-top: 3px solid #27ae60;
    padding: 0 !important;
    max-height: 55vh;
    overflow-y: auto;
    transition: transform 0.3s ease;
    flex: none !important;
    max-width: 100% !important;
  }
  .mobile-mode #scenario-interactive .chart-col.collapsed {
    transform: translateY(100%);
  }
  .mobile-mode #scenario-interactive .chart-col .card {
    margin: 0 !important; border-radius: 0 !important;
    border: none !important; box-shadow: none !important;
  }
  .mobile-mode #scenario-interactive .chart-col .card-header {
    display: none !important;
  }
  .mobile-mode #scenario-interactive .chart-col .card-body {
    padding: 2px !important;
  }
  /* Chart height on mobile */
  .mobile-mode #scenario-interactive .chart-col
    #chart_2030_custom {
    height: 50vh !important;
  }
  .mobile-mode #scenario-interactive .chart-col
    .js-plotly-plot, .mobile-mode #scenario-interactive
    .chart-col .plotly {
    height: 50vh !important;
  }
  /* Hide plotly toolbar on mobile */
  .mobile-mode .modebar-container {
    display: none !important;
  }

  /* Hide download/hr/h4 in floating chart */
  .mobile-mode #scenario-interactive .chart-col hr,
  .mobile-mode #scenario-interactive .chart-col h4 {
    display: none !important; }
  .mobile-mode #scenario-interactive .chart-col .btn-info {
    display: none !important; }
  /* Download row hidden */
  .mobile-mode #scenario-interactive .chart-col
    .card-body > :last-child { display: none !important; }

  /* Add bottom padding so params don't hide behind floating chart */
  .mobile-mode #scenario-interactive .col-sm-6:first-child {
    padding-bottom: 60vh !important;
  }

  /* Button bar at top of floating chart */
  .mobile-mode #scenario-interactive .chart-col
    .card-body > hr + .row {
    position: absolute;
    top: 2px; left: 2px; right: 2px;
    z-index: 100;
    display: flex !important;
    flex-wrap: nowrap !important;
    margin: 0 !important;
    padding: 0 !important;
    gap: 3px;
  }
  .mobile-mode #scenario-interactive .chart-col
    .card-body > hr + .row > .col-sm-6 {
    flex: none !important;
    max-width: none !important;
    width: auto !important;
    padding: 0 !important;
  }
  .mobile-mode #scenario-interactive .chart-col #show_icm_btn,
  .mobile-mode #scenario-interactive .chart-col #hide_icm_btn {
    font-size: 7px !important;
    padding: 3px 5px !important;
    width: auto !important;
    border-radius: 4px;
    opacity: 0.9;
  }
  .mobile-mode #scenario-interactive .chart-col #show_icm_btn .fa,
  .mobile-mode #scenario-interactive .chart-col #hide_icm_btn .fa,
  .mobile-mode #scenario-interactive .chart-col #show_icm_btn .fas,
  .mobile-mode #scenario-interactive .chart-col #hide_icm_btn .fas {
    font-size: 7px !important;
  }

  /* Gap label: inline with solution buttons */
  .mobile-mode #remaining_gap_label {
    font-size: 7px !important;
    padding: 3px 5px !important;
    top: 2px !important;
    right: 2px !important;
    border-radius: 4px;
  }
  .mobile-mode #gap_tooltip {
    top: 22px !important;
    right: 2px !important;
    min-width: 180px !important;
    padding: 8px !important;
    font-size: 11px !important;
  }

  /* Toggle button for floating chart */
  #mobile-chart-toggle {
    display: none;
  }
  .mobile-mode #mobile-chart-toggle {
    display: block !important;
    position: fixed;
    bottom: 0;
    left: 0;
    right: 0;
    width: 100%;
    z-index: 10000;
    background: #27ae60;
    color: white;
    border: none;
    padding: 8px;
    font-size: 14px;
    font-weight: 600;
    text-align: center;
    cursor: pointer;
  }

  /* Add bottom padding so params not hidden behind float */
  .mobile-mode #scenario-interactive .params-col {
    padding-bottom: 50vh !important;
  }

  /* Single column params on mobile */
  .mobile-mode #scenario-interactive .params-col
    .card-body > .row > .col-sm-6 {
    flex: 0 0 100% !important;
    max-width: 100% !important;
  }
  .mobile-mode #scenario-interactive .params-col h6 {
    font-size: 0.8rem !important; }
"

# Helper function to create label with clickable tooltip icon
labelWithTooltip <- function(label, definition, whatItDoes) {
  tooltip_content <- paste0(
    '<div style=\"text-align:left;\">',
    '<strong style=\"color:#f39c12;\">📖 Definition:</strong><br>',
    definition, '<br><br>',
    '<strong style=\"color:#27ae60;\">⚙️ What it does:</strong><br>',
    whatItDoes,
    '</div>'
  )
  tags$span(
    label,
    tags$i(
      class = "fa fa-info-circle tooltip-icon",
      `data-bs-toggle` = "tooltip",
      `data-bs-html` = "true",
      `data-bs-placement` = "top",
      `data-bs-trigger` = "click",
      title = tooltip_content
    )
  )
}

ui <- page_fluid(
  shinyjs::useShinyjs(),
  theme = app_theme,
  tags$head(
    # Mobile viewport
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1, maximum-scale=1"),
    # Import Roboto font from Google Fonts
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;600;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML(custom_css)),
    # Initialize Bootstrap tooltips + mobile mode detection
    tags$script(HTML("
      // Show fraction-valued sliders as percentages (model values stay fractions)
      (function() {
        var cfg = {r: [2, 5], g: [2, 4], C_Adv: [0, 5], C_EMDEs: [0, 5]};
        var tries = 0;
        function apply() {
          var pending = 0;
          Object.keys(cfg).forEach(function(id) {
            var s = $('#' + id).data('ionRangeSlider');
            if (!s) { pending++; return; }
            if (s.__pct) return;
            var dec = cfg[id][0], grid = cfg[id][1];
            var pretty = function(v) { return (+(v * 100).toFixed(dec)) + '%'; };
            // keep % formatting when the server updates the slider (Show Solution, Reset, steps)
            var origUpdate = s.update;
            s.update = function(o) {
              o = o || {};
              o.prettify = pretty;
              o.grid_num = grid;
              return origUpdate.call(s, o);
            };
            s.update({});
            s.__pct = true;
          });
          if (pending && tries++ < 50) setTimeout(apply, 200);
        }
        $(document).on('shiny:connected', function() { setTimeout(apply, 100); });
      })();
    ")),
    tags$script(HTML("
      $(document).on('click', 'a.reflect-chip', function(e) {
        e.preventDefault();
        var id = $(this).data('target');
        var el = document.getElementById(id);
        if (!el) return;
        var box = $(el).closest('.shiny-input-container')[0] || el;
        box.scrollIntoView({behavior: 'smooth', block: 'center'});
        $(box).removeClass('slider-flash');
        void box.offsetWidth;
        $(box).addClass('slider-flash');
        setTimeout(function() { $(box).removeClass('slider-flash'); }, 2100);
      });
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        $('body').tooltip({
          selector: '[data-bs-toggle=\"tooltip\"]',
          html: true,
          trigger: 'hover',
          container: 'body'
        });
        // Detect ?mode=mobile in URL for QR code users
        var params = new URLSearchParams(window.location.search);
        if (params.get('mode') === 'mobile') {
          $('body').addClass('mobile-mode');
          Shiny.setInputValue('is_mobile', true);
          // Auto-skip landing page on mobile
          setTimeout(function() {
            var btn = document.getElementById('enter_tool');
            if (btn) btn.click();
          }, 500);
        } else {
          Shiny.setInputValue('is_mobile', false);
        }
      });
    "))
  ),
  
  # Hidden input to track landing page state
  shinyjs::hidden(
    div(
      id = "landing_state",
      textInput("show_landing", "", value = "true")
    )
  ),
  
  # ========== LANDING PAGE ==========
  conditionalPanel(
    condition = "input.show_landing == 'true'",
    div(
      class = "landing-page",
      # Navbar
      div(
        class = "navbar-top",
        div(class = "logo", icon("globe"), " Climate Finance Gap Explorer"),
        tags$a(
          href = "Interactive Tool Instruction.PDF",
          target = "_blank",
          class = "instruction-btn",
          "Interactive Tool Instruction"
        )
      ),
      # Hero Section
      div(
        class = "hero",
        div(
          class = "hero-text",
          h1("Bridge the Climate Finance Gap"),
          p(
            class = "subtitle",
            "Explore how different policy choices can help close the ",
            "USD 7.1 trillion annual climate finance gap by 2030."
          ),
          p(
            class = "subtitle",
            "Start from a business-as-usual baseline, then try the levers yourself: ",
            "expanding domestic carbon pricing, opening up international carbon markets, ",
            "and raising leverage ratios. See how far each one gets us toward closing the gap.",
            style = "font-size: 1rem; color: #555;"
          ),
          tags$a(
            href = "#",
            class = "cta-btn",
            onclick = "Shiny.setInputValue('enter_tool', true);",
            icon("arrow-right"), " Launch Interactive Tool"
          )
        ),
        div(
          class = "hero-image",
          img(
            src = "https://images.unsplash.com/photo-1446776653964-20c1d3a81b06?w=600&h=500&fit=crop",
            alt = "Climate Finance"
          )
        )
      ),
      # Features Section
      div(
        class = "features",
        div(
          class = "features-container",
          h2("What You Can Do"),
          div(
            class = "features-grid",
            div(
              class = "feature-item",
              div(class = "feature-icon", icon("sliders-h")),
              h3("Adjust Parameters"),
              p("Customize key variables like public finance growth, carbon pricing, and leverage ratios to model different scenarios.")
            ),
            div(
              class = "feature-item",
              div(class = "feature-icon", icon("chart-bar")),
              h3("Visualize Impact"),
              p("See real-time waterfall charts showing how your choices affect the climate finance gap through 2030.")
            ),
            div(
              class = "feature-item",
              div(class = "feature-icon", icon("download")),
              h3("Download Results"),
              p("Export your scenario parameters and charts for reports, presentations, and further analysis.")
            )
          )
        )
      ),
      # Footer with Contact
      div(
        class = "landing-footer",
        div(
          class = "footer-left",
          "Reference | ",
          tags$a(href = "https://www.nature.com/articles/s44168-025-00220-x", target = "_blank", "Read the paper")
        ),
        div(
          class = "footer-right",
          span(class = "contact-label", "Contact"),
          span(class = "contact-email",
               tags$a(href = "mailto:xihu@edf.org", "xihu@edf.org"))
        )
      )
    )
  ),
  
  # ========== TITLE ==========
  conditionalPanel(
    condition = "input.show_landing == 'false'",
    div(
      class = "text-center py-3",
      style = "background: #ffffff; border-bottom: 1px solid #f0f0f0;",
      h1(
        icon("globe-americas", class = "fa-lg"),
        " Climate Finance Gap Explorer",
        style = paste0(
          "color: #333; font-weight: 600; letter-spacing: 1px; ",
          "font-family: 'Roboto', sans-serif; font-size: 1.8rem; margin: 0;"
        )
      )
    )
  ),
  
  # ========== MAIN CONTENT (only show when not on landing page) ==========
  conditionalPanel(
    condition = "input.show_landing == 'false'",
    
    # ========== MAIN INTERACTION: ABOUT (LEFT) + CONTROLS + YOUR SCENARIO (RIGHT) ==========
    div(
      class = "container-fluid py-4",
      style = "background: #f0f8f5; border-top: 5px solid #1e5631;",
      
      fluidRow(
        # ===== LEFT: About Panel (3 columns) =====
        column(
          3,
          card(
            card_header(
              style = "background: #d4efdf; color: #1e5631; padding: 15px;",
              icon("info-circle"), " About"
            ),
            card_body(
              style = "padding: 20px; background: #f0f8f5;",
              p(
                "The Climate Finance Gap Explorer helps clarify a simple but urgent reality: the world is not investing enough to meet its climate goals.",
                style = "font-size: 14px; line-height: 1.6; color: #333; margin-bottom: 15px;"
              ),
              p(
                strong("In 2022,"), " a gap of USD 7.1 trillion remained between what is needed and what is actually flowing, especially to emerging economies.",
                style = "font-size: 14px; line-height: 1.6; color: #333; margin-bottom: 15px;"
              ),
              p(
                "This tool shows how policy choices could meaningfully shift this trajectory by 2030. Adjust assumptions below to see how different strategies, from public finance growth to carbon market expansion, can help achieve our 2030 goals.",
                style = "font-size: 14px; line-height: 1.6; color: #333; margin-bottom: 15px;"
              ),
              p(
                "This tool builds on our published paper (Kerr et al., 2025, npj Climate Action), ",
                "which estimates that about USD 8.4 trillion a year is needed by 2030 and finds that ",
                "international carbon markets paired with higher leverage ratios could close the gap. ",
                "This version updates that work in two ways: the baseline now assumes domestic carbon ",
                "pricing stays at its current level (World Bank, 2026), and the solution is split into three steps you can ",
                "stack, namely domestic expansion, international markets, and higher leverage. ",
                tags$a(
                  href = "https://www.nature.com/articles/s44168-025-00220-x",
                  target = "_blank",
                  "Read the paper",
                  style = "color: #1e5631; text-decoration: underline; font-weight: 600;"
                ),
                " for the full method.",
                style = "font-size: 14px; line-height: 1.6; color: #333;"
              )
            )
          )
        ),
        
        # ===== RIGHT: Baseline Charts (9 columns) =====
        column(
          9,
          div(
            class = "bau-box",
            p(
              "Even if public and private finance keep growing at their historical pace, ",
              "that growth alone isn't enough. Without domestic carbon pricing expanding ",
              "beyond its current level, the climate finance gap in 2030 is still about ",
              "1.4 times everything already being financed.",
              style = "margin: 0; font-size: 16px; color: #555; line-height: 1.6;"
            )
          ),
          fluidRow(
            column(6,
                   card(
                     card_header(style = "background: #272b30; color: white;",
                                 icon("chart-area"), " 2022 Baseline"),
                     card_body(plotlyOutput("chart_2022", height = "400px"))
                   )
            ),
            column(6,
                   card(
                     card_header(style = "background: #272b30; color: white;",
                                 icon("chart-area"), " 2030 BAU"),
                     card_body(plotlyOutput("chart_2030_bau", height = "400px"))
                   )
            )
          )
        )
      ),
      
      # ========== BUILD YOUR SCENARIO (full-width) ==========
      fluidRow(
        column(
          12,
          div(
            class = "text-center mb-3",
            style = paste0(
              "background: linear-gradient(90deg, #1e5631, #27ae60);",
              "color: white; padding: 15px; border-radius: 10px;",
              "margin-bottom: 15px;"
            ),
            h3(icon("sliders-h", class = "fa-lg"), " Build Your Scenario",
               style = "color: #fff; font-weight: 700; margin: 0;"),
            p("Adjust parameters to see if you can close the climate finance gap!",
              style = "color: #d4efdf; margin: 5px 0 0 0; font-size: 14px;")
          ),
          # ===== Where Things Stand Today + Guided Questions =====
          fluidRow(
            class = "mb-3",
            column(
              5,
              div(
                class = "about-box",
                h5(icon("mountain"), " Where things stand today",
                   style = "color: #1e5631; font-weight: 700; margin-bottom: 12px;"),
                p(
                  "As of 2025, there are 80 carbon pricing instruments in operation ",
                  "worldwide, 43 carbon taxes and 37 emissions trading systems, together ",
                  "covering about 28% of global greenhouse gas emissions and raising over ",
                  "USD 100 billion in revenue in 2024.",
                  style = "font-size: 16px; color: #333; line-height: 1.6; margin-bottom: 12px;"
                ),
                p(
                  tags$strong("What's next for 2030?"),
                  " Closing the remaining gap will take more than holding steady: ",
                  "expanding carbon pricing to higher prices and broader coverage ",
                  "(including EMDEs establishing their own policies), building out ",
                  "international carbon markets, and raising leverage ratios so each ",
                  "dollar of carbon payment mobilises more private investment.",
                  style = "font-size: 16px; color: #333; line-height: 1.6; margin-bottom: 12px;"
                ),
                p(
                  em("Sources: World Bank, State and Trends of Carbon Pricing 2025; ICAP, Status Report 2026."),
                  style = "font-size: 12px; color: #777; line-height: 1.5; margin-bottom: 0;"
                )
              )
            ),
            column(
              7,
              tags$strong("Guided questions to explore:",
                          style = "display: block; margin-bottom: 10px; color: #1e5631;"),
              div(
                class = "guided-question-card",
                icon("question-circle", style = "color: #27ae60;"),
                tags$strong(" What if carbon prices rise and coverage grows?", style = "color: #1e5631;"),
                " Suppose domestic carbon markets raise their prices and widen their ",
                "coverage, while EMDEs start carbon pricing policies of their own. How ",
                "much additional finance could that mobilise? Try the sliders in the ",
                "Parameters panel to find out."
              ),
              div(
                class = "guided-question-card",
                icon("question-circle", style = "color: #27ae60;"),
                tags$strong(" Can international carbon markets help?", style = "color: #1e5631;"),
                " Cooperation between countries can also mobilise climate finance. Try ",
                "the International Market Size slider under International Carbon Price to see how much ",
                "international carbon markets could contribute on their own."
              ),
              div(
                class = "guided-question-card",
                icon("question-circle", style = "color: #27ae60;"),
                tags$strong(" Can higher leverage unlock more investment?", style = "color: #1e5631;"),
                " If carbon pricing revenue can leverage more private capital, it may ",
                "create a better investment environment overall. Try the Leverage ",
                "Ratio sliders to see the effect."
              )
            )
          ),
          
          # Gap status message (below the guided questions)
          uiOutput("gap_status"),
          
          div(id = "scenario-interactive",
              fluidRow(
                # ===== PARAMETERS CARD (6 columns) =====
                column(
                  6, class = "params-col",
                  card(
                    card_header(style = "background: #272b30; color: white;",
                                icon("sliders-h"), " Parameters"),
                    card_body(
                      fluidRow(
                        # Left column: Public + Private and International Carbon
                        column(6,
                               # Public + Private section
                               div(
                                 style = "background: #fff3e0; padding: 10px; border-radius: 8px;
                           margin-bottom: 15px;",
                                 tags$h6(icon("landmark"), " Public + Private",
                                         style = "color: #e65100; margin-bottom: 10px;"),
                                 sliderInput("r",
                                             label = labelWithTooltip(
                                               "Public Growth Rate",
                                               "Annual growth rate of public climate finance.",
                                               "Controls how fast government and public institutions increase climate-related funding over time."
                                             ),
                                             min = 0, max = 0.25,
                                             value = DEFAULT_PARAMS$r, step = 0.0001),
                                 sliderInput("g",
                                             label = labelWithTooltip(
                                               "Private Growth Rate",
                                               "Annual growth rate of private climate finance not driven by carbon pricing or public leverage.",
                                               "Controls baseline growth of market-driven private investment in climate mitigation."
                                             ),
                                             min = 0, max = 0.20,
                                             value = DEFAULT_PARAMS$g, step = 0.0001),
                                 sliderInput("Lhighpub",
                                             label = labelWithTooltip(
                                               "Public+Private Leverage Ratio",
                                               "Amount of private finance mobilised per dollar of public finance.",
                                               "Captures the crowd-in effect of public funding through guarantees, subsidies, and risk-sharing instruments."
                                             ),
                                             min = 0.1, max = 1.5,
                                             value = DEFAULT_PARAMS$Lhighpub, step = 0.1)
                               ),
                               # International Carbon section
                               div(
                                 style = "background: #e0f7fa; padding: 10px; border-radius: 8px;",
                                 tags$h6(icon("globe"), " International Carbon Price",
                                         style = "color: #00838f; margin-bottom: 10px;"),
                                 sliderInput("ICMS_2030",
                                             label = labelWithTooltip(
                                               "International Market Size",
                                               "Annual transaction value of the international carbon market.",
                                               "Determines how much capital is transferred through cross-border carbon trading mechanisms."
                                             ),
                                             min = 0, max = 500,
                                             value = DEFAULT_PARAMS$ICMS_2030, step = 10, post = " B"),
                                 sliderInput("Lcp_int",
                                             label = labelWithTooltip(
                                               "International Carbon Price Leverage Ratio",
                                               "Private investment mobilised per dollar of international carbon market transactions.",
                                               "Measures how international carbon markets amplify finance beyond direct credit payments."
                                             ),
                                             min = 1, max = 20,
                                             value = DEFAULT_PARAMS$Lcp_int, step = 1)
                               ),
                               hr(),
                               actionButton("reset_btn", label = "Reset All",
                                            icon = icon("sync-alt"),
                                            class = "btn btn-success w-100")
                        ),

                        # Right column: Domestic + Voluntary
                        column(6,
                               # Domestic Carbon section
                               div(
                                 style = "background: #e8f5e9; padding: 10px; border-radius: 8px;
                           margin-bottom: 15px;",
                                 tags$h6(icon("industry"), " Domestic Carbon Price",
                                         style = "color: #2e7d32; margin-bottom: 10px;"),
                                 sliderInput("P_Adv",
                                             label = labelWithTooltip(
                                               "Advanced Economies Price ($/tCO2)",
                                               "Average carbon price level in Advanced economies.",
                                               "Determines price incentives for emissions reductions in advanced economies."
                                             ),
                                             min = 0, max = 80, value = DEFAULT_PARAMS$P_Adv, step = 0.1),
                                 sliderInput("C_Adv",
                                             label = labelWithTooltip(
                                               "Advanced Economies Coverage",
                                               "Share of global emissions covered by carbon pricing in Advanced economies.",
                                               "Controls how much of the economy is affected by carbon pricing in Advanced economies."
                                             ),
                                             min = 0, max = 0.5, value = DEFAULT_PARAMS$C_Adv, step = 0.01),
                                 sliderInput("P_EMDEs",
                                             label = labelWithTooltip(
                                               "EMDEs Price ($/tCO2)",
                                               "Average carbon price level in Emerging market and developing economies (EMDEs).",
                                               "Represents modest price incentives due to institutional or economic constraints in EMDEs."
                                             ),
                                             min = 0, max = 80, value = DEFAULT_PARAMS$P_EMDEs, step = 5),
                                 sliderInput("C_EMDEs",
                                             label = labelWithTooltip(
                                               "EMDEs Coverage",
                                               "Share of global emissions covered by carbon pricing in Emerging market and developing economies (EMDEs).",
                                               "Controls carbon pricing coverage in EMDEs with nascent or developing systems."
                                             ),
                                             min = 0, max = 0.5, value = DEFAULT_PARAMS$C_EMDEs, step = 0.01),
                                 sliderInput("Lcp_dom",
                                             label = labelWithTooltip(
                                               "Domestic Carbon Price Leverage Ratio",
                                               "Private capital mobilised per dollar of domestic carbon pricing revenue.",
                                               "Captures how effectively carbon revenues translate into real investment."
                                             ),
                                             min = 1, max = 20, value = DEFAULT_PARAMS$Lcp_dom, step = 1)
                               ),
                               # Voluntary Carbon Market section
                               div(
                                 style = "background: #f3e5f5; padding: 10px; border-radius: 8px;",
                                 tags$h6(icon("leaf"), " Voluntary Carbon Price",
                                         style = "color: #7b1fa2; margin-bottom: 10px;"),
                                 sliderInput("VMS_2030",
                                             label = labelWithTooltip(
                                               "Voluntary Market Size",
                                               "Annual transaction value of the voluntary carbon market.",
                                               "Controls how much capital is mobilised through voluntary carbon credit purchases."
                                             ),
                                             min = 0, max = 100, value = DEFAULT_PARAMS$VMS_2030, step = 5, post = " B"),
                                 sliderInput("Lcp_vol",
                                             label = labelWithTooltip(
                                               "Voluntary Carbon Market Leverage Ratio",
                                               "Private investment mobilised per dollar spent in the voluntary carbon market.",
                                               "Measures whether voluntary credits lead to additional mitigation investment."
                                             ),
                                             min = 1, max = 20, value = DEFAULT_PARAMS$Lcp_vol, step = 1)
                               )
                        )
                      )
                    )
                  )
                  ,
                  # Reflection questions (fills the blank space below the Parameters card)
                  div(
                    class = "reflect-card",
                    div(class = "reflect-title",
                        strong(icon("lightbulb"), " Think it through"),
                        span("Questions to explore after building your scenario")),
                    div(
                      class = "reflect-grid",
                      div(
                        class = "reflect-item q-intl",
                        div(class = "reflect-icon", icon("globe")),
                        div(class = "reflect-q",
                            "How could international carbon markets push domestic markets to raise ambition?"),
                        div(class = "reflect-hint",
                            "Could cheaper compliance abroad let countries aim higher at home?"),
                        div(
                          class = "reflect-try",
                          span("Try:"),
                          tags$a(href = "#", class = "reflect-chip", `data-target` = "ICMS_2030",
                                  title = "Jump to this slider", "International Market Size", icon("arrow-up", class = "reflect-arrow"))
                        )
                      ),
                      div(
                        class = "reflect-item q-vol",
                        div(class = "reflect-icon", icon("leaf")),
                        div(class = "reflect-q",
                            "Under Article 6, are voluntary and international markets substitutes or complements?"),
                        div(class = "reflect-hint",
                            "Do they compete for the same credits, or build on each other?"),
                        div(
                          class = "reflect-try",
                          span("Try:"),
                          tags$a(href = "#", class = "reflect-chip", `data-target` = "ICMS_2030",
                                  title = "Jump to this slider", "International Market Size", icon("arrow-up", class = "reflect-arrow")),
                          tags$a(href = "#", class = "reflect-chip", `data-target` = "VMS_2030",
                                  title = "Jump to this slider", "Voluntary Market Size", icon("arrow-up", class = "reflect-arrow"))
                        )
                      ),
                      div(
                        class = "reflect-item q-pp",
                        div(class = "reflect-icon", icon("landmark")),
                        div(class = "reflect-q",
                            "How do public and private growth rates relate?"),
                        div(class = "reflect-hint",
                            "Does one pull the other up, or are you counting the same effect twice?"),
                        div(
                          class = "reflect-try",
                          span("Try:"),
                          tags$a(href = "#", class = "reflect-chip", `data-target` = "r",
                                  title = "Jump to this slider", "Public Growth Rate", icon("arrow-up", class = "reflect-arrow")),
                          tags$a(href = "#", class = "reflect-chip", `data-target` = "g",
                                  title = "Jump to this slider", "Private Growth Rate", icon("arrow-up", class = "reflect-arrow"))
                        )
                      )
                    )
                  )
                ),
                
                # ===== RIGHT: Your Scenario Chart (6 columns) =====
                column(
                  6, class = "chart-col",
                  card(
                    card_header(
                      style = "background: #272b30; color: white;",
                      icon("chart-area"), " Your Scenario in 2030"
                    ),
                    card_body(
                      div(
                        style = "position: relative;",
                        plotlyOutput("chart_2030_custom", height = "600px"),
                        # Remaining Gap label with click tooltip
                        div(
                          id = "remaining_gap_label",
                          style = paste0(
                            "position: absolute; top: 80px; right: 20px; ",
                            "background: #27ae60; padding: 10px 15px; ",
                            "border-radius: 6px; cursor: pointer; ",
                            "font-weight: 600; color: white; font-size: 14px; ",
                            "border: 2px solid #27ae60; transition: all 0.3s ease;"
                          ),
                          "Check Remaining Gap"
                        ),
                        # Hidden tooltip that shows on click
                        div(
                          id = "gap_tooltip",
                          style = paste0(
                            "position: absolute; top: 130px; right: 20px; ",
                            "background: white; border: 2px solid #333; ",
                            "border-radius: 8px; padding: 20px; ",
                            "min-width: 250px; box-shadow: 0 4px 15px rgba(0,0,0,0.2); ",
                            "z-index: 1000; display: none; text-align: center;"
                          ),
                          div(id = "gap_tooltip_inner")
                        )
                      ),
                      hr(),
                      p("Build the solution step by step. Click an active step again to undo it.",
                        class = "text-center mb-1",
                        style = "font-size: 13px; color: #555;"),
                      uiOutput("step_status_text"),
                      fluidRow(
                        column(4,
                               actionButton("step1_btn", "Step 1: Expand Domestic Price",
                                            class = "btn btn-step-idle w-100")
                        ),
                        column(4,
                               actionButton("step2_btn", "Step 2: Expand International",
                                            class = "btn btn-step-idle w-100")
                        ),
                        column(4,
                               actionButton("step3_btn", "Step 3: Raise Leverage Ratio",
                                            class = "btn btn-step-idle w-100")
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(6,
                               actionButton("show_icm_btn",
                                            label = "Show Our Solution",
                                            icon = icon("lightbulb"),
                                            class = "btn btn-warning w-100")
                        ),
                        column(6,
                               actionButton("hide_icm_btn", label = "Hide Our Solution",
                                            icon = icon("eye-slash"),
                                            class = "btn btn-secondary w-100")
                        )
                      ),
                      hr(),
                      h4("Download Your Scenario",
                         class = "text-center mb-3",
                         style = paste0(
                           "font-weight: 600; font-size: 1.3rem; color: #2c3e50; ",
                           "letter-spacing: 0.5px; font-family: 'Roboto', sans-serif;"
                         )),
                      fluidRow(
                        column(6,
                               downloadButton("download_params",
                                              label = "Download Parameters",
                                              icon = icon("file-csv"),
                                              class = "btn btn-info w-100")
                        ),
                        column(6,
                               downloadButton("download_chart",
                                              label = "Download Chart",
                                              icon = icon("chart-bar"),
                                              class = "btn btn-info w-100")
                        )
                      )
                    )
                  )
                )
              ) # end fluidRow params+chart
          ) # end div scenario-interactive
        ) # end column(12)
      ), # end fluidRow Build Your Scenario
      
      # Mobile floating chart toggle button (hidden on desktop)
      tags$button(
        id = "mobile-chart-toggle",
        onclick = paste0(
          "var c = document.querySelector('.chart-col');",
          "if(c) c.classList.toggle('collapsed');",
          "var btn = document.getElementById('mobile-chart-toggle');",
          "if(c && c.classList.contains('collapsed')){",
          "btn.innerHTML='&#9650; Show Chart';}",
          "else{btn.innerHTML='&#9660; Hide Chart';}"
        ),
        HTML("&#9660; Hide Chart")
      )
    ),
    
    # ========== FOOTER ==========
    div(
      class = "text-center py-3 text-muted",
      style = "background: #f8f9fa;",
      icon("leaf"), " Climate Finance Gap Analysis Tool"
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # Handle "Launch Interactive Tool" button click
  observeEvent(input$enter_tool, {
    updateTextInput(session, "show_landing", value = "false")
    shinyjs::runjs("window.scrollTo(0, 0);")
  })
  
  # Reactive value to track whether to show ICM+HLR solution
  show_icm_solution <- reactiveVal(FALSE)
  
  # Store user's parameters before showing ICM solution
  saved_user_params <- reactiveVal(NULL)
  
  # Reactive value tracking how many "build the solution" steps are active
  # (0 = none, 1 = domestic expanded, 2 = + international, 3 = + leverage)
  active_step <- reactiveVal(0)
  
  # Get current parameters from inputs (use defaults for removed sliders)
  current_params <- reactive({
    list(
      CF = DEFAULT_PARAMS$CF,
      PUB_2022 = DEFAULT_PARAMS$PUB_2022,
      r = input$r,
      PRIV_2022 = DEFAULT_PARAMS$PRIV_2022,
      g = input$g,
      Lhighpub = input$Lhighpub,
      E_2030 = DEFAULT_PARAMS$E_2030,
      epsilon = DEFAULT_PARAMS$epsilon,
      P_Adv = input$P_Adv,
      C_Adv = input$C_Adv,
      P_EMDEs = input$P_EMDEs,
      C_EMDEs = input$C_EMDEs,
      Lcp_dom = input$Lcp_dom,
      VMS_2030 = input$VMS_2030,
      Lcp_vol = input$Lcp_vol,
      ICMS_2030 = input$ICMS_2030,
      Lcp_int = input$Lcp_int
    )
  })
  
  # Show ICM+HLR button handler
  observeEvent(input$show_icm_btn, {
    # Save current user parameters before switching
    saved_user_params(current_params())
    
    show_icm_solution(TRUE)
    # Update sliders to ICM+HLR values
    updateSliderInput(session, "r", value = ICM_HLR_PARAMS$r)
    updateSliderInput(session, "g", value = ICM_HLR_PARAMS$g)
    updateSliderInput(session, "Lhighpub", value = ICM_HLR_PARAMS$Lhighpub)
    updateSliderInput(session, "ICMS_2030", value = ICM_HLR_PARAMS$ICMS_2030)
    updateSliderInput(session, "Lcp_int", value = ICM_HLR_PARAMS$Lcp_int)
    updateSliderInput(session, "P_Adv", value = ICM_HLR_PARAMS$P_Adv)
    updateSliderInput(session, "C_Adv", value = ICM_HLR_PARAMS$C_Adv)
    updateSliderInput(session, "P_EMDEs", value = ICM_HLR_PARAMS$P_EMDEs)
    updateSliderInput(session, "C_EMDEs", value = ICM_HLR_PARAMS$C_EMDEs)
    updateSliderInput(session, "Lcp_dom", value = ICM_HLR_PARAMS$Lcp_dom)
    updateSliderInput(session, "VMS_2030", value = ICM_HLR_PARAMS$VMS_2030)
    updateSliderInput(session, "Lcp_vol", value = ICM_HLR_PARAMS$Lcp_vol)
    # Disable all parameter sliders
    shinyjs::disable("r")
    shinyjs::disable("g")
    shinyjs::disable("Lhighpub")
    shinyjs::disable("ICMS_2030")
    shinyjs::disable("Lcp_int")
    shinyjs::disable("P_Adv")
    shinyjs::disable("C_Adv")
    shinyjs::disable("P_EMDEs")
    shinyjs::disable("C_EMDEs")
    shinyjs::disable("Lcp_dom")
    shinyjs::disable("VMS_2030")
    shinyjs::disable("Lcp_vol")
    shinyjs::disable("reset_btn")
    active_step(3)
  })
  
  # Hide ICM+HLR button handler
  observeEvent(input$hide_icm_btn, {
    show_icm_solution(FALSE)
    
    # Restore user's saved parameters
    if (!is.null(saved_user_params())) {
      params <- saved_user_params()
      updateSliderInput(session, "r", value = params$r)
      updateSliderInput(session, "g", value = params$g)
      updateSliderInput(session, "Lhighpub", value = params$Lhighpub)
      updateSliderInput(session, "ICMS_2030", value = params$ICMS_2030)
      updateSliderInput(session, "Lcp_int", value = params$Lcp_int)
      updateSliderInput(session, "P_Adv", value = params$P_Adv)
      updateSliderInput(session, "C_Adv", value = params$C_Adv)
      updateSliderInput(session, "P_EMDEs", value = params$P_EMDEs)
      updateSliderInput(session, "C_EMDEs", value = params$C_EMDEs)
      updateSliderInput(session, "Lcp_dom", value = params$Lcp_dom)
      updateSliderInput(session, "VMS_2030", value = params$VMS_2030)
      updateSliderInput(session, "Lcp_vol", value = params$Lcp_vol)
    }
    
    # Re-enable all parameter sliders
    shinyjs::enable("r")
    shinyjs::enable("g")
    shinyjs::enable("Lhighpub")
    shinyjs::enable("ICMS_2030")
    shinyjs::enable("Lcp_int")
    shinyjs::enable("P_Adv")
    shinyjs::enable("C_Adv")
    shinyjs::enable("P_EMDEs")
    shinyjs::enable("C_EMDEs")
    shinyjs::enable("Lcp_dom")
    shinyjs::enable("VMS_2030")
    shinyjs::enable("Lcp_vol")
    shinyjs::enable("reset_btn")
    active_step(0)
  })
  
  # ===== Build the solution step by step (Step 1 -> 2 -> 3) =====
  # Clicking an active step again undoes just that step (cascading, one
  # level at a time); a step only responds once the previous one is active.
  
  # Step 1: Expand Domestic Price
  observeEvent(input$step1_btn, {
    if (active_step() == 0) {
      updateSliderInput(session, "P_Adv", value = ICM_HLR_PARAMS$P_Adv)
      updateSliderInput(session, "C_Adv", value = ICM_HLR_PARAMS$C_Adv)
      updateSliderInput(session, "P_EMDEs", value = ICM_HLR_PARAMS$P_EMDEs)
      updateSliderInput(session, "C_EMDEs", value = ICM_HLR_PARAMS$C_EMDEs)
      active_step(1)
    } else if (active_step() == 1) {
      updateSliderInput(session, "P_Adv", value = DEFAULT_PARAMS$P_Adv)
      updateSliderInput(session, "C_Adv", value = DEFAULT_PARAMS$C_Adv)
      updateSliderInput(session, "P_EMDEs", value = DEFAULT_PARAMS$P_EMDEs)
      updateSliderInput(session, "C_EMDEs", value = DEFAULT_PARAMS$C_EMDEs)
      active_step(0)
    }
  })
  
  # Step 2: Expand International
  observeEvent(input$step2_btn, {
    if (active_step() == 1) {
      updateSliderInput(session, "ICMS_2030", value = ICM_HLR_PARAMS$ICMS_2030)
      active_step(2)
    } else if (active_step() == 2) {
      updateSliderInput(session, "ICMS_2030", value = DEFAULT_PARAMS$ICMS_2030)
      active_step(1)
    }
  })
  
  # Step 3: Raise Leverage Ratio
  observeEvent(input$step3_btn, {
    if (active_step() == 2) {
      updateSliderInput(session, "Lhighpub", value = ICM_HLR_PARAMS$Lhighpub)
      updateSliderInput(session, "Lcp_dom", value = ICM_HLR_PARAMS$Lcp_dom)
      updateSliderInput(session, "Lcp_int", value = ICM_HLR_PARAMS$Lcp_int)
      updateSliderInput(session, "Lcp_vol", value = ICM_HLR_PARAMS$Lcp_vol)
      active_step(3)
    } else if (active_step() == 3) {
      updateSliderInput(session, "Lhighpub", value = DEFAULT_PARAMS$Lhighpub)
      updateSliderInput(session, "Lcp_dom", value = DEFAULT_PARAMS$Lcp_dom)
      updateSliderInput(session, "Lcp_int", value = DEFAULT_PARAMS$Lcp_int)
      updateSliderInput(session, "Lcp_vol", value = DEFAULT_PARAMS$Lcp_vol)
      active_step(2)
    }
  })
  
  # Keep the 3 step buttons' look/enabled-state in sync with active_step,
  # without recreating them (so their click counters are never reset).
  observe({
    as <- active_step()
    locked <- show_icm_solution()
    
    # All 3 steps start in their plain default look. A step only turns
    # green once it has actually been clicked (as >= i) -- becoming
    # clickable is not the same as being active, so it must not change
    # color just from that; when not lit, leave it as the plain default
    # button (no grey override).
    shinyjs::removeClass("step1_btn", "btn-step-idle btn-success")
    shinyjs::addClass("step1_btn", if (as >= 1) "btn-success" else "btn-step-idle")
    
    shinyjs::removeClass("step2_btn", "btn-step-idle btn-success")
    shinyjs::addClass("step2_btn", if (as >= 2) "btn-success" else "btn-step-idle")
    
    shinyjs::removeClass("step3_btn", "btn-step-idle btn-success")
    shinyjs::addClass("step3_btn", if (as >= 3) "btn-success" else "btn-step-idle")
    
    shinyjs::toggleState("step1_btn", !locked && as <= 1)
    shinyjs::toggleState("step2_btn", !locked && as >= 1)
    shinyjs::toggleState("step3_btn", !locked && as >= 2)
  })
  
  output$step_status_text <- renderUI({
    as <- active_step()
    msg <- switch(as + 1,
                  "No steps active yet. Click Step 1 to begin building the solution.",
                  "Step 1 active: domestic carbon pricing expanded. Click Step 2 to continue, or click Step 1 again to undo.",
                  "Steps 1 and 2 active: domestic and international carbon markets expanded. Click Step 3 to continue, or click Step 2 again to undo.",
                  "All 3 steps active: you have closed the climate finance gap! Click Step 3 again to undo."
    )
    p(msg, class = "text-center text-muted mb-2", style = "font-size: 13px;")
  })
  
  # Chart 1: 2022 Baseline (smaller axis fonts for compact display)
  output$chart_2022 <- renderPlotly({
    result <- calculate_2022_baseline(DEFAULT_PARAMS)
    create_waterfall_chart(result, "") %>%
      layout(
        xaxis = list(tickfont = list(size = 9), tickangle = -35),
        yaxis = list(titlefont = list(size = 9), tickfont = list(size = 9)),
        margin = list(b = 100, t = 40, l = 50, r = 10)
      )
  })
  
  # Chart 2: 2030 BAU (smaller axis fonts for compact display)
  output$chart_2030_bau <- renderPlotly({
    result <- calculate_finance(BAU_PARAMS, year = 2030)
    create_waterfall_chart(result, "") %>%
      layout(
        xaxis = list(tickfont = list(size = 9), tickangle = -35),
        yaxis = list(titlefont = list(size = 9), tickfont = list(size = 9)),
        margin = list(b = 100, t = 40, l = 50, r = 10)
      )
  })
  
  # Chart 3: 2030 Custom (Your Scenario - updates with sliders)
  # Shows comparison with ICM+HLR when button is clicked
  #
  # This only fully re-renders when the STRUCTURE changes (switching in/out
  output$chart_2030_custom <- renderPlotly({
    user_result <- calculate_finance(current_params(), year = 2030)
    is_mobile <- isTRUE(input$is_mobile)
    
    if (show_icm_solution()) {
      icm_result <- calculate_finance(ICM_HLR_PARAMS, year = 2030)
      p <- create_comparison_waterfall(user_result, icm_result)
    } else {
      p <- create_waterfall_chart(user_result, "")
    }
    
    # Mobile: smaller axis fonts, wider bars, tighter margins
    if (is_mobile) {
      p <- p %>%
        style(textfont = list(size = 7)) %>%
        layout(
          xaxis = list(
            title = "",
            tickfont = list(size = 3),
            tickangle = -70,
            automargin = TRUE
          ),
          yaxis = list(
            title = "B USD",
            titlefont = list(size = 6),
            tickfont = list(size = 6),
            gridcolor = "#e0e0e0",
            zerolinecolor = "#333",
            range = c(0, 9500),
            dtick = 2000
          ),
          margin = list(b = 55, t = 10, l = 30, r = 5),
          bargap = 0.1
        )
    }
    p
  })
  
  # Gap status message
  output$gap_status <- renderUI({
    result <- calculate_finance(current_params(), year = 2030)
    gap <- result$CFG
    
    if (gap <= 0) {
      div(
        class = "text-center mb-3",
        style = paste0(
          "background: linear-gradient(90deg, #27ae60, #2ecc71);",
          "color: white; padding: 15px; border-radius: 10px;",
          "font-size: 18px; font-weight: 700;",
          "animation: pulse 0.6s ease-in-out 2;"
        ),
        icon("check-circle", class = "fa-lg"),
        " 🎉 Congratulations! You filled the climate finance gap! ",
        sprintf("(Surplus: $%.0f B)", abs(gap))
      )
    } else {
      div(
        class = "text-center mb-3",
        style = paste0(
          "background: linear-gradient(90deg, #f39c12, #f1c40f);",
          "color: white; padding: 15px; border-radius: 10px;",
          "font-size: 18px; font-weight: 700;"
        ),
        icon("exclamation-triangle", class = "fa-lg"),
        sprintf(" Remaining Gap: $%.0f B. Keep adjusting!", gap)
      )
    }
  })
  
  # JavaScript to handle click and display tooltip with gap value
  session$onFlushed(function() {
    shinyjs::runjs("
      $('#remaining_gap_label').click(function(e) {
        e.stopPropagation();

        // Get the gap value from the waterfall chart
        var plotDiv = document.getElementById('chart_2030_custom');
        var gapValue = 0;

        if (plotDiv && plotDiv.data && plotDiv.data.length > 0) {
          // The last trace should be the gap value
          var lastTrace = plotDiv.data[plotDiv.data.length - 1];
          if (lastTrace.y && lastTrace.y.length > 0) {
            gapValue = lastTrace.y[lastTrace.y.length - 1];
          }
        }

        // Determine color and message based on gap value
        var color = gapValue >= 0 ? '#27ae60' : '#f39c12';
        var title = gapValue >= 0 ? '🎉 Congrats!' : 'Keep adjusting!';
        var message = gapValue >= 0 ?
          'You filled the climate finance gap! Surplus: $' + Math.round(gapValue) + ' B' :
          'Remaining gap: $' + Math.abs(Math.round(gapValue)) + ' B';
        var number = gapValue >= 0 ? '+' + Math.round(gapValue) : '-' + Math.abs(Math.round(gapValue));

        // Build and display tooltip
        var html = '<div style=\"font-size: 80px; font-weight: 700; color: ' + color + '; margin-bottom: 5px; font-family: Segoe UI, Trebuchet MS, sans-serif; letter-spacing: 0px; line-height: 1;\">' +
                   number +
                   '</div>' +
                   '<div style=\"font-size: 14px; color: #666; margin-bottom: 15px;\">Billion USD</div>' +
                   '<hr style=\"border: none; border-top: 2px solid #555; margin: 15px 0;\">' +
                   '<div style=\"font-size: 16px; font-weight: 600; color: ' + color + '; margin-top: 15px;\">' +
                   title +
                   '</div>' +
                   '<div style=\"font-size: 14px; color: #333; margin-top: 8px;\">' +
                   message +
                   '</div>';

        $('#gap_tooltip_inner').html(html);
        $('#gap_tooltip').fadeToggle(200);
      });

      // Close tooltip when clicking outside
      $(document).click(function(e) {
        if (!$(e.target).closest('#remaining_gap_label').length &&
            !$(e.target).closest('#gap_tooltip').length) {
          $('#gap_tooltip').fadeOut(200);
        }
      });

      // Prevent tooltip from closing when clicking inside it
      $('#gap_tooltip').click(function(e) {
        e.stopPropagation();
      });
    ")
  }, once = TRUE)
  
  # Reset button handler
  observeEvent(input$reset_btn, {
    # Public + Private Finance
    updateSliderInput(session, "r", value = DEFAULT_PARAMS$r)
    updateSliderInput(session, "g", value = DEFAULT_PARAMS$g)
    updateSliderInput(session, "Lhighpub", value = DEFAULT_PARAMS$Lhighpub)
    # Domestic Carbon Market
    updateSliderInput(session, "P_Adv", value = DEFAULT_PARAMS$P_Adv)
    updateSliderInput(session, "C_Adv", value = DEFAULT_PARAMS$C_Adv)
    updateSliderInput(session, "P_EMDEs", value = DEFAULT_PARAMS$P_EMDEs)
    updateSliderInput(session, "C_EMDEs", value = DEFAULT_PARAMS$C_EMDEs)
    updateSliderInput(session, "Lcp_dom", value = DEFAULT_PARAMS$Lcp_dom)
    # International Carbon Market
    updateSliderInput(session, "ICMS_2030", value = DEFAULT_PARAMS$ICMS_2030)
    updateSliderInput(session, "Lcp_int", value = DEFAULT_PARAMS$Lcp_int)
    # Voluntary Carbon Market
    updateSliderInput(session, "VMS_2030", value = DEFAULT_PARAMS$VMS_2030)
    updateSliderInput(session, "Lcp_vol", value = DEFAULT_PARAMS$Lcp_vol)
    active_step(0)
  })
  
  # Download parameters as CSV
  output$download_params <- downloadHandler(
    filename = function() {
      paste0("climate_finance_scenario_", Sys.Date(), ".csv")
    },
    content = function(file) {
      params <- current_params()
      df <- data.frame(
        Parameter = names(params),
        Value = unlist(params)
      )
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Download chart as HTML
  output$download_chart <- downloadHandler(
    filename = function() {
      paste0("climate_finance_chart_", Sys.Date(), ".html")
    },
    content = function(file) {
      user_result <- calculate_finance(current_params(), year = 2030)
      
      if (show_icm_solution()) {
        icm_result <- calculate_finance(ICM_HLR_PARAMS, year = 2030)
        chart <- create_comparison_waterfall(user_result, icm_result)
      } else {
        chart <- create_waterfall_chart(user_result, "")
      }
      
      # Export as interactive HTML (without selfcontained to avoid pandoc requirement)
      htmlwidgets::saveWidget(
        as_widget(chart),
        file = file,
        selfcontained = FALSE
      )
    }
  )
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================

shinyApp(ui = ui, server = server)
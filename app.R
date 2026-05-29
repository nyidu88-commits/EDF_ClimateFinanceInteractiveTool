# ==============================================================================
# Ghana Climate Finance Gap Explorer
# Mitigation-only focus | Interactive Scenario Builder
# ==============================================================================
library(shiny)
library(shinyjs)
library(bslib)
library(plotly)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

data_raw <- read_excel("Ghana_Finance_By_Year_Cleaned.xlsx") %>%
  mutate(
    Year = as.numeric(Year),
    PUB = as.numeric(PUB_2022) / 1e9,
    PRIV = as.numeric(PRIV_2022) / 1e9,
    Mitigation = as.numeric(Mitigation) / 1e9,
    Adaptation = as.numeric(Adaptation) / 1e9,
    Mit_Share = ifelse(is.na(Mitigation_Share) | is.nan(Mitigation_Share), 0, Mitigation_Share)
  ) %>%
  mutate(PUBmit = Mitigation, PRIVmit = PRIV * Mit_Share) %>%
  arrange(Year)

BAU_PARAMS <- list(
  CF_cumulative = 12.4, pub_growth = 14.37, priv_growth = 9.54,
  lev_energy = 0.4, lev_nature = 0.1, share_energy = 0.50,
  carbon_price = 10, coverage = 0.30, carbon_leverage = 5,
  article6 = 0.8, extra_credits = 0, emissions_mt = 36.5, elasticity = 0.0028
)

project_growth <- function(start_val, rate_pct, n_years) {
  if (n_years <= 0) return(numeric(0))
  rate <- rate_pct / 100
  vals <- numeric(n_years)
  vals[1] <- start_val * (1 + rate)
  if (n_years > 1) for (i in 2:n_years) vals[i] <- vals[i - 1] * (1 + rate)
  vals
}

get_combined_data <- function(pub_growth, priv_growth, end_year = 2035) {
  last_year <- max(data_raw$Year, na.rm = TRUE)
  future_years <- if (last_year < end_year) (last_year + 1):end_year else integer(0)
  n_future <- length(future_years)
  last_pub <- tail(data_raw$PUBmit, 1)
  last_priv <- tail(data_raw$PRIVmit, 1)
  pub_proj <- if (n_future > 0) project_growth(last_pub, pub_growth, n_future) else numeric(0)
  priv_proj <- if (n_future > 0) project_growth(last_priv, priv_growth, n_future) else numeric(0)
  df_proj <- if (n_future > 0) {
    data.frame(Year = future_years, PUBmit = pub_proj, PRIVmit = priv_proj)
  } else {
    data.frame(Year = integer(0), PUBmit = numeric(0), PRIVmit = numeric(0))
  }
  bind_rows(data_raw %>% select(Year, PUBmit, PRIVmit), df_proj) %>%
    filter(Year <= end_year) %>% arrange(Year)
}

calculate_gap <- function(df, year, params) {
  year <- as.numeric(year)
  df_use <- df %>% filter(Year == year)
  pub_val <- sum(df_use$PUBmit)
  priv_val <- sum(df_use$PRIVmit)
  pub_leverage <- pub_val * (
    params$share_energy * params$lev_energy +
    (1 - params$share_energy) * params$lev_nature
  )
  Et <- params$emissions_mt / 1000
  carbon_value <- Et * params$elasticity * (params$carbon_price^2) * params$coverage
  carbon_mob <- carbon_value * params$carbon_leverage
  art6 <- ifelse(year >= 2025, params$article6, 0)
  extra <- params$extra_credits
  annual_need <- params$CF_cumulative / 6
  total_supply <- pub_val + priv_val + pub_leverage + carbon_mob + art6 + extra
  gap <- annual_need - total_supply
  list(CF = annual_need, PUB = pub_val, PRIV = priv_val, PRIVpub = pub_leverage,
       Carbon = carbon_mob, Article6 = art6, Extra = extra, CFG = gap,
       total_supply = total_supply)
}

WF_COLORS <- list(
  need = "#e74c3c", pub = "#2e86c1", priv = "#9b59b6",
  leverage = "#f1c40f", carbon = "#27ae60", article6 = "#00838f",
  extra = "#e67e22", gap_pos = "#e74c3c", gap_neg = "#1abc9c"
)

create_waterfall_chart <- function(result, title = "") {
  values <- c(result$CF, -result$PUB, -result$PRIV, -result$PRIVpub,
              -result$Carbon, -result$Article6, -result$Extra, result$CFG)
  labels <- c("Climate Finance\nNeed", "Public Finance\n(Mitigation)",
    "Private Finance\n(Mitigation)", "Public\nLeverage",
    "Carbon Market\nMobilization", "Article 6\nFlow",
    "Additional\nCredits", "Remaining\nGap")
  n <- length(values)
  starts <- numeric(n)
  starts[1] <- 0
  for (i in 2:n) {
    if (i == n) { starts[i] <- 0 } else { starts[i] <- starts[i - 1] + values[i - 1] }
  }
  ends <- starts + values
  colors <- c(WF_COLORS$need, WF_COLORS$pub, WF_COLORS$priv,
              WF_COLORS$leverage, WF_COLORS$carbon, WF_COLORS$article6,
              WF_COLORS$extra,
              ifelse(result$CFG > 0, WF_COLORS$gap_pos, WF_COLORS$gap_neg))
  hover_text <- sprintf("<b>%s</b><br>$%.3f B", gsub("\n", " ", labels), abs(values))
  plot_ly() %>%
    add_trace(type = "bar", x = labels, y = abs(values), base = pmin(starts, ends),
      marker = list(color = colors, line = list(color = "#fff", width = 1)),
      text = sprintf("$%.3fB", abs(values)), textposition = "outside",
      textfont = list(size = 11, color = "#333"),
      hovertext = hover_text, hoverinfo = "text") %>%
    layout(
      title = list(text = title, font = list(size = 16, color = "#1a5f4a")),
      xaxis = list(title = "", tickfont = list(size = 10), categoryorder = "array",
                   categoryarray = labels),
      yaxis = list(title = "Billion USD", gridcolor = "#e0e0e0",
                   zerolinecolor = "#333", tickfont = list(size = 12)),
      margin = list(b = 100, t = 60, l = 60, r = 20),
      bargap = 0.15, plot_bgcolor = "#fafafa", paper_bgcolor = "#fafafa",
      showlegend = FALSE
    ) %>%
    config(displayModeBar = TRUE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Quicksand:wght@400;600;700&display=swap');
  body { font-family: 'Roboto', sans-serif; }
  .landing-page { min-height: 100vh; background: linear-gradient(135deg, #f0faf5 0%, #e8f5e9 100%); }
  .navbar-top {
    display: flex; justify-content: space-between; align-items: center;
    padding: 15px 40px; background: #1e5631; color: white;
  }
  .navbar-top .logo { font-size: 1.3rem; font-weight: 700; }
  .navbar-top .instruction-btn {
    padding: 8px 20px; background: #27ae60; color: white;
    border-radius: 6px; text-decoration: none; font-weight: 500;
  }
  .hero {
    display: flex; align-items: center; justify-content: center;
    padding: 60px 40px; max-width: 1200px; margin: 0 auto; gap: 40px;
  }
  .hero-text { flex: 1; }
  .hero-text h1 { font-size: 2.5rem; color: #1e5631; font-weight: 700; font-family: 'Quicksand'; }
  .hero-text .subtitle { color: #333; font-size: 1.1rem; line-height: 1.6; margin-top: 15px; }
  .cta-btn {
    display: inline-block; margin-top: 25px; padding: 12px 30px;
    background: #27ae60; color: white; border-radius: 8px;
    text-decoration: none; font-weight: 600; font-size: 1.1rem;
  }
  .cta-btn:hover { background: #219a52; color: white; }
  .hero-image img { max-width: 500px; border-radius: 12px; box-shadow: 0 10px 40px rgba(0,0,0,0.15); }
  .features { background: white; padding: 60px 40px; }
  .features-container { max-width: 1000px; margin: 0 auto; text-align: center; }
  .features-container h2 { color: #1e5631; font-family: 'Quicksand'; font-size: 2rem; }
  .features-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 30px; margin-top: 30px; }
  .feature-item { padding: 25px; border-radius: 12px; background: #f0faf5; border: 1px solid #d4efdf; text-align: center; }
  .feature-icon { font-size: 2rem; color: #27ae60; margin-bottom: 10px; }
  .feature-item h3 { color: #1e5631; font-size: 1.1rem; }
  .feature-item p { color: #555; font-size: 0.9rem; }
  .footer { background: #1e5631; color: white; padding: 30px; text-align: center; }
  .footer a { color: #27ae60; text-decoration: none; }
"

app_theme <- bs_theme(version = 5, bootswatch = "flatly",
  bg = "#f0f8f5", fg = "#2c3e50", primary = "#27ae60", secondary = "#1e5631")

labelWithTooltip <- function(label, definition, what_it_does) {
  tooltip_content <- paste0(
    "<div style=\"text-align:left;\">",
    "<strong style=\"color:#f39c12;\">Definition:</strong><br>",
    definition, "<br><br>",
    "<strong style=\"color:#27ae60;\">What it does:</strong><br>",
    what_it_does, "</div>")
  tags$span(label,
    tags$i(class = "fas fa-info-circle",
           style = "margin-left: 5px; color: #3498db; cursor: pointer;",
           `data-bs-toggle` = "tooltip", `data-bs-placement` = "right",
           `data-bs-html` = "true", title = tooltip_content))
}
ui <- page_fluid(
  shinyjs::useShinyjs(), theme = app_theme,
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;600;700&display=swap", rel = "stylesheet"),
    tags$style(HTML(custom_css)),
    tags$script(HTML("$(document).ready(function(){$('body').tooltip({selector:'[data-bs-toggle=\"tooltip\"]',html:true,trigger:'hover',container:'body'});});"))
  ),
    div(class = "container-fluid py-4", style = "background: #f0f8f5; border-top: 5px solid #1e5631;",
      fluidRow(column(12,
        div(class = "text-center mb-3",
          style = "background: linear-gradient(135deg, #1e5631, #27ae60); padding: 15px; border-radius: 10px;",
          h2(icon("globe-africa"), " Ghana Climate Finance Gap", style = "color: #fff; font-weight: 700; margin: 0;"),
          p(icon("sliders-h"), " Build Your Scenario", style = "color: #d4efdf; margin: 5px 0 0 0; font-size: 16px;")),
        div(id = "scenario-interactive", fluidRow(
          column(6, card(
            card_header(style = "background: #1e5631; color: white;", icon("cogs"), " Scenario Parameters"),
            card_body(fluidRow(
              column(6,
                div(style = "background: #e8f5e9; padding: 10px; border-radius: 8px; margin-bottom: 10px;",
                  tags$h6(icon("university"), " Public Finance", style = "color: #1e5631; margin-bottom: 10px;"),
                  sliderInput("pub_growth",
                    label = labelWithTooltip("Public Growth (%)", "Annual growth rate of public mitigation finance.", "Controls how fast public climate finance grows."),
                    min = 0, max = 30, value = BAU_PARAMS$pub_growth, step = 0.5),
                  sliderInput("lev_energy",
                    label = labelWithTooltip("Leverage - Energy", "Private per dollar of public energy investment.", "Co-financing effect of public energy investments."),
                    min = 0, max = 2, value = BAU_PARAMS$lev_energy, step = 0.05),
                  sliderInput("lev_nature",
                    label = labelWithTooltip("Leverage - Nature", "Private per dollar of public nature investment.", "Nature-based solutions have lower leverage."),
                    min = 0, max = 2, value = BAU_PARAMS$lev_nature, step = 0.05),
                  sliderInput("share_energy",
                    label = labelWithTooltip("Energy Share", "Fraction of public finance to energy vs nature.", "Determines weighted average leverage ratio."),
                    min = 0, max = 1, value = BAU_PARAMS$share_energy, step = 0.05)),
                div(style = "background: #e0f7fa; padding: 10px; border-radius: 8px;",
                  tags$h6(icon("leaf"), " Private Finance", style = "color: #00838f; margin-bottom: 10px;"),
                  sliderInput("priv_growth",
                    label = labelWithTooltip("Private Growth (%)", "Annual growth rate of private mitigation finance.", "Controls baseline growth of private investment."),
                    min = 0, max = 30, value = BAU_PARAMS$priv_growth, step = 0.5))),
              column(6,
                div(style = "background: #e8f5e9; padding: 10px; border-radius: 8px; margin-bottom: 10px;",
                  tags$h6(icon("industry"), " Carbon Pricing", style = "color: #1e5631; margin-bottom: 10px;"),
                  sliderInput("carbon_price",
                    label = labelWithTooltip("Carbon Price ($/tCO2)", "Price per ton of CO2.", "Higher prices generate more carbon revenue."),
                    min = 0, max = 100, value = BAU_PARAMS$carbon_price, step = 1),
                  sliderInput("coverage",
                    label = labelWithTooltip("Market Coverage", "Share of emissions covered.", "Scope of carbon pricing mechanism."),
                    min = 0, max = 1, value = BAU_PARAMS$coverage, step = 0.05),
                  sliderInput("carbon_leverage",
                    label = labelWithTooltip("Carbon Leverage (x)", "Private capital per dollar carbon revenue.", "Carbon market multiplier effect."),
                    min = 0, max = 15, value = BAU_PARAMS$carbon_leverage, step = 0.5)),
                div(style = "background: #fff3e0; padding: 10px; border-radius: 8px; margin-bottom: 10px;",
                  tags$h6(icon("handshake"), " Article 6 & Credits", style = "color: #e67e22; margin-bottom: 10px;"),
                  sliderInput("article6",
                    label = labelWithTooltip("Article 6 (B USD/yr)", "Ghana-Switzerland Article 6 flow.", "Climate finance from 2025 onward."),
                    min = 0, max = 5, value = BAU_PARAMS$article6, step = 0.1),
                  numericInput("extra_credits", label = "Additional Credits (B USD)", value = BAU_PARAMS$extra_credits, min = 0, step = 0.01)),
                div(style = "background: #fce4ec; padding: 10px; border-radius: 8px;",
                  tags$h6(icon("bullseye"), " Finance Need", style = "color: #c62828; margin-bottom: 10px;"),
                  sliderInput("CF_cumulative",
                    label = labelWithTooltip("2025-2030 Need (B USD)", "Ghana total mitigation finance need.", "Range $9.3B-$15.5B. Default midpoint $12.4B."),
                    min = 9.3, max = 15.5, value = BAU_PARAMS$CF_cumulative, step = 0.1),
                  selectInput("scenario_year", "Scenario Year:", choices = 2025:2035, selected = 2030)),
                hr(),
                actionButton("reset_btn", label = "Reset All", icon = icon("redo"), class = "btn btn-outline-danger w-100")))))),
          column(6, class = "chart-col", card(
            card_header(style = "background: #272b30; color: white;", icon("chart-area"), " Your Scenario"),
            card_body(div(style = "position: relative;",
              plotlyOutput("waterfall_chart", height = "600px")))))
  )))))
)
server <- function(input, output, session) {
  current_params <- reactive({
    list(CF_cumulative = input$CF_cumulative, pub_growth = input$pub_growth,
         priv_growth = input$priv_growth, lev_energy = input$lev_energy,
         lev_nature = input$lev_nature, share_energy = input$share_energy,
         carbon_price = input$carbon_price, coverage = input$coverage,
         carbon_leverage = input$carbon_leverage, article6 = input$article6,
         extra_credits = input$extra_credits,
         emissions_mt = BAU_PARAMS$emissions_mt, elasticity = BAU_PARAMS$elasticity)
  })
  output$waterfall_chart <- renderPlotly({
    params <- current_params()
    df <- get_combined_data(params$pub_growth, params$priv_growth)
    result <- calculate_gap(df, input$scenario_year, params)
    create_waterfall_chart(result, paste("Ghana Climate Finance Gap -", input$scenario_year))
  })
  observeEvent(input$reset_btn, {
    updateSliderInput(session, "pub_growth", value = BAU_PARAMS$pub_growth)
    updateSliderInput(session, "priv_growth", value = BAU_PARAMS$priv_growth)
    updateSliderInput(session, "lev_energy", value = BAU_PARAMS$lev_energy)
    updateSliderInput(session, "lev_nature", value = BAU_PARAMS$lev_nature)
    updateSliderInput(session, "share_energy", value = BAU_PARAMS$share_energy)
    updateSliderInput(session, "carbon_price", value = BAU_PARAMS$carbon_price)
    updateSliderInput(session, "coverage", value = BAU_PARAMS$coverage)
    updateSliderInput(session, "carbon_leverage", value = BAU_PARAMS$carbon_leverage)
    updateSliderInput(session, "article6", value = BAU_PARAMS$article6)
    updateNumericInput(session, "extra_credits", value = BAU_PARAMS$extra_credits)
    updateSliderInput(session, "CF_cumulative", value = BAU_PARAMS$CF_cumulative)
    updateSelectInput(session, "scenario_year", selected = 2030)
  })
}

shinyApp(ui = ui, server = server)

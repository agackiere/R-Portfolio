#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# app.R
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

df <- readRDS("spotify_data.rds")

all_artists <- sort(unique(df$artist))
date_min    <- min(df$date)
date_max    <- max(df$date)

# ── Palette ───────────────────────────────────────────────────────────────────
SPOTIFY_GREEN <- "#1DB954"
BG            <- "#121212"
SURFACE       <- "#181818"
SURFACE2      <- "#282828"
TEXT          <- "#FFFFFF"
SUBTEXT       <- "#B3B3B3"


spotify_theme <- function(p) {
  p %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color = TEXT, family = "circular, helvetica, sans-serif"),
      xaxis         = list(gridcolor = SURFACE2, zerolinecolor = SURFACE2, tickcolor = SUBTEXT, color = SUBTEXT),
      yaxis         = list(gridcolor = SURFACE2, zerolinecolor = SURFACE2, tickcolor = SUBTEXT, color = SUBTEXT),
      margin        = list(t = 40, b = 40, l = 10, r = 10),
      hoverlabel    = list(bgcolor = SURFACE2, bordercolor = SPOTIFY_GREEN, font = list(color = TEXT))
    )
}

# UI
ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;800&display=swap",
      rel  = "stylesheet"
    ),
    tags$style(HTML(paste0("
      * { box-sizing: border-box; margin: 0; padding: 0; }

      body, .shiny-app-container {
        background-color: ", BG, ";
        color: ", TEXT, ";
        font-family: 'Montserrat', sans-serif;
        min-height: 100vh;
      }

      /* ── Header ── */
      .app-header {
        background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
        padding: 36px 40px 28px;
        border-bottom: 1px solid ", SURFACE2, ";
        display: flex;
        align-items: center;
        gap: 16px;
      }
      .app-header .logo {
        font-size: 28px;
        font-weight: 800;
        letter-spacing: -0.5px;
      }
      .app-header .logo span { color: ", SPOTIFY_GREEN, "; }
      .app-header .subtitle {
        font-size: 13px;
        color: ", SUBTEXT, ";
        margin-top: 4px;
        font-weight: 400;
        letter-spacing: 0.5px;
      }

      /* ── Stat cards ── */
      .stats-row {
        display: flex;
        gap: 16px;
        padding: 24px 40px 0;
        flex-wrap: wrap;
      }
      .stat-card {
        background: ", SURFACE, ";
        border-radius: 12px;
        padding: 20px 28px;
        flex: 1;
        min-width: 160px;
        border: 1px solid ", SURFACE2, ";
        transition: border-color 0.2s;
      }
      .stat-card:hover { border-color: ", SPOTIFY_GREEN, "; }
      .stat-card .stat-value {
        font-size: 28px;
        font-weight: 800;
        color: ", SPOTIFY_GREEN, ";
        line-height: 1;
      }
      .stat-card .stat-label {
        font-size: 12px;
        color: ", SUBTEXT, ";
        margin-top: 6px;
        text-transform: uppercase;
        letter-spacing: 1px;
        font-weight: 600;
      }

      /* ── Filters ── */
      .filters-row {
        display: flex;
        gap: 16px;
        padding: 24px 40px;
        flex-wrap: wrap;
        align-items: flex-end;
      }
      .filter-card {
        background: ", SURFACE, ";
        border-radius: 12px;
        padding: 16px 20px;
        border: 1px solid ", SURFACE2, ";
        flex: 1;
        min-width: 240px;
      }
      .filter-card label {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 1px;
        color: ", SUBTEXT, ";
        font-weight: 600;
        display: block;
        margin-bottom: 10px;
      }

      /* Override Shiny inputs */
      .selectize-control .selectize-input,
      .selectize-dropdown {
        background: ", SURFACE2, " !important;
        border-color: #3e3e3e !important;
        color: ", TEXT, " !important;
        border-radius: 8px !important;
        font-family: 'Montserrat', sans-serif !important;
        font-size: 13px !important;
      }
      .selectize-dropdown-content .option:hover,
      .selectize-dropdown-content .option.active {
        background: #333 !important;
        color: ", SPOTIFY_GREEN, " !important;
      }
      .irs--shiny .irs-bar { background: ", SPOTIFY_GREEN, " !important; border-color: ", SPOTIFY_GREEN, " !important; }
      .irs--shiny .irs-handle { background: ", SPOTIFY_GREEN, " !important; border-color: ", SPOTIFY_GREEN, " !important; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: ", SPOTIFY_GREEN, " !important;
        font-family: 'Montserrat', sans-serif !important;
        font-size: 11px !important;
      }
      .irs--shiny .irs-line { background: ", SURFACE2, " !important; border-color: ", SURFACE2, " !important; }
      .irs--shiny .irs-grid-text { color: ", SUBTEXT, " !important; font-size: 10px !important; }

      /* ── Plot grid ── */
      .plots-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 20px;
        padding: 0 40px 40px;
      }
      .plot-card {
        background: ", SURFACE, ";
        border-radius: 16px;
        padding: 24px;
        border: 1px solid ", SURFACE2, ";
        transition: border-color 0.2s;
      }
      .plot-card:hover { border-color: #3e3e3e; }
      .plot-card.full-width { grid-column: 1 / -1; }
      .plot-title {
        font-size: 14px;
        font-weight: 600;
        color: ", TEXT, ";
        margin-bottom: 4px;
        letter-spacing: 0.2px;
      }
      .plot-subtitle {
        font-size: 12px;
        color: ", SUBTEXT, ";
        margin-bottom: 16px;
      }

      /* Responsive */
      @media (max-width: 900px) {
        .plots-grid { grid-template-columns: 1fr; }
        .app-header, .stats-row, .filters-row, .plots-grid { padding-left: 20px; padding-right: 20px; }
      }
    ")))
  ),

  # Header
  div(class = "app-header",
      div(
        div(class = "logo", tags$span("♫ "), "Your Spotify", tags$span(" Wrapped")),
        div(class = "subtitle", "Personal listening dashboard · Past 12 months")
      )
  ),

  # Stat cards (reactive)
  div(class = "stats-row",
      div(class = "stat-card", div(class = "stat-value", textOutput("stat_plays")),   div(class = "stat-label", "Total Plays")),
      div(class = "stat-card", div(class = "stat-value", textOutput("stat_hours")),   div(class = "stat-label", "Hours Listened")),
      div(class = "stat-card", div(class = "stat-value", textOutput("stat_tracks")),  div(class = "stat-label", "Unique Tracks")),
      div(class = "stat-card", div(class = "stat-value", textOutput("stat_artists")), div(class = "stat-label", "Unique Artists"))
  ),

  # Filters
  div(class = "filters-row",
      div(class = "filter-card",
          tags$label("Date Range"),
          sliderInput("date_range", label = NULL,
                      min   = date_min, max = date_max,
                      value = c(date_min, date_max),
                      timeFormat = "%b %Y", step = 30,
                      width = "100%"
          )
      ),
      div(class = "filter-card",
          tags$label("Filter by Artist"),
          selectizeInput("artist_filter", label = NULL,
                         choices  = c("All Artists" = "", all_artists),
                         selected = "",
                         options  = list(placeholder = "Search for an artist...", maxItems = 1),
                         width    = "100%"
          )
      )
  ),

  # Plots
  div(class = "plots-grid",
      div(class = "plot-card",
          div(class = "plot-title", "Top 15 Artists"),
          div(class = "plot-subtitle", "By number of plays"),
          plotlyOutput("plot_artists", height = "380px")
      ),
      div(class = "plot-card",
          div(class = "plot-title", "Top 15 Tracks"),
          div(class = "plot-subtitle", "By number of plays"),
          plotlyOutput("plot_tracks", height = "380px")
      ),
      div(class = "plot-card",
          div(class = "plot-title", "Listening by Hour"),
          div(class = "plot-subtitle", "When do you listen most?"),
          plotlyOutput("plot_hour", height = "300px")
      ),
      div(class = "plot-card",
          div(class = "plot-title", "Listening by Day"),
          div(class = "plot-subtitle", "Which days are most active?"),
          plotlyOutput("plot_day", height = "300px")
      ),
      div(class = "plot-card full-width",
          div(class = "plot-title", "Monthly Activity"),
          div(class = "plot-subtitle", "Hours listened per month"),
          plotlyOutput("plot_monthly", height = "280px")
      ),
      div(class = "plot-card full-width",
          div(class = "plot-title", "Artist Diversity"),
          div(class = "plot-subtitle", "Unique artists discovered each month"),
          plotlyOutput("plot_diversity", height = "280px")
      )
  )
)

# Server
server <- function(input, output, session) {

  filtered <- reactive({
    d <- df %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    if (!is.null(input$artist_filter) && input$artist_filter != "") {
      d <- d %>% filter(artist == input$artist_filter)
    }
    d
  })

  # Stat cards
  output$stat_plays   <- renderText(scales::comma(nrow(filtered())))
  output$stat_hours   <- renderText(paste0(round(sum(filtered()$minutes_played) / 60, 0), "h"))
  output$stat_tracks  <- renderText(scales::comma(n_distinct(filtered()$track)))
  output$stat_artists <- renderText(scales::comma(n_distinct(filtered()$artist)))

  # Top artists
  output$plot_artists <- renderPlotly({
    d <- filtered() %>%
      count(artist, sort = TRUE) %>%
      slice_head(n = 15) %>%
      mutate(artist = fct_reorder(artist, n))

    plot_ly(d, x = ~n, y = ~artist, type = "bar", orientation = "h",
            marker = list(color = SPOTIFY_GREEN, line = list(width = 0)),
            hovertemplate = "<b>%{y}</b><br>%{x} plays<extra></extra>"
    ) %>%
      spotify_theme() %>%
      layout(yaxis = list(tickfont = list(size = 11)))
  })

  # Top tracks
  output$plot_tracks <- renderPlotly({
    d <- filtered() %>%
      count(track, artist, sort = TRUE) %>%
      slice_head(n = 15) %>%
      mutate(
        label = paste0(track, " — ", artist),
        label = fct_reorder(label, n)
      )

    plot_ly(d, x = ~n, y = ~label, type = "bar", orientation = "h",
            marker = list(color = SPOTIFY_GREEN, line = list(width = 0)),
            hovertemplate = "<b>%{y}</b><br>%{x} plays<extra></extra>"
    ) %>%
      spotify_theme() %>%
      layout(yaxis = list(tickfont = list(size = 10)))
  })

  # Hour
  output$plot_hour <- renderPlotly({
    d <- filtered() %>% count(hour)

    plot_ly(d, x = ~hour, y = ~n, type = "bar",
            marker = list(color = SPOTIFY_GREEN, line = list(width = 0)),
            hovertemplate = "%{x}:00 — <b>%{y} plays</b><extra></extra>"
    ) %>%
      spotify_theme() %>%
      layout(xaxis = list(tickvals = seq(0, 23, 3), ticktext = paste0(seq(0, 23, 3), ":00")))
  })

  # Day of week
  output$plot_day <- renderPlotly({
    d <- filtered() %>% count(day_of_week)

    plot_ly(d, x = ~day_of_week, y = ~n, type = "bar",
            marker = list(color = SPOTIFY_GREEN, line = list(width = 0)),
            hovertemplate = "<b>%{x}</b><br>%{y} plays<extra></extra>"
    ) %>%
      spotify_theme()
  })

  # Monthly hours
  output$plot_monthly <- renderPlotly({
    d <- filtered() %>%
      group_by(month) %>%
      summarise(hours = round(sum(minutes_played) / 60, 1), .groups = "drop")

    plot_ly(d, x = ~month, y = ~hours, type = "bar",
            marker = list(color = SPOTIFY_GREEN, line = list(width = 0)),
            hovertemplate = "<b>%{x|%b %Y}</b><br>%{y} hours<extra></extra>"
    ) %>%
      spotify_theme()
  })

  # Artist diversity
  output$plot_diversity <- renderPlotly({
    d <- filtered() %>%
      group_by(month) %>%
      summarise(unique_artists = n_distinct(artist), .groups = "drop")

    plot_ly(d, x = ~month, y = ~unique_artists, type = "scatter", mode = "lines+markers",
            line   = list(color = SPOTIFY_GREEN, width = 2.5),
            marker = list(color = SPOTIFY_GREEN, size = 6),
            hovertemplate = "<b>%{x|%b %Y}</b><br>%{y} artists<extra></extra>"
    ) %>%
      spotify_theme()
  })
}

shinyApp(ui, server)

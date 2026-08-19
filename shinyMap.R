# shinyMap.R -- interactive companion to analysis_4.qmd
#
# Two metrics, chosen from the six in data_wide.csv, drawn as a pair of leaflet
# maps plus a country-level scatter. One row per education level: Master's on
# top, Doctorate below, so a column reads as "same view, other degree".
#
# Launch from the project root:  shiny::runApp("shinyMap.R")
# It is deliberately outside every .qmd so `quarto render` never evaluates it.

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(sf)
library(leaflet)
library(plotly)


# ---- data ------------------------------------------------------------------

# data_wide.csv is written by analysis_4.qmd *before* the Croatia currency fix,
# so the correction has to be repeated here or HRV earnings come out 7.5x high.
# Same constant, same reasoning as the qmd: kuna -> euro at the irrevocable rate.
HRK_PER_EUR <- 7.53450

education_levels <- c("Total population", "Master's", "Doctorate")
age_levels <- c("All ages", "25-34", "35-44", "45-54", "55-64")

data_wide <- read_csv("data_wide.csv", show_col_types = FALSE) |>
  mutate(
    earnings_gross_yearly = if_else(
      iso3 == "HRV",
      earnings_gross_yearly / HRK_PER_EUR,
      earnings_gross_yearly
    ),
    education = factor(education, levels = education_levels),
    age_group = factor(age_group, levels = age_levels)
  )

stopifnot(!anyNA(data_wide$education), !anyNA(data_wide$age_group))

# Same tribble as the qmd, so a metric looks the same in both places.
metrics <- tibble::tribble(
  ~col                     , ~label                    , ~palette ,
  "pct_happy"              , "Happy (%)"               , "YlGnBu" ,
  "pct_satisfied_finances" , "Satisfied, finances (%)" , "YlGnBu" ,
  "pct_satisfied_leisure"  , "Satisfied, leisure (%)"  , "YlGnBu" ,
  "pct_permanent_contract" , "Permanent contract (%)"  , "YlGnBu" ,
  "earnings_gross_yearly"  , "Gross yearly earnings"   , "YlOrBr" ,
  "hours_worked"           , "Hours worked per year"   , "YlOrBr"
)
stopifnot(all(metrics$col %in% names(data_wide)))

metric_label <- setNames(metrics$label, metrics$col)
metric_palette <- setNames(metrics$palette, metrics$col)

# The two rows of the app. Named so output ids stay short: map_mas_x, map_phd_y.
edu_rows <- c(mas = "Master's", phd = "Doctorate")

# Boundaries come from a file, not from giscoR at runtime. Two reasons: the
# WebAssembly build has no way to reach the GISCO service (browser CORS, and
# webR has no raw sockets), and a fixed file makes the app's geometry
# reproducible. analysis_4.qmd writes it from the same gisco_get_countries()
# call the static maps use -- see the `save-borders` chunk there.
stopifnot(file.exists("borders.geojson"))
borders <- sf::st_read("borders.geojson", quiet = TRUE)

# The join below is a left join onto `borders`, so a country present in the data
# but missing from the file would vanish silently.
stopifnot(all(unique(data_wide$iso3) %in% borders$ISO3_CODE))

# France, Spain, Portugal, Norway carry overseas/Arctic territory in GISCO,
# so never trust the automatic bounds -- set them explicitly.
eu_bbox <- c(xmin = -12, ymin = 34, xmax = 32, ymax = 71)

# Coverage: the wellbeing measures are reported only at "All ages", the labour
# measures only per age band. A metric pair that is legal at one band is empty
# at another, so the UI has to say which pairs actually exist.
coverage <- data_wide |>
  filter(education %in% edu_rows) |>
  summarise(across(all_of(metrics$col), \(x) any(!is.na(x))), .by = age_group)

has_data <- function(col, age) {
  coverage[[col]][match(age, as.character(coverage$age_group))]
}

# One row per country, one geometry, one education x age slice -- the join the
# maps need. `country` comes along so map labels and scatter labels agree.
edu_slice <- function(edu, age) {
  s <- borders |>
    left_join(
      filter(data_wide, education == edu, age_group == age),
      by = c("ISO3_CODE" = "iso3")
    )
  stopifnot(nrow(s) == nrow(borders)) # one row, one geometry, one country
  s |> mutate(country = coalesce(country, NAME_ENGL))
}


# ---- drawing helpers -------------------------------------------------------

# NULL means "nothing to colour": colorNumeric() and addLegend() both fall over
# on an all-NA domain, and range() would hand back c(Inf, -Inf).
metric_domain <- function(...) {
  v <- c(...)
  if (all(is.na(v))) NULL else range(v, na.rm = TRUE)
}

metric_map <- function(sf_slice, col, domain) {
  v <- sf_slice[[col]]
  txt <- ifelse(
    is.na(v),
    "no data",
    formatC(v, format = "f", digits = 1, big.mark = " ")
  )
  lab <- lapply(sprintf("%s: %s", sf_slice$country, txt), htmltools::HTML)

  # colorNumeric() is fine with a wider domain than the data, which is what the
  # shared-scale toggle feeds it.
  pal <- if (is.null(domain)) {
    NULL
  } else {
    colorNumeric(metric_palette[[col]], domain = domain, na.color = "#e8e8e8")
  }
  fill <- if (is.null(pal)) "#e8e8e8" else pal(v)

  m <- leaflet(options = leafletOptions(minZoom = 3)) |>
    addProviderTiles(providers$CartoDB.PositronNoLabels) |>
    fitBounds(
      eu_bbox[["xmin"]],
      eu_bbox[["ymin"]],
      eu_bbox[["xmax"]],
      eu_bbox[["ymax"]]
    ) |>
    addPolygons(
      data = sf_slice,
      layerId = ~ISO3_CODE, # what a click reports back
      fillColor = fill,
      fillOpacity = 0.8,
      color = "white",
      weight = 1,
      label = lab,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#333",
        bringToFront = TRUE
      )
    )

  if (is.null(pal)) {
    m
  } else {
    m |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = domain,
        title = NULL,
        opacity = 0.9,
        labFormat = labelFormat(big.mark = " ")
      )
  }
}

# Columns are renamed to x/y because plotly's formula interface evaluates in the
# data frame and cannot see .data[[col]].
scatter_data <- function(sf_slice, xcol, ycol) {
  sf_slice |>
    sf::st_drop_geometry() |>
    transmute(
      iso3 = ISO3_CODE,
      country,
      x = .data[[xcol]],
      y = .data[[ycol]]
    ) |>
    # A point needs both values, and plotly warns "Ignoring n observations" for
    # every half-populated row it is handed.
    filter(!is.na(x), !is.na(y))
}

# Pearson and Spearman on the overlap, plus the n they are computed from --
# without the n, a strong r over 6 countries reads like a strong r over 30.
# Kept ASCII on purpose: this file is read under whatever native encoding R
# starts in, and the project has already been bitten by a latin1 round-trip.
cor_text <- function(d) {
  n <- nrow(d) # scatter_data() has already dropped incomplete pairs
  if (n < 3) {
    return(sprintf("n = %d - too few overlapping countries", n))
  }
  sprintf(
    "n = %d | Pearson r = %.2f | Spearman rho = %.2f",
    n,
    cor(d$x, d$y),
    cor(d$x, d$y, method = "spearman")
  )
}

trend_line <- function(d, method) {
  if (method == "none") {
    return(NULL)
  }
  dd <- d[!is.na(d$x) & !is.na(d$y), ]
  if (nrow(dd) < 4 || diff(range(dd$x)) == 0) {
    return(NULL)
  }
  grid <- data.frame(x = seq(min(dd$x), max(dd$x), length.out = 100))
  fit <- if (method == "lm") lm(y ~ x, dd) else loess(y ~ x, dd, span = 0.9)
  grid$y <- suppressWarnings(predict(fit, newdata = grid))
  grid[!is.na(grid$y), ]
}

metric_scatter <- function(d, xcol, ycol, trend, selected, source) {
  hl <- if (is.null(selected)) d[0, ] else d[d$iso3 %in% selected, ]

  # `source` is what ties a click on this trace back to event_data().
  p <- plot_ly(
    d,
    source = source,
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "markers",
    text = ~country,
    customdata = ~iso3,
    marker = list(
      size = 9,
      color = "#3b6ea5",
      line = list(width = 1, color = "white")
    ),
    hovertemplate = paste0(
      "<b>%{text}</b><br>",
      metric_label[[xcol]],
      ": %{x:,.1f}<br>",
      metric_label[[ycol]],
      ": %{y:,.1f}<extra></extra>"
    ),
    showlegend = FALSE
  )

  tl <- trend_line(d, trend)
  if (!is.null(tl)) {
    p <- add_lines(
      p,
      data = tl,
      x = ~x,
      y = ~y,
      inherit = FALSE,
      line = list(color = "#888", width = 1.5, dash = "dot"),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  }

  # The clicked country, ringed rather than recoloured, so its position on the
  # x/y ranges stays readable.
  if (nrow(hl)) {
    p <- add_markers(
      p,
      data = hl,
      x = ~x,
      y = ~y,
      inherit = FALSE,
      text = ~country,
      marker = list(
        size = 16,
        color = "rgba(0,0,0,0)",
        line = list(width = 3, color = "#d62728")
      ),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  }

  p |>
    layout(
      xaxis = list(title = metric_label[[xcol]], zeroline = FALSE),
      yaxis = list(title = metric_label[[ycol]], zeroline = FALSE),
      margin = list(l = 55, r = 10, t = 10, b = 45),
      # keeps a user's zoom/pan across the re-render a click triggers
      uirevision = "keep"
    ) |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d")
    )
}


# ---- ui --------------------------------------------------------------------

# Card ids follow map_<row>_<x|y> and scatter_<row>. The education level itself
# is the h5 above the row, not part of the cards.
edu_row_ui <- function(key) {
  layout_columns(
    col_widths = c(4, 4, 4),
    card(
      height = 400,
      full_screen = TRUE,
      card_header(textOutput(paste0("hdr_", key, "_x"), inline = TRUE)),
      leafletOutput(paste0("map_", key, "_x"), height = "100%")
    ),
    card(
      height = 400,
      full_screen = TRUE,
      card_header(textOutput(paste0("hdr_", key, "_y"), inline = TRUE)),
      leafletOutput(paste0("map_", key, "_y"), height = "100%")
    ),
    card(
      height = 400,
      full_screen = TRUE,
      card_header(textOutput(paste0("hdr_", key, "_s"), inline = TRUE)),
      plotlyOutput(paste0("scatter_", key), height = "100%")
    )
  )
}

ui <- page_sidebar(
  title = "Happiness on a map - two metrics, two degrees",
  sidebar = sidebar(
    width = 320,
    selectInput(
      "metric_x",
      "Metric 1 (x axis)",
      choices = setNames(metrics$col, metrics$label),
      selected = "pct_happy"
    ),
    selectInput(
      "metric_y",
      "Metric 2 (y axis)",
      choices = setNames(metrics$col, metrics$label),
      selected = "pct_satisfied_finances"
    ),
    selectInput(
      "age_group",
      "Age band",
      choices = levels(droplevels(coverage$age_group)),
      selected = "All ages"
    ),
    checkboxInput(
      "shared_scale",
      "Shared colour scale across the two rows",
      value = TRUE
    ),
    radioButtons(
      "trend",
      "Scatter trend line",
      choices = c("none" = "none", "linear (OLS)" = "lm", "loess" = "loess"),
      selected = "lm"
    ),
    hr(),
    uiOutput("selection"),
    hr(),
    helpText(
      "Click a country on any map, or a point in any scatter, to ring it",
      "in all six panels. Click it again to clear."
    ),
    helpText(
      "Non-European countries in the data (BRA, COL, KOR, USA) appear in",
      "the scatters but fall outside the map extent."
    )
  ),
  uiOutput("coverage_note"),
  h5(edu_rows[["mas"]], class = "mt-2 mb-1"),
  edu_row_ui("mas"),
  h5(edu_rows[["phd"]], class = "mt-3 mb-1"),
  edu_row_ui("phd")
)


# ---- server ----------------------------------------------------------------

server <- function(input, output, session) {
  selected <- reactiveVal(NULL) # iso3 of the clicked country, or NULL

  # Annotate the metric menus with the coverage of the chosen age band, so a
  # dead-end pair is visible before it is picked rather than after.
  observeEvent(input$age_group, {
    ok <- vapply(metrics$col, has_data, logical(1), age = input$age_group)
    labs <- ifelse(
      ok,
      metrics$label,
      paste0(metrics$label, " - no data at ", input$age_group)
    )
    ch <- setNames(metrics$col, labs)
    updateSelectInput(
      session,
      "metric_x",
      choices = ch,
      selected = input$metric_x
    )
    updateSelectInput(
      session,
      "metric_y",
      choices = ch,
      selected = input$metric_y
    )
  })

  slices <- reactive({
    lapply(edu_rows, edu_slice, age = input$age_group)
  })

  # NULL when the metric is empty in this band; both rows then share it, which
  # is what makes the two maps of a column comparable.
  domains <- reactive({
    s <- slices()
    lapply(c(x = input$metric_x, y = input$metric_y), function(col) {
      if (input$shared_scale) {
        list(
          mas = metric_domain(s$mas[[col]], s$phd[[col]]),
          phd = metric_domain(s$mas[[col]], s$phd[[col]])
        )
      } else {
        list(
          mas = metric_domain(s$mas[[col]]),
          phd = metric_domain(s$phd[[col]])
        )
      }
    })
  })

  output$coverage_note <- renderUI({
    chosen <- unique(c(input$metric_x, input$metric_y))
    missing <- chosen[
      !vapply(chosen, has_data, logical(1), age = input$age_group)
    ]
    if (!length(missing)) {
      return(NULL)
    }
    div(
      class = "alert alert-warning py-2 mb-2",
      strong("No data at ", input$age_group, ": "),
      paste(metric_label[missing], collapse = ", "),
      " - those maps are drawn grey and drop out of the scatter."
    )
  })

  output$selection <- renderUI({
    iso <- selected()
    if (is.null(iso)) {
      return(helpText("No country selected."))
    }
    rows <- data_wide |>
      filter(
        iso3 == !!iso,
        education %in% edu_rows,
        age_group == input$age_group
      )
    if (!nrow(rows)) {
      return(helpText("No country selected."))
    }
    fmt <- function(v) {
      if (is.na(v)) {
        "no data"
      } else {
        formatC(v, format = "f", digits = 1, big.mark = " ")
      }
    }
    tagList(
      strong(rows$country[1]),
      tags$ul(
        class = "ps-3 mb-1",
        lapply(seq_len(nrow(rows)), \(i) {
          tags$li(
            sprintf(
              "%s - %s: %s; %s: %s",
              as.character(rows$education[i]),
              metric_label[[input$metric_x]],
              fmt(rows[[input$metric_x]][i]),
              metric_label[[input$metric_y]],
              fmt(rows[[input$metric_y]][i])
            )
          )
        })
      ),
      actionLink("clear_sel", "clear selection")
    )
  })

  observeEvent(input$clear_sel, selected(NULL))

  # One block per row x panel. local() freezes the loop variables, otherwise all
  # six renderers would close over the last value of key/axis.
  for (key in names(edu_rows)) {
    for (axis in c("x", "y")) {
      local({
        k <- key
        a <- axis
        map_id <- paste0("map_", k, "_", a)

        output[[paste0("hdr_", k, "_", a)]] <- renderText(
          metric_label[[input[[paste0("metric_", a)]]]]
        )

        output[[map_id]] <- renderLeaflet({
          col <- input[[paste0("metric_", a)]]
          metric_map(slices()[[k]], col, domains()[[a]][[k]])
        })

        # Toggle: clicking the ringed country clears it.
        observeEvent(input[[paste0(map_id, "_shape_click")]], {
          iso <- input[[paste0(map_id, "_shape_click")]]$id
          selected(if (identical(selected(), iso)) NULL else iso)
        })
      })
    }

    local({
      k <- key
      output[[paste0("hdr_", k, "_s")]] <- renderText({
        cor_text(scatter_data(slices()[[k]], input$metric_x, input$metric_y))
      })
      output[[paste0("scatter_", k)]] <- renderPlotly({
        metric_scatter(
          scatter_data(slices()[[k]], input$metric_x, input$metric_y),
          input$metric_x,
          input$metric_y,
          input$trend,
          selected(),
          source = paste0("scatter_", k)
        )
      })
    })
  }

  # Scatter clicks. event_data() resets to NULL on the re-render the selection
  # causes, which is what lets the same point toggle off on a second click.
  for (key in names(edu_rows)) {
    local({
      k <- key
      observeEvent(event_data("plotly_click", source = paste0("scatter_", k)), {
        ev <- event_data("plotly_click", source = paste0("scatter_", k))
        iso <- ev$customdata
        if (is.null(iso) || !length(iso)) {
          return()
        }
        selected(if (identical(selected(), iso[[1]])) NULL else iso[[1]])
      })
    })
  }

  # Ring the selection on all four maps without redrawing them.
  observeEvent(
    selected(),
    {
      iso <- selected()
      g <- if (is.null(iso)) {
        borders[0, ]
      } else {
        borders[borders$ISO3_CODE == iso, ]
      }
      for (key in names(edu_rows)) {
        for (axis in c("x", "y")) {
          p <- leafletProxy(paste0("map_", key, "_", axis), session) |>
            clearGroup("highlight")
          if (nrow(g)) {
            addPolylines(
              p,
              data = g,
              group = "highlight",
              color = "#d62728",
              weight = 3,
              opacity = 1
            )
          }
        }
      }
    },
    ignoreNULL = FALSE
  )
}

shinyApp(ui, server)

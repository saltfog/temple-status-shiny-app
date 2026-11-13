# LDS temples map (Shiny)
# Source: https://churchofjesuschristtemples.org/maps/gps/ (third-party site)
# Required packages: shiny, bslib, leaflet, dplyr, DT

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(DT)

# ---- Data (loaded once at app start) ---------------------------------------
gps_url <- "https://churchofjesuschristtemples.org/maps/gps/"
gps_file <- tempfile(fileext = ".csv")
# Fallback copy of the last successful download, kept next to the app
cache_file <- file.path("data", "temples_cache.csv")

downloaded <- tryCatch({
  download.file(gps_url, gps_file, mode = "wb", quiet = TRUE,
                headers = c("User-Agent" = "Mozilla/5.0"))
  TRUE
}, error = function(e) FALSE, warning = function(w) FALSE)

# The file is latin1-encoded. The degrees columns contain a stray double quote and
# some addresses contain unquoted commas, so read.csv() misparses rows. Split
# manually: 4 fixed fields up front, 2 degree fields, then Address (may contain
# commas), then 6 fixed fields (City, County, State, Postal Code, Country, Phone).
read_temples <- function(file) {
  lines <- readLines(file, encoding = "latin1", warn = FALSE)[-1]
  lines <- lines[nzchar(lines)]
  # Sentinel keeps trailing empty fields from being dropped by strsplit()
  parts <- strsplit(paste0(lines, ",x"), ",", fixed = TRUE) |>
    lapply(\(p) p[-length(p)])
  data.frame(
    Temple = vapply(parts, `[`, "", 1),
    Status = vapply(parts, `[`, "", 2),
    Latitude = as.numeric(vapply(parts, `[`, "", 3)),
    Longitude = as.numeric(vapply(parts, `[`, "", 4)),
    Address = vapply(parts, \(p) paste(p[seq(7, length(p) - 6)], collapse = ","), ""),
    City = vapply(parts, \(p) p[length(p) - 5], ""),
    State = vapply(parts, \(p) p[length(p) - 3], ""),
    Postal = vapply(parts, \(p) p[length(p) - 2], ""),
    Country = vapply(parts, \(p) p[length(p) - 1], ""),
    Phone = vapply(parts, \(p) p[length(p)], "")
  )
}

# Use the fresh download if it parses to a plausible number of temples, and refresh
# the cache with it. Otherwise fall back to the cached copy.
temples <- if (downloaded) tryCatch(read_temples(gps_file), error = function(e) NULL)
fresh <- !is.null(temples) && nrow(temples) >= 100 &&
  all(c("Dedicated", "Under Construction") %in% temples$Status)

if (fresh) {
  dir.create(dirname(cache_file), showWarnings = FALSE)
  try(file.copy(gps_file, cache_file, overwrite = TRUE), silent = TRUE)
  data_note <- paste("Data downloaded", format(Sys.time(), "%Y-%m-%d %H:%M"))
} else if (file.exists(cache_file)) {
  temples <- read_temples(cache_file)
  data_note <- paste("Download failed; using cached data from",
                     format(file.mtime(cache_file), "%Y-%m-%d"))
  warning(data_note, call. = FALSE)
} else {
  stop("Could not download the temple data and no cached copy exists at ",
       cache_file, call. = FALSE)
}

# Announced temples without an announced site have no coordinates, so drop them
temple_map <- temples |>
  filter(Status %in% c("Dedicated", "Under Construction", "Announced"),
         !is.na(Latitude), !is.na(Longitude))

# Legend swatches use the theme colors of the count badges (success/warning/primary)
app_theme <- bs_theme(version = 5)
status_colors <- setNames(
  unname(bs_get_variables(app_theme, c("success", "warning", "primary"))),
  c("Dedicated", "Under Construction", "Announced")
)
countries <- sort(unique(temple_map$Country))

status_glyphs <- c(
  Dedicated = "circle-check",
  `Under Construction` = "person-digging",
  Announced = "bullhorn"
)

make_pin <- function(color, glyph) {
  fa_svg <- as.character(fontawesome::fa(glyph))
  view_box <- as.numeric(strsplit(sub('.*viewBox="([^"]+)".*', "\\1", fa_svg), " ")[[1]])
  path_d <- sub('.*<path d="([^"]+)".*', "\\1", fa_svg)
  # Scale the glyph to fit a 14px box centred in the pin's head
  scale <- 14 / max(view_box[3:4])
  tx <- 15 - view_box[3] * scale / 2
  ty <- 15 - view_box[4] * scale / 2
  svg <- sprintf(
    paste0('<svg xmlns="http://www.w3.org/2000/svg" width="30" height="42" ',
           'viewBox="0 0 30 42">',
           '<path d="M15 1C7 1 1 7 1 15c0 10.5 14 26 14 26s14-15.5 14-26C29 7 23 1 15 1z" ',
           'fill="%s" stroke="#333333" stroke-width="1.5"/>',
           '<path d="%s" fill="white" transform="translate(%f %f) scale(%f)"/></svg>'),
    color, path_d, tx, ty, scale
  )
  paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(svg)))
}

pin_urls <- mapply(make_pin, status_colors[names(status_glyphs)], status_glyphs)
names(pin_urls) <- names(status_glyphs)

# Keyless basemaps
basemaps <- list(
  Light = providers$Esri.NatGeoWorldMap,
  Satellite = providers$USGS.USImageryTopo
)

temple_popup <- function(df) {
  esc <- htmltools::htmlEscape
  vapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    # City, state and postal code on one line; drop blank parts
    locality <- paste(Filter(nzchar, c(r$City, r$State, r$Postal)), collapse = ", ")
    lines <- c(
      paste0("<b>", esc(r$Temple), "</b>"),
      esc(r$Status),
      if (nzchar(r$Address)) esc(r$Address),
      if (nzchar(locality)) esc(locality),
      if (nzchar(r$Country)) esc(r$Country),
      if (nzchar(r$Phone)) {
        sprintf('<a href="tel:%s">%s</a>', gsub("[^0-9+]", "", r$Phone), esc(r$Phone))
      }
    )
    paste(lines, collapse = "<br>")
  }, "")
}

# ---- UI --------------------------------------------------------------------
ui <- page_sidebar(
  title = "LDS Temples",
  theme = app_theme,
  fillable = TRUE,
  tags$head(tags$style(HTML("
    /* Keeps the map usable when the page is short, e.g. phones in landscape */
    #map { min-height: 400px; }
    @media (max-width: 575.98px) {
      .badge { font-size: .8rem !important; }
      .leaflet-control.legend { font-size: 11px; line-height: 1.2; padding: 4px 6px; }
      .dataTables_wrapper { font-size: .85rem; }
      /* Hide the per-column filter row (its cells are td; header cells are th) */
      .dataTables_wrapper thead tr:has(> td) { display: none; }
    }
  "))),
  sidebar = sidebar(
    input_dark_mode(id = "dark_mode"),
    radioButtons("basemap", "Basemap", choices = names(basemaps), selected = "Light"),
    checkboxGroupInput(
      "status", "Temple status",
      choices = names(status_colors),
      selected = names(status_colors)
    ),
    selectizeInput(
      "country", "Country",
      choices = countries, selected = NULL, multiple = TRUE,
      options = list(placeholder = "All countries")
    ),
    downloadButton("download", "Download CSV"),
    hr(),
    helpText("Announced temples with no announced site have no coordinates",
             "and are not shown."),
    helpText(data_note)
  ),
  # Compact status counts
  div(
    class = "d-flex flex-wrap gap-1 gap-sm-2 mb-2",
    span(class = "badge text-bg-success fs-6 fw-normal",
         "Dedicated: ", textOutput("n_dedicated", inline = TRUE)),
    span(class = "badge text-bg-warning fs-6 fw-normal",
         "Under construction: ", textOutput("n_construction", inline = TRUE)),
    span(class = "badge text-bg-primary fs-6 fw-normal",
         "Announced: ", textOutput("n_announced", inline = TRUE))
  ),
  navset_card_underline(
    id = "tabs",
    full_screen = TRUE,
    nav_panel("Map", leafletOutput("map", height = "100%")),
    nav_panel("Table", DTOutput("table"))
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {

  filtered <- reactive({
    df <- filter(temple_map, Status %in% input$status)
    if (length(input$country)) df <- filter(df, Country %in% input$country)
    df
  })

  count_status <- function(status) {
    # Counts respect the country filter but not the status checkboxes
    df <- filter(temple_map, Status == status)
    if (length(input$country)) df <- filter(df, Country %in% input$country)
    format(nrow(df), big.mark = ",")
  }
  output$n_dedicated <- renderText(count_status("Dedicated"))
  output$n_construction <- renderText(count_status("Under Construction"))
  output$n_announced <- renderText(count_status("Announced"))

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(basemaps[[isolate(input$basemap)]]) |>
      addLegend(colors = unname(status_colors), labels = names(status_colors),
                title = "Temple status", opacity = 1) |>
      setView(lng = 0, lat = 20, zoom = 2)
  })

  # Swap the basemap tiles without redrawing the map or markers
  observeEvent(input$basemap, {
    leafletProxy("map") |>
      clearTiles() |>
      addProviderTiles(basemaps[[input$basemap]])
  }, ignoreInit = TRUE)

  # Update markers (and zoom to them) without redrawing the whole map
  observe({
    df <- filtered()
    proxy <- leafletProxy("map") |> clearMarkerClusters()
    if (nrow(df) == 0) return()
    proxy |>
      addMarkers(
        data = df,
        lng = ~Longitude, lat = ~Latitude,
        icon = icons(iconUrl = unname(pin_urls[df$Status]),
                     iconWidth = 24, iconHeight = 34,
                     iconAnchorX = 12, iconAnchorY = 34,
                     popupAnchorX = 0, popupAnchorY = -30),
        label = ~Temple,
        popup = temple_popup(df),
        clusterOptions = markerClusterOptions()
      )
    if (length(input$country)) {
      proxy |> fitBounds(min(df$Longitude), min(df$Latitude),
                         max(df$Longitude), max(df$Latitude))
    }
  })

  output$table <- renderDT({
    filtered() |>
      select(Temple, Status, City, State, Country, Phone, Latitude, Longitude) |>
      # Phone becomes a tel: link (escaped by hand; other columns stay escaped by DT)
      mutate(Phone = ifelse(
        nzchar(Phone),
        sprintf('<a href="tel:%s">%s</a>', gsub("[^0-9+]", "", Phone),
                htmltools::htmlEscape(Phone)),
        ""
      )) |>
      datatable(
        rownames = FALSE, filter = "top", selection = "single",
        escape = setdiff(c("Temple", "Status", "City", "State", "Country",
                           "Latitude", "Longitude"), "Phone"),
        # Responsive collapses low-priority columns on narrow screens;
        # Buttons provides the column visibility menu
        extensions = c("Responsive", "Buttons"),
        options = list(
          pageLength = 15, search = list(regex = FALSE), responsive = TRUE,
          dom = "Bfrtip",
          buttons = list(list(extend = "colvis", text = "Columns")),
          # State and Phone (0-based: 3, 5) are hidden until chosen
          columnDefs = list(list(visible = FALSE, targets = c(3, 5)))
        )
      )
  })

  # Clicking a table row switches to the map and zooms to that temple. The zoom
  # waits until the Map tab is visible so Leaflet knows the size of its container.
  target <- reactiveVal(NULL)

  observeEvent(input$table_rows_selected, {
    # Row indices refer to the data passed to datatable(), i.e. filtered()
    target(filtered()[input$table_rows_selected, ])
    nav_select("tabs", "Map")
    # Clear the selection so the same row can be clicked again later
    selectRows(dataTableProxy("table"), NULL)
  })

  observeEvent(list(input$tabs, target()), {
    req(input$tabs == "Map", target())
    t <- target()
    leafletProxy("map") |>
      flyTo(t$Longitude, t$Latitude, zoom = 13) |>
      clearPopups() |>
      addPopups(t$Longitude, t$Latitude, temple_popup(t))
    target(NULL)
  })

  output$download <- downloadHandler(
    filename = function() "lds_temples.csv",
    content = function(file) {
      filtered() |>
        select(Temple, Status, Latitude, Longitude) |>
        write.csv(file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)

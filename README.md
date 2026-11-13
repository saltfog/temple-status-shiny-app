# LDS Temples Interactive Map

An interactive R Shiny application for exploring temples of The Church of Jesus Christ of Latter-day Saints around the world.

The application displays temple locations on a map, allowing users to explore temples by country and construction status.

## Live Demo

**Try the application online:**

https://dodaac-temple-app.share.connect.posit.cloud

The application is hosted on Posit Connect Cloud and can be accessed directly through your web browser without installing R or Shiny.

## Features

- **Interactive Map** — Explore temple locations worldwide using Leaflet.
- **Temple Status** — Filter temples by Dedicated, Under Construction, or Announced.
- **Custom Map Markers** — Color-coded SVG pins with Font Awesome icons.
- **Country Filtering** — View temples in selected countries.
- **Map Layers** — Switch between standard and satellite-style maps.
- **Searchable Table** — Browse temple information in an interactive data table.
- **Map Navigation** — Select a temple from the table to locate it on the map.
- **CSV Export** — Download filtered temple locations and coordinates.
- **Dark Mode** — Switch between light and dark themes.
- **Responsive Design** — Supports desktop and mobile browsers.

## Built With

- R
- Shiny and bslib
- Leaflet
- dplyr
- DT
- Font Awesome
- Bootstrap 5

## Getting Started

Install the required R packages:

```r
install.packages(c(
  "shiny",
  "bslib",
  "leaflet",
  "dplyr",
  "DT",
  "fontawesome",
  "base64enc",
  "htmltools"
))
```

Run the application from R or RStudio:

```r
shiny::runApp()
```

Run this command from the project directory containing `app.R`.

## Data Source

Temple information is retrieved from:

https://churchofjesuschristtemples.org/maps/gps/

The application attempts to download updated temple information when it starts. If the download fails, it uses a previously cached copy when available.

**Note:** Announced temples without published geographic coordinates cannot be displayed on the map.

## Disclaimer

This is an independent project and is not affiliated with or endorsed by The Church of Jesus Christ of Latter-day Saints.

Temple information is provided by a third-party source and may change over time.

## License

No license has been specified. Add a `LICENSE` file if you intend to define permissions for others to use, modify, or distribute the project.

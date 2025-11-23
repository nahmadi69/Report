# Export and Forecast Dashboard

This Shiny application presents interactive export and forecast data dashboards.

## Features

- Data upload for export, MA, forecast, and sales files in Excel format (.xlsx).
- Dynamic filtering by year, month, category, manufacturer, country, consignee, medicine, dosage, and forecast type.
- Interactive plots visualizing export and forecast trends using Plotly.
- Reactable tables with data download options.
- Resizable sidebar for customized UI experience.

## Installation

Ensure you have R and RStudio installed. Then install the required packages:

```r
install.packages(c("shiny", "readxl", "rsconnect", "dplyr", "ggplot2", "plotly", "tidyverse", "DT", "viridis", "webshot", "RColorBrewer", "tidyr", "scales", "bslib", "reactable", "bsicons", "shinyWidgets", "lubridate"))
```

## Usage

1. Clone this repository:
```
git clone https://github.com/nahmadi69/Report.git
```
2. Open the app in RStudio:
```r
shiny::runApp("Report")
```
3. Upload your data files in the left sidebar and use filters to explore exports and forecasts.

## Author

Naser Ahmadi

## License

Specify a license if applicable.

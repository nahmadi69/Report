library(shiny)
library(readxl)
library(rsconnect)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(DT)
library(viridis)
library(webshot)
library(RColorBrewer)
library(tidyr)
library(scales)
library(bslib)
library(reactable)
library(bsicons) # Added for value box icons
library(scales)
library(shinyWidgets) # <-- Add this line

# A custom function to create a value box output, simplifying the UI code
value_box_output <- function(id) {
  uiOutput(id)
}

ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "cosmo",
    primary = "#0077be",
    "enable-rounded" = TRUE,
    "body-bg" = "#f8f9fa"
  ),
  layout_column_wrap(
    width = 1,
    style = "background: linear-gradient(to right, #f8f9fa, #e9ecef); padding: 5px; border-bottom: 1px solid #dee2e6; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    card(
      full_screen = FALSE,
      height = "auto",
      width = 1,
      card_body(
        class = "d-flex justify-content-between align-items-center p-2",
        div(
          class = "d-flex align-items-center",
          h3("Export and Forecast Dashboard", 
             style = "color: #3c4858; font-weight: 400; margin: 0; font-size: 1.2rem;")
        )
      )
    )
  ),
  tags$head(
    tags$style(HTML("
      body {
          overflow-y: scroll;
          overflow-x: scroll;
          font-family: 'Open Sans';
          background-color: #f7f7f7;
        }
      .tab-content { padding-top: 20px; }
      .accordion-item { margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,.1); }
      .accordion-button { background-color: #f8f9fa; }
      .accordion-button:not(.collapsed) { background-color: #e9ecef; }
    "))
  ),
  
  navset_tab(
    nav_panel("Export",
              layout_sidebar(
                sidebar = sidebar(
                  width = 300,
                  fileInput("file_1", "Upload your export file", accept = c(".xlsx")),
                  accordion(
                    accordion_panel(
                      "Date selection panel",
                      uiOutput("V_Year_ui_1"),
                      uiOutput("V_Month_ui_1")
                    ),
                    accordion_panel(
                      "Filters panel",
                      uiOutput("Category_ui_1"),
                      uiOutput("Year_ui_1"),
                      uiOutput("Month_ui_1"),
                      uiOutput("Manufacturer_ui_1"),
                      uiOutput("Country_ui_1"),
                      uiOutput("Medicine_ui_1"),
                      uiOutput("Dosage_ui_1")#,
                    ),
                    card(
                      actionButton("calcButton_1", "Calculate", class = "btn-primary btn-block mt-3")
                      # img(src = "CinnaGen_Logo.png", height = 80, width = 160, style = "object-fit: contain; margin-right: 20px;"),
                    )                    
                  ),
                  card(
                    card_footer("Author: Naser Ahmadi", a(href = "mailto:Naserahmadi3002@gmail.com", "Email"))
                  )
                ),
                # NEW: Export-specific key metric cards
                layout_column_wrap(
                  width = "250px",
                  fill = FALSE,
                  value_box_output("total_net_export_card"),
                  # value_box_output("total_qty_export_card"),
                  value_box_output("top_export_country_card"),
                  value_box_output("top_export_medicine_card")
                ),
                accordion(
                  accordion_panel(
                    "Data Table",
                    style = "width: 1500px;",
                    reactableOutput("data_1"),
                    downloadButton("downloadTable1", "Download Table", class = "btn-sm btn-secondary mt-2")
                  ),
                  accordion_panel("Year and Manufacturer Plot", style = "width: 1500px;", plotlyOutput('plot_year')),
                  accordion_panel("Country Plot", style = "width: 1500px;", plotlyOutput('plot_Country')),
                  accordion_panel(
                    "Table Country",
                    style = "width: 1500px;",
                    reactableOutput("data_country111"),
                    downloadButton("downloadTable111", "Download Table", class = "btn-sm btn-secondary mt-2")
                  ),      
                  accordion_panel("Category Plot", style = "width: 1500px;", plotlyOutput("plot_Manufacturer")),
                  accordion_panel("Medicine Plot", style = "width: 1500px;", plotlyOutput("plot_Medicine")),
                  accordion_panel("Heat Map", style = "width: 1500px;", plotlyOutput("plot_heat_map")),
                  accordion_panel(
                    "Additional data",
                    style = "width: 1500px;",
                    downloadButton("downloadTable_country", "Download Country year data", class = "btn-sm btn-secondary mt-2"),
                    downloadButton("downloadTable_medicine", "Download Medicine year data", class = "btn-sm btn-secondary mt-2")
                  ),      
                  
                )
              )
    ),
    nav_panel("Forecast",
              layout_sidebar(
                sidebar = sidebar(
                  width = 300,
                  fileInput("file_forcast", "Upload your forecast file", accept = c(".xlsx")),
                  fileInput("file_sales", "Upload your sales file", accept = c(".xlsx")),
                  accordion(
                    accordion_panel(
                      "Date selection panel",
                      uiOutput("V_Year_ui_2")
                    ),
                    accordion_panel(
                      "Filters panel",
                      uiOutput("Probability_ui_2"),
                      uiOutput("Category_ui_2"),
                      uiOutput("Year_ui_2"),
                      uiOutput("Manufacturer_ui_2"),
                      uiOutput("Country_ui_2"),
                      uiOutput("Medicine_ui_2"),
                      uiOutput("Dosage_ui_2"),
                      uiOutput("type_ui_2")
                      # ,
                      # RESTORED: Calculate button for forecast tab
                      # actionButton("calcButton_2", "Calculate", class = "btn-primary btn-block mt-3")
                    ),
                    card(
                      # img(src = "CinnaGen_Logo.png", height = 80, width = 160, style = "object-fit: contain; margin-right: 20px;"),
                      actionButton("calcButton_2", "Calculate", class = "btn-primary btn-block mt-3")
                      
                    )
                  ),
                  card(
                    card_footer("Author: Naser Ahmadi", a(href = "mailto:Naserahmadi3002@gmail.com", "Email"))
                  )
                ),
                # Main content area with forecast cards and accordion
                div(
                  # MOVED BACK: Forecast comparison cards
                  layout_column_wrap(
                    width = "250px",
                    fill = FALSE,
                    value_box_output("forecast_card"),
                    value_box_output("actual_card"),
                    # value_box_output("achievement_card"),
                    value_box_output("top_country_card"),
                    value_box_output("top_medicine_card")
                  ),
                  accordion(
                    accordion_panel(
                      "Data Table",
                      style = "width: 1500px;",
                      reactableOutput("data_2"),
                      downloadButton("downloadTable2", "Download Table", class = "btn-sm btn-secondary mt-2")
                    ),
                    accordion_panel("Year Forecast", style = "width: 1500px;", plotlyOutput('plot_Year_Forcast')),
                    accordion_panel("Manufacturer Forecast", style = "width: 1500px;", plotlyOutput('plot_Manufacturer_Forcast')),
                    accordion_panel("Medicine Forecast", style = "width: 1500px;", plotlyOutput('plot_Medicine_Forcast')),
                    accordion_panel("Country Forecast", style = "width: 1500px;", plotlyOutput('plot_Country_Forcast')),
                    accordion_panel("Manufacturer Forecast Net", style = "width: 1500px;", plotlyOutput('plot_Manufacturer_Forcast_Net')),
                    accordion_panel("Medicine Forecast Net", style = "width: 1500px;", plotlyOutput('plot_Medicine_Forcast_Net'))
                  )
                )
              )
    )
  )
)

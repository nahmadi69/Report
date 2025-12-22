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
library(bsicons)
library(shinyWidgets)
library(lubridate)
library(googlesheets4)


value_box_output <- function(id) {
  uiOutput(id)
}

ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#214b72",
    secondary = "#adb5bd",
    bg = "#f8f9fb",
    fg = "#2c3e50",
    "enable-rounded" = TRUE,
    "font-size-base" = "1rem",
    "font-family-base" = "'Segoe UI', 'Arial', sans-serif"
  ),
  
  # Header
  layout_column_wrap(
    width = 1,
    style = "background: #f8f9fb; padding: 5px; border-bottom: 1px solid #dee2e6; box-shadow: 0 2px 8px rgba(34,74,94,0.07);",
    card(
      full_screen = FALSE,
      height = "auto",
      width = 1,
      card_body(
        class = "d-flex justify-content-between align-items-center p-2",
        div(
          class = "d-flex align-items-center",
          h3("Export and Forecast Dashboard", 
             style = "color: #214b72; font-weight: 600; margin: 0; font-size: 1.3rem; letter-spacing:0.02em;")
        )
      )
    )
  ),
  
  # Custom CSS and JavaScript
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', 'Arial', sans-serif;
        background-color: #f8f9fb;
      }
      .tab-content {
        padding-top: 16px;
      }
      .accordion-item {
        margin-bottom: 15px;
        box-shadow: 0 2px 8px rgba(33,75,114,0.08);
        border-radius: 8px;
        border: 1px solid #e3e6ea;
      }
      .accordion-button {
        background-color: #ffffff;
        font-weight: 600;
        color: #214b72;
        padding: 12px 16px;
      }
      .accordion-button:not(.collapsed) {
        background-color: #f0f4f8;
        color: #214b72;
      }
      .accordion-button:hover {
        background-color: #f8f9fb;
      }
      .accordion-body {
        padding: 16px;
        background-color: #ffffff;
      }
      .card, .bslib-card {
        background-color: #fff;
        border: 1px solid #dee2e6;
        border-radius: 10px;
      }
      .value-box, .bslib-value-box {
        border-radius: 10px !important;
      }

      /* Formal Resizable Sidebar */
      .resizable-sidebar {
        position: relative;
      }
      .sidebar-resizer {
        position: absolute;
        right: 0;
        top: 0;
        bottom: 0;
        width: 8px;
        cursor: ew-resize;
        background: transparent;
        z-index: 1000;
        border-right: 1px solid #ced4da;
        transition: background 0.2s;
      }
      .sidebar-resizer:hover {
        background: #e9ecef;
      }
      .sidebar-resizer:active {
        background: #b0c4da;
      }
      .bslib-sidebar-layout {
        --_sidebar-width: 450px;
      }
      .no-select {
        user-select: none;
      }
      .sidebar {
        border-right: 1.5px solid #dee2e6;
        background: #f6f7fa;
      }

      /* File input section styling */
      .file-upload-section {
        margin-bottom: 15px;
        padding: 15px;
        background: #ffffff;
        border-radius: 8px;
        border: 1px solid #e3e6ea;
      }
      .section-title {
        font-size: 0.95rem;
        font-weight: 600;
        color: #214b72;
        margin-bottom: 12px;
        padding-bottom: 8px;
        border-bottom: 2px solid #e3e6ea;
      }

      /* Responsive accordion width - FIXED */
      .accordion-panel-content {
        width: 100%;
        max-width: 100%;
        overflow-x: auto;
      }
    ")),
    
    tags$script(HTML("
      $(document).ready(function() {
        let isResizing = false;
        let startX, startWidth;
        let currentSidebar = null;

        // Add resizer handle to all sidebars
        $('.bslib-sidebar-layout > .sidebar').each(function() {
          if ($(this).find('.sidebar-resizer').length === 0) {
            $(this).addClass('resizable-sidebar');
            $(this).append('<div class=\"sidebar-resizer\"></div>');
          }
        });

        $(document).on('mousedown', '.sidebar-resizer', function(e) {
          isResizing = true;
          startX = e.clientX;
          currentSidebar = $(this).closest('.sidebar');
          startWidth = currentSidebar.outerWidth();
          $('body').addClass('no-select');
          e.preventDefault();
        });

        $(document).on('mousemove', function(e) {
          if (!isResizing || !currentSidebar) return;
          const newWidth = startWidth + (e.clientX - startX);
          if (newWidth >= 260 && newWidth <= 700) {
            currentSidebar.closest('.bslib-sidebar-layout').css('--_sidebar-width', newWidth + 'px');
          }
        });

        $(document).on('mouseup', function() {
          if (isResizing) {
            isResizing = false;
            currentSidebar = null;
            $('body').removeClass('no-select');
          }
        });

        $(document).on('mouseleave', function() {
          if (isResizing) {
            isResizing = false;
            currentSidebar = null;
            $('body').removeClass('no-select');
          }
        });
      });
    "))
  ),
  
  # Main Tabs
  navset_tab(
    # Export Tab
    nav_panel("Export",
              layout_sidebar(
                sidebar = sidebar(
                  width = 300,
                  
                  # File Upload Section (always visible)
                  # accordion(
                  #   class = "file-upload-section",
                  #   div(class = "section-title", "Data Upload"),
                  #   fileInput("file_1", "Upload your export file", accept = c(".xlsx")),
                  #   # fileInput("MA", "Upload your MA file", accept = c(".xlsx"))
                  # ),
                  # 
                  # Collapsible Accordion Sections
                  accordion(
                    accordion_panel(
                      "Date Selection",
                      fileInput("file_1", "Upload your export file", accept = c(".xlsx")),
                      h1(),
                      uiOutput("V_Year_ui_1"),
                      uiOutput("V_Month_ui_1")
                    ),
                    accordion_panel(
                      "Filters",
                      uiOutput("Category_ui_1"),
                      uiOutput("Year_ui_1"),
                      uiOutput("Month_ui_1"),
                      uiOutput("Manufacturer_ui_1"),
                      uiOutput("Country_ui_1"),
                      uiOutput("Consignee_ui_1"),
                      uiOutput("Medicine_ui_1"),
                      uiOutput("Dosage_ui_1")
                    )
                  ),
                  
                  # Action Button
                  card(
                    style = "margin-top: 15px;",
                    actionButton("calcButton_1", "Calculate", 
                                 class = "btn-primary btn-block", 
                                 style = "font-weight: 500;")
                  ),
                  
                  # Footer
                  card(
                    card_footer(
                      "Author: Naser Ahmadi"
                    )
                  )
                ),
                
                # Main content area
                layout_column_wrap(
                  width = "240px",
                  fill = FALSE,
                  value_box_output("total_net_export_card"),
                  value_box_output("top_export_country_card"),
                  value_box_output("top_export_medicine_card")
                ),
                
                accordion(
                  accordion_panel(
                    "Data Table",
                    reactableOutput("data_1"),
                    downloadButton("downloadTable1", "Download Table", 
                                   class = "btn-sm btn-secondary mt-2")
                  ),
                  accordion_panel("Year and Manufacturer Plot", 
                                  plotlyOutput('plot_year')),
                  accordion_panel("Country Plot", 
                                  plotlyOutput('plot_Country')),
                  accordion_panel("Consignee Plot", 
                                  plotlyOutput('plot_Consignee')),
                  accordion_panel("Category Plot", 
                                  plotlyOutput("plot_Category")),
                  accordion_panel("Medicine Plot", 
                                  plotlyOutput("plot_Medicine")),
                  accordion_panel(
                    "Additional data",
                    downloadButton("downloadTable_country", "Download Country year data", 
                                   class = "btn-sm btn-secondary mt-2"),
                    downloadButton("downloadTable_medicine", "Download Medicine year data", 
                                   class = "btn-sm btn-secondary mt-2"),
                    downloadButton("downloadTable_consignee", "Download consignee year data", 
                                   class = "btn-sm btn-secondary mt-2"),
                  )
                )
              )
    ),
    
    nav_panel("MA",
              layout_sidebar(
                sidebar = sidebar(
                  width = 300,
                  
                  # div(
                  #   class = "file-upload-section",
                  #   div(class = "section-title", "Data Upload"),
                  #   # fileInput("MA", "Upload your MA file", accept = c(".xlsx"))
                  # ),
                  # Single Filters Panel (ALL filters together)
                  accordion(
                    accordion_panel(
                      "Filters",
                      fileInput("file_MA", "Upload your export file", accept = c(".xlsx")),
                      
                      uiOutput("Year_ui_MA"),
                      uiOutput("Manufacturer_ui_MA"),
                      uiOutput("Country_ui_MA"),
                      uiOutput("Consignee_ui_MA"),
                      uiOutput("Medicine_ui_MA"),
                      uiOutput("Dosage_ui_MA")
                    )
                  ),
                  
                  # Action Button
                  card(
                    style = "margin-top: 15px;",
                    actionButton("calcButton_MA", "Filter MA Data", 
                                 class = "btn-primary btn-block", 
                                 style = "font-weight: 500;")
                  ),
                  
                  # Footer
                  card(
                    card_footer(
                      "Author: Naser Ahmadi"
                    )
                  )
                ),
                
                
                
                # Main content area (unchanged)
                layout_column_wrap(
                  width = "200px",
                  fill = FALSE,
                  value_box_output("total_net_MA_card"),
                  value_box_output("Ma_card")
                ),
                
                accordion(
                  accordion_panel("Year and MA status Plot", 
                                  plotlyOutput('plot_year_MA')),
                  accordion_panel(
                    "Export With MA Table",
                    reactableOutput("data_11"),
                    downloadButton("downloadTable_MA_percent", "Download Table", 
                                   class = "btn-sm btn-secondary mt-2")
                  ),
                  accordion_panel(
                    "MA Table",
                    reactableOutput("data_MA_11"),
                    downloadButton("downloadTable_MA", "Download MA list", 
                                   class = "btn-sm btn-secondary mt-2")
                  )
                )
              )
    ),
    # Forecast Tab
    nav_panel("Forecast",
              layout_sidebar(
                sidebar = sidebar(
                  width = 300,
                  
                  # File Upload Section (always visible)
                  div(
                    class = "file-upload-section",
                    div(class = "section-title", "Data Upload"),
                    fileInput("file_forcast", "Upload your forecast file", accept = c(".xlsx")),
                    fileInput("file_sales", "Upload your sales file", accept = c(".xlsx"))
                  ),
                  
                  # Collapsible Accordion Sections
                  accordion(
                    accordion_panel(
                      "Date Selection",
                      uiOutput("V_Year_ui_2")
                    ),
                    accordion_panel(
                      "Filters",
                      uiOutput("Probability_ui_2"),
                      uiOutput("Category_ui_2"),
                      uiOutput("Year_ui_2"),
                      uiOutput("Manufacturer_ui_2"),
                      uiOutput("Country_ui_2"),
                      uiOutput("Medicine_ui_2"),
                      uiOutput("Dosage_ui_2"),
                    )
                  ),
                  
                  # Action Button
                  card(
                    style = "margin-top: 15px;",
                    actionButton("calcButton_2", "Calculate", 
                                 class = "btn-primary btn-block", 
                                 style = "font-weight: 500;")
                  ),
                  
                  # Footer
                  card(
                    card_footer(
                      "Author: Naser Ahmadi"
                    )
                  )
                ),
                
                # Main content area
                div(
                  layout_column_wrap(
                    width = "240px",
                    fill = FALSE,
                    value_box_output("forecast_card"),
                    value_box_output("actual_card"),
                    value_box_output("top_country_card"),
                    value_box_output("top_medicine_card")
                  ),
                  
                  accordion(
                    accordion_panel(
                      "Data Table",
                      reactableOutput("data_2"),
                      downloadButton("downloadTable2", "Download Table", 
                                     class = "btn-sm btn-secondary mt-2")
                    ),
                    accordion_panel("Year Forecast", 
                                    plotlyOutput('plot_Year_Forcast')),
                    accordion_panel("Manufacturer Forecast", 
                                    plotlyOutput('plot_Manufacturer_Forcast')),
                    accordion_panel("Medicine Forecast", 
                                    plotlyOutput('plot_Medicine_Forcast')),
                    accordion_panel("Country Forecast", 
                                    plotlyOutput('plot_Country_Forcast')),

                  )
                )
              )
    )
  )
)

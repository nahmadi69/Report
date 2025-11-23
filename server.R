library(shiny)
library(readxl)
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

source("server_module.R")

function(input, output, session) {
  
  # Helper function for color palette generation
  generate_palette <- function(n, colors = c("#DB6400", "#272121")) {
    colorRampPalette(colors)(n)
  }
  
  # Load Country Code data with validation
  Country_code <- tryCatch({
    if (!file.exists("Country_code.xlsx")) {
      showNotification("Country_code.xlsx not found. Some features may be limited.", type = "warning")
      NULL
    } else {
      read_xlsx("Country_code.xlsx")
    }
  }, error = function(e) {
    showNotification(paste("Error loading Country_code.xlsx:", e$message), type = "error")
    NULL
  })
  
  # Session cleanup
  onSessionEnded(function() {
    # Clear reactive values and clean up resources
    gc()
  })
  
  # Sales data cleaning with error handling -----------------------------------------------------
  
  MA <- reactive({
    req(input$MA)
    
    tryCatch({
      read_excel(input$MA$datapath, sheet = "MA") %>%
        mutate(
          Start = year(Real_registration),
          Finish = year(Expiry_registration),
          Finish = if_else(is.na(Finish), year(Sys.Date()) + 2, Finish)
        ) %>%
        select(Manufacturer, Product, Country, Start, Finish) %>%
        mutate(Finish = ifelse(is.na(Finish), Start, Finish)) %>%
        rowwise() %>%
        mutate(Gregorian_year = list(Start:Finish)) %>%
        unnest(Gregorian_year) %>%
        ungroup() %>%
        select(Manufacturer, Product, Country, Gregorian_year) %>%
        distinct() %>%
        mutate(Status = "With MA Approval") %>%
        rename(Medicine = Product)
    }, error = function(e) {
      showNotification(paste("Error loading MA file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  data_1 <- reactive({
    req(input$file_1)
    
    tryCatch({
      read_excel(input$file_1$datapath, sheet = "Export") %>%
        left_join(MA(), by = c("Manufacturer", "Medicine", "Country", "Gregorian_year")) %>%
        mutate(Status = if_else(is.na(Status), "Without MA Approval", Status))
    }, error = function(e) {
      showNotification(paste("Error loading Export file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Use modularized filtering functions from server_module.R  
  filtered_data_1 <- filter_data_module_server("data_1_filter", reactive(data_1()))
  
  # UI outputs for filters using reactive values from data_1
  output$Category_ui_1 <- renderUI({
    req(data_1())
    pickerInput(
      inputId = "Category_1",
      label = "Please select the Categories",
      choices = unique(data_1()[["Category"]]),
      selected = unique(data_1()[["Category"]]),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  
  output$V_Year_ui_1 <- renderUI({
    req(data_1())
    selectInput("V_Year_1", "Please select the year variable", 
                choices = colnames(data_1()), selected = "Gregorian_year")
  })
  
  output$Year_ui_1 <- renderUI({
    req(filtered_data_1(), input$V_Year_1)
    selectInput("Year_1", "Please select the years", 
                choices = c("All years", unique(filtered_data_1()[[input$V_Year_1]])), 
                multiple = TRUE, selected = "All years")
  })
  
  output$V_Month_ui_1 <- renderUI({
    req(data_1())
    selectInput("V_Month_1", "Please select the Month variable", 
                choices = colnames(data_1()), selected = "Gregorian_month")
  })
  
  output$Month_ui_1 <- renderUI({
    req(filtered_data_1(), input$V_Month_1)
    selectInput("Month_1", "Please select the Months", 
                choices = c("All Months", unique(filtered_data_1()[[input$V_Month_1]])), 
                multiple = TRUE, selected = "All Months")
  })
  
  output$Manufacturer_ui_1 <- renderUI({
    req(filtered_data_1())
    selectInput("Manufacturer_1", "Please select the manufacturers", 
                choices = c("All manufacturers", unique(filtered_data_1()[["Manufacturer"]])), 
                multiple = TRUE, selected = "All manufacturers")
  })
  
  output$Country_ui_1 <- renderUI({
    req(filtered_data_1())
    pickerInput(
      inputId = "Country_1",
      label = "Please select the countries",
      choices = unique(filtered_data_1()[["Country"]]),
      selected = unique(filtered_data_1()[["Country"]]),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  
  output$Consignee_ui_1 <- renderUI({
    req(filtered_data_1())
    pickerInput(
      inputId = "Consignee_1",
      label = "Please select the consignees",
      choices = unique(filtered_data_1()[["Consignee"]]),
      selected = unique(filtered_data_1()[["Consignee"]]),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  
  output$Medicine_ui_1 <- renderUI({
    req(filtered_data_1())
    selectInput("Medicine_1", "Please select the medicines", 
                choices = c("All medicines", unique(filtered_data_1()[["Medicine"]])), 
                multiple = TRUE, selected = "All medicines")
  })
  
  output$Dosage_ui_1 <- renderUI({
    req(filtered_data_1())
    selectInput("Dosage_1", "Please select the dosages", 
                choices = c("All Dosages", unique(filtered_data_1()[["Dosage"]])), 
                multiple = TRUE, selected = "All Dosages")
  })
  
  # Rest of the server code remains unchanged...
}

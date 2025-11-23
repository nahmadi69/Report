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

source("server_filter_module.R")

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
  
  # Use modularized filtering for data_1
  filtered_data_1 <- export_filter_module_server("export_filter", reactive(data_1()))
  
  # UI outputs for filters
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

  # MA_1 reactive
  MA_1 <- reactive({
    req(filtered_data_1(), MA())
    
    Ma <- unique(filtered_data_1()$Manufacturer)
    Me <- unique(filtered_data_1()$Medicine)
    Co <- unique(filtered_data_1()$Country)
    Gr <- unique(filtered_data_1()$Gregorian_year)
    
    MA() %>% 
      filter(
        Manufacturer %in% Ma &
          Medicine %in% Me &
          Country %in% Co &
          Gregorian_year %in% Gr
      )
  })

  # Sales tables -----------------------------------------------------
  
  data_table_1 <- eventReactive(input$calcButton_1, {
    req(filtered_data_1())
    
    vars <- syms(c(input$V_Year_1, input$V_Month_1, "Category", "Invoice", "Manufacturer", 
                   "Country", "Consignee", "Medicine", "Dosage", "Status", "Total_Net",
                   "Total_Gross", "QTY"))
    vars <- vars[!sapply(vars, is.null)]
    
    filtered_data_1() %>%
      select(!!!vars)
  })

  output$data_1 <- renderReactable({
    req(data_table_1())
    
    reactable(
      data_table_1(),
      columns = list(
        Total_Net = colDef(
          format = colFormat(separators = TRUE, digits = 0),
          defaultSortOrder = "desc"
        ),
        Total_Gross = colDef(
          format = colFormat(separators = TRUE, digits = 0)
        ),
        QTY = colDef(
          format = colFormat(separators = TRUE, digits = 0)
        )
      ),
      pagination = TRUE,
      defaultPageSize = 15,
      pageSizeOptions = c(10, 20, 50),
      filterable = TRUE,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px"
      )
    )
  })

  output$downloadTable1 <- downloadHandler(
    filename = function() {
      req(data_table_1())
      paste("table1-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(data_table_1())
      write.csv(data_table_1(), file, row.names = FALSE)
    }
  )

  # Download handlers for aggregated data
  data_plot_Country_year <- eventReactive(input$calcButton_1, {
    req(filtered_data_1(), input$V_Year_1)
    
    filtered_data_1() %>%
      group_by(!!sym(input$V_Year_1), Country) %>%
      summarise(Total_Net = round(sum(Total_Net, na.rm = TRUE)), .groups = "drop")
  })

  output$downloadTable_country <- downloadHandler(
    filename = function() {
      req(data_plot_Country_year())
      paste("Country_year-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(data_plot_Country_year())
      write.csv(data_plot_Country_year(), file, row.names = FALSE)
    }
  )

  data_plot_Medicine_year <- eventReactive(input$calcButton_1, {
    req(filtered_data_1(), input$V_Year_1)
    
    filtered_data_1() %>%
      group_by(!!sym(input$V_Year_1), Medicine) %>%
      summarise(Total_Net = round(sum(Total_Net, na.rm = TRUE)), .groups = "drop")
  })

  output$downloadTable_medicine <- downloadHandler(
    filename = function() {
      req(data_plot_Medicine_year())
      paste("Medicine-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(data_plot_Medicine_year())
      write.csv(data_plot_Medicine_year(), file, row.names = FALSE)
    }
  )

  data_plot_Consignee_year <- eventReactive(input$calcButton_1, {
    req(filtered_data_1(), input$V_Year_1)
    
    filtered_data_1() %>%
      group_by(!!sym(input$V_Year_1), Consignee) %>%
      summarise(Total_Net = round(sum(Total_Net, na.rm = TRUE)), .groups = "drop")
  })

  output$downloadTable_consignee <- downloadHandler(
    filename = function() {
      req(data_plot_Consignee_year())
      paste("Consignee-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(data_plot_Consignee_year())
      write.csv(data_plot_Consignee_year(), file, row.names = FALSE)
    }
  )

  output$downloadTable_MA <- downloadHandler(
    filename = function() {
      req(MA_1())
      paste("MA-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(MA_1())
      write.csv(MA_1(), file, row.names = FALSE)
    }
  )

  # MA Percent Table
  percent_with_ma_country <- eventReactive(input$calcButton_1, {
    req(MA_1(), filtered_data_1())
    
    result <- filtered_data_1() %>%
      group_by(Manufacturer, Medicine, Country, Gregorian_year, Status) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop")
    
    result <- MA_1() %>%
      full_join(result, by = c("Manufacturer", "Medicine", "Country", "Gregorian_year")) %>%
      mutate(Total_Net = if_else(is.na(Total_Net), 0, Total_Net))
    
    return(result)
  })

  output$data_11 <- renderReactable({
    req(percent_with_ma_country())
    
    reactable(
      percent_with_ma_country(),
      columns = list(
        Total_Net = colDef(
          format = colFormat(separators = TRUE, digits = 0),
          defaultSortOrder = "desc"
        )
      ),
      pagination = TRUE,
      defaultPageSize = 15,
      pageSizeOptions = c(10, 20, 50),
      filterable = TRUE,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px"
      )
    )
  })

  output$downloadTable_MA_percent <- downloadHandler(
    filename = function() {
      req(percent_with_ma_country())
      paste("table2-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(percent_with_ma_country())
      write.csv(percent_with_ma_country(), file, row.names = FALSE)
    }
  )

  # Export Value Box Rendering ============================================
  
  export_card_data <- eventReactive(input$calcButton_1, {
    req(filtered_data_1())
    filtered_data_1()
  })
  
  output$total_net_export_card <- renderUI({
    df <- export_card_data()
    req(df, nrow(df) > 0)
    
    total_net <- sum(df$Total_Net, na.rm = TRUE)
    
    value_box(
      title = "Total Net Exports",
      value = scales::dollar(total_net, accuracy = 1),
      showcase = bsicons::bs_icon("cash-coin"),
      theme = "primary",
      height = "150px",
      p(HTML("&nbsp;"))
    )
  })
  
  output$top_export_country_card <- renderUI({
    df <- export_card_data()
    req(df, nrow(df) > 0)
    
    top_c <- df %>%
      group_by(Country) %>%
      summarise(total_net = sum(Total_Net, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_net)) %>%
      slice(1)
    
    req(nrow(top_c) > 0)
    
    value_box(
      title = "Top Export Country",
      value = top_c$Country,
      showcase = bsicons::bs_icon("globe-americas"),
      p(class = "small", paste("Value:", scales::dollar(top_c$total_net, accuracy = 1))),
      theme = "primary",
      height = "150px"
    )
  })
  
  output$top_export_medicine_card <- renderUI({
    df <- export_card_data()
    req(df, nrow(df) > 0)
    
    top_m <- df %>%
      group_by(Medicine) %>%
      summarise(total_net = sum(Total_Net, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_net)) %>%
      slice(1)
    
    req(nrow(top_m) > 0)
    
    value_box(
      title = "Top Exported Medicine",
      value = top_m$Medicine,
      showcase = bsicons::bs_icon("capsule"),
      p(class = "small", paste("Value:", scales::dollar(top_m$total_net, accuracy = 1))),
      theme = "primary",
      height = "150px"
    )
  })

  # (Rest of the original server code unchanged, including plotting, forecast tab, etc.)
}
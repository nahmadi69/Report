function(input, output, session) {
  
  # Your sheet ID
  SHEET_ID <- "13EYwvmP5uWQP_movX4vITdj8KtP6RiRi0WhNfJiXjVA"
  SHEET_ID_1 <- "1q95ePu8dSSw2jq_0EC8I3iKOLJUcesyMM476eheVUn8"
  gs4_deauth()
  
  # MA_base <- read_sheet(SHEET_ID)
  
  status_palette_cached <- memoise::memoise(function(num_status) {
    generate_palette(num_status, c("#DB6400", "#272121"))
  })
  
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
  
  MA <- reactive({
    tryCatch({
     read_sheet(SHEET_ID)%>%
        mutate(Status = "With MA Approval") %>% 
        select( "id"
               ,"Gregorian_year"
               ,"Medicine"                   
               ,"Dosage"                     
               ,"Manufacturer"               
               ,"Country"
               ,"Registration_date_Gregorian"
               ,"Expiry_date_Gregorian"
               ,"Expiration_status"
               ,"Project_Status"
               ,"Status"  )
    }, error = function(e) {
      showNotification(paste("Error loading MA file:", e$message), type = "error")
      return(NULL)
    })
  })
  MA_wide <- reactive({
    tryCatch({
      read_sheet(SHEET_ID_1)%>%
        mutate(Status = "With MA Approval") %>% 
        select( "id"
                ,"Medicine"                   
                ,"Dosage"                     
                ,"Manufacturer"               
                ,"Country"
                ,"Registration_date_Gregorian"
                ,"Expiry_date_Gregorian"
                ,"Expiration_status"
                ,"Project_Status"
                ,"Status"  )
    }, error = function(e) {
      showNotification(paste("Error loading MA file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Export data cleaning with error handling ----------------------------------------------------
  data_1 <- reactive({
    req(input$file_1)
    
    tryCatch({
      ex=read_excel(input$file_1$datapath, sheet = "Export") 
      ex_with_dosage <- ex %>%
        filter(!is.na(Dosage) & Dosage != "")
      
      ex_without_dosage <- ex %>%
        filter(is.na(Dosage) | Dosage == "")
      
      # 2) Join each part
      joined_with_dosage <- ex_with_dosage %>%
        left_join(
          MA(),
          by = c("Medicine", "Manufacturer", "Country", "Gregorian_year", "Dosage")
        )
      
      joined_without_dosage <- ex_without_dosage %>%
        left_join(
          MA() %>% select(-Dosage),
          by = c("Medicine", "Manufacturer", "Country", "Gregorian_year")
        )
      
      # 3) Append (stack) them again
      result <- bind_rows(joined_with_dosage, joined_without_dosage)%>% 
        # filter(!is.na(Total_Net))%>%
        relocate(Dosage, .after = Medicine) %>%
        mutate(Status = if_else(is.na(Status), "Without MA Approval", Status))
    }, error = function(e) {
      showNotification(paste("Error loading Export file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Filter Chain 1 - Category
  Category_1 <- reactive({
    req(data_1())
    unique(data_1()[["Category"]])
  })
  output$Category_ui_1 <- renderUI({
    req(Category_1())
    pickerInput(
      inputId = "Category_1",
      label = "Please select the Categories",
      choices = Category_1(),
      selected = Category_1(),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  Category_data_1 <- reactive({
    req(data_1(), input$Category_1)
    d_filtered <- data_1()
    
    all_categories <- Category_1()
    
    # If user selected a proper subset, filter; if all selected, keep full data
    if (!setequal(input$Category_1, all_categories)) {
      d_filtered <- d_filtered %>%
        dplyr::filter(Category %in% input$Category_1)
    }
    
    d_filtered
  })
  
  # Filter Chain 2 - Year Variable Selection
  output$V_Year_ui_1 <- renderUI({
    req(data_1())
    selectInput("V_Year_1", "Please select the year variable", 
                choices = colnames(data_1()), selected = "Gregorian_year")
  })
  Year_1 <- reactive({
    req(Category_data_1(), input$V_Year_1)
    unique(Category_data_1()[[input$V_Year_1]])
  })
  output$Year_ui_1 <- renderUI({
    req(Year_1(), input$V_Year_1)
    selectInput("Year_1", "Please select the years", 
                choices = c("All years", Year_1()), 
                multiple = TRUE, selected = "All years")
  })
  year_data_1 <- reactive({
    req(Category_data_1())
    d_filtered <- Category_data_1()
    
    if (!("All years" %in% input$Year_1)) {
      d_filtered <- d_filtered %>% filter(!!sym(input$V_Year_1) %in% input$Year_1)
    }
    
    return(d_filtered)
  })
  
  # Filter Chain 3 - Month Variable Selection
  output$V_Month_ui_1 <- renderUI({
    req(data_1())
    selectInput("V_Month_1", "Please select the Month variable", 
                choices = colnames(data_1()), selected = "Gregorian_month")
  })
  Month_1 <- reactive({
    req(year_data_1(), input$V_Month_1)
    d <- year_data_1() %>% arrange(!!sym(input$V_Month_1))  # Fixed: was !!!sym
    unique(d[[input$V_Month_1]])
  })
  output$Month_ui_1 <- renderUI({
    req(Month_1(), input$V_Month_1)
    selectInput("Month_1", "Please select the Months", 
                choices = c("All Months", Month_1()), 
                multiple = TRUE, selected = "All Months")
  })
  Month_data_1 <- reactive({
    req(year_data_1())
    d_filtered <- year_data_1()
    
    if (!("All Months" %in% input$Month_1)) {
      d_filtered <- d_filtered %>% filter(!!sym(input$V_Month_1) %in% input$Month_1)
    }
    
    return(d_filtered)
  })
  
  # Filter Chain 4 - Manufacturer
  Manufacturer_1 <- reactive({
    req(Month_data_1())
    unique(Month_data_1()[["Manufacturer"]])
  })
  output$Manufacturer_ui_1 <- renderUI({
    req(Manufacturer_1())
    selectInput("Manufacturer_1", "Please select the manufacturers", 
                choices = c("All manufacturers", Manufacturer_1()), 
                multiple = TRUE, selected = "All manufacturers")
  })
  manufacturer_data_1 <- reactive({
    req(Month_data_1())
    d_filtered <- Month_data_1()
    
    if (!("All manufacturers" %in% input$Manufacturer_1)) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_1)
    }
    
    return(d_filtered)
  })
  
  # Filter Chain 5 - Country
  Country_1 <- reactive({
    req(manufacturer_data_1())
    manufacturer_data_1_sorted <- manufacturer_data_1() %>% arrange(Country)
    unique(manufacturer_data_1_sorted[["Country"]])
  })
  output$Country_ui_1 <- renderUI({
    req(Country_1())
    pickerInput(
      inputId = "Country_1",
      label = "Please select the countries",
      choices = Country_1(),
      selected = Country_1(),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  country_data_1 <- reactive({
    req(manufacturer_data_1(), input$Country_1)
    d_filtered <- manufacturer_data_1()
    
    all_countries <- Country_1()
    
    # If user has selected a proper subset, filter; otherwise return full data
    if (!setequal(input$Country_1, all_countries)) {
      d_filtered <- d_filtered %>%
        dplyr::filter(Country %in% input$Country_1)
    }
    
    d_filtered
  })
  
  # Filter Chain 6 - Consignee
  Consignee_1 <- reactive({
    req(country_data_1())
    country_data_1_sorted <- country_data_1() %>% arrange(Consignee)
    unique(country_data_1_sorted[["Consignee"]])
  })
  output$Consignee_ui_1 <- renderUI({
    req(Consignee_1())
    pickerInput(
      inputId = "Consignee_1",
      label = "Please select the consignees",
      choices = Consignee_1(),
      selected = Consignee_1(),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  Consignee_data_1 <- reactive({
    req(country_data_1(), input$Consignee_1)
    d_filtered <- country_data_1()
    
    all_cons <- Consignee_1()
    
    # Only filter when a proper subset of consignees is selected
    if (!setequal(input$Consignee_1, all_cons)) {
      d_filtered <- d_filtered %>%
        dplyr::filter(Consignee %in% input$Consignee_1)
    }
    
    d_filtered
  })
  
  # Filter Chain 7 - Medicine
  Medicine_1 <- reactive({
    req(Consignee_data_1())
    unique(Consignee_data_1()[["Medicine"]])
  })
  output$Medicine_ui_1 <- renderUI({
    req(Medicine_1())
    selectInput("Medicine_1", "Please select the medicines", 
                choices = c("All medicines", Medicine_1()), 
                multiple = TRUE, selected = "All medicines")
  })
  medicine_data_1 <- reactive({
    req(Consignee_data_1())
    d_filtered <- Consignee_data_1()
    
    if (!("All medicines" %in% input$Medicine_1)) {
      d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine_1)
    }
    
    return(d_filtered)
  })
  
  # Filter Chain 8 - Dosage
  Dosage_1 <- reactive({
    req(medicine_data_1())
    unique(medicine_data_1()[["Dosage"]])
  })
  output$Dosage_ui_1 <- renderUI({
    req(Dosage_1())
    selectInput("Dosage_1", "Please select the dosages", 
                choices = c("All Dosages", Dosage_1()), 
                multiple = TRUE, selected = "All Dosages")
  })
  filtered_data_1 <- reactive({
    req(medicine_data_1())
    d_filtered <- medicine_data_1()
    
    if (!("All Dosages" %in% input$Dosage_1)) {
      d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage_1)
    }
    
    return(d_filtered)
  })
  # Export tables -----------------------------------------------------
  
  data_table_1 <- eventReactive(input$calcButton_1, {
    req(filtered_data_1())
    
    vars <- c(input$V_Year_1, input$V_Month_1, "Category", "Invoice", "Manufacturer", "Country", 
              "Consignee", "Medicine", "Dosage", "Status", "Total_Net", "Total_Gross", "QTY")
    vars <- vars[vars %in% colnames(filtered_data_1())]
    filtered_data_1() %>% select(all_of(vars))
    
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


  # Export Value Box Rendering ============================================
  
  # Create a reactive dataset that updates ONLY when the button is clicked
  net_export_data <- eventReactive(input$calcButton_1, {
    req(filtered_data_1()) # Ensure base data exists
    filtered_data_1()
  })
  
  output$total_net_export_card <- renderUI({
    # Loading State
    if (is.null(net_export_data())) {
      return(value_box(
        title = "Total Net Exports", 
        value = "Waiting...", 
        showcase = bsicons::bs_icon("hourglass-split", animation = "spin", class = "text-muted"),
        theme = "white",
        class = "shadow-sm border-start border-5 border-secondary", 
        height = "150px"
      ))
    }
    
    df <- net_export_data()
    total_net <- sum(df$Total_Net, na.rm = TRUE)
    
    value_box(
      title = "Total Net Exports",
      # Updated: Millions with M suffix
      value = tags$span(scales::dollar(total_net, accuracy = 0.01, scale = 1e-6, suffix = "M"), style = "font-size: 1.5rem;"),
      showcase = bsicons::bs_icon("wallet2", class = "text-primary"), 
      theme = "white",
      class = "shadow-sm border-start border-5 border-primary", 
      height = "150px",
      
      # Explanation
      div(class = "pt-2",
          p(class = "text-muted small mb-0", 
            "Aggregate net revenue from all exported units.")
      )
    )
  })
  
  output$top_export_country_card <- renderUI({
    
    df <- tryCatch(net_export_data(), error = function(e) NULL)
    
    if (is.null(net_export_data())) {
      return(value_box(
        title = "Total Net Exports", 
        value = "Waiting...", 
        showcase = bsicons::bs_icon("hourglass-split", animation = "spin", class = "text-muted"),
        theme = "white",
        class = "shadow-sm border-start border-5 border-secondary", 
        height = "150px"
      ))
    }
    
    top_c <- df %>%
      group_by(Country) %>%
      summarise(total_net = sum(Total_Net, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_net)) %>%
      slice(1)
    
    if (nrow(top_c) == 0) {
      return(value_box(
        title = "Top Country", value = "N/A", theme = "white", height = "150px"
      ))
    }
    
    value_box(
      title = "Top Export Country",
      value = top_c$Country,
      showcase = bsicons::bs_icon("geo-alt-fill", class = "text-info"), 
      theme = "white",
      class = "shadow-sm border-start border-5 border-info", 
      height = "150px",
      
      # Explanation: Millions with M suffix
      div(class = "pt-2",
          p(class = "text-muted small mb-0", 
            paste0("Highest revenue market generating ", 
                   scales::dollar(top_c$total_net, accuracy = 0.01, scale = 1e-6, suffix = "M"), "."))
      )
    )
  })
  
  output$top_export_medicine_card <- renderUI({
    df <- tryCatch(net_export_data(), error = function(e) NULL)
    
    if (is.null(net_export_data())) {
      return(value_box(
        title = "Total Net Exports", 
        value = "Waiting...", 
        showcase = bsicons::bs_icon("hourglass-split", animation = "spin", class = "text-muted"),
        theme = "white",
        class = "shadow-sm border-start border-5 border-secondary", 
        height = "150px"
      ))
    }
    
    top_m <- df %>%
      group_by(Medicine) %>%
      summarise(total_net = sum(Total_Net, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_net)) %>%
      slice(1)
    
    if (nrow(top_m) == 0) {
      return(value_box(title = "Top Medicine", value = "N/A", theme = "white", height = "150px"))
    }
    
    value_box(
      title = "Top Exported Medicine",
      value = top_m$Medicine,
      showcase = bsicons::bs_icon("capsule-pill", class = "text-warning"), 
      theme = "white",
      class = "shadow-sm border-start border-5 border-warning", 
      height = "150px",
      
      # Explanation: Millions with M suffix
      div(class = "pt-2",
          p(class = "text-muted small mb-0", 
            paste0("Leading product contributing ", 
                   scales::dollar(top_m$total_net, accuracy = 0.01, scale = 1e-6, suffix = "M"), 
                   " to sales."))
      )
    )
  })
  

  # Export plots -----------------------------------------------------
  
  data_plot_year <- eventReactive(input$calcButton_1, {
    req(filtered_data_1(), input$V_Year_1)
    
    filtered_data_1() %>%
      group_by(!!sym(input$V_Year_1), Manufacturer) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop") %>%
      filter(Manufacturer != "Arvand Pharmed")
  })
  
  output$plot_year <- renderPlotly({
    req(data_plot_year(), input$V_Year_1)
    
    data_to_plot <- data_plot_year() %>%
      mutate(x_year = factor(!!sym(input$V_Year_1)))
    
    num_years <- n_distinct(data_to_plot$x_year)
    professional_palette <- generate_palette(num_years, c("#272121", "#DB6400"))
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Manufacturer,
      y = ~Total_Net,
      color = ~x_year,
      colors = professional_palette,
      type = 'bar',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      hovertemplate = paste0(
        '%{x}<br>',
        'Year: %{fullData.name}<br>',
        'Total Net: %{y:$,.0f}<extra></extra>'
      )
    ) %>%
      layout(
        title = "Total Net by Manufacturer and Year",
        xaxis = list(title = "Manufacturer", tickangle = 0),
        yaxis = list(
          title = "Total Net",
          range = c(0, max(data_to_plot$Total_Net, na.rm = TRUE) * 1.15)
        ),
        barmode = 'group',
        legend = list(title = list(text = 'Year')),
        margin = list(b = 150)
      )
    
    p
  })
  
  output$table_year <- DT::renderDT({
    req(data_plot_year(), input$V_Year_1)
    
    # Get data
    df <- data_plot_year()
    
    # Identify the dynamic year column name for the header
    year_col_name <- input$V_Year_1
    
    DT::datatable(
      df,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        autoWidth = TRUE
      ),
      # Use dynamic column names to match your input selection
      colnames = c(year_col_name, "Manufacturer", "Total Net") 
    ) %>%
      DT::formatCurrency("Total_Net", currency = "$", digits = 0)
  })
  
  # Country Plot
  data_plot_Country <- eventReactive(input$calcButton_1, {
    req(filtered_data_1())
    
    data_temp <- filtered_data_1() %>%
      group_by(Country) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Net))
    
    top_9_countries <- data_temp %>% slice_head(n = 9)
    other_countries <- data_temp %>%
      slice_tail(n = max(0, nrow(data_temp) - 9)) %>%
      summarise(Country = "Other Country", Total_Net = sum(Total_Net, na.rm = TRUE))
    
    data_plot_Country <- bind_rows(top_9_countries, other_countries)
    data_plot_Country$Country <- factor(data_plot_Country$Country, levels = data_plot_Country$Country)
    
    return(data_plot_Country)
  })
  
  output$plot_Country <- renderPlotly({
    req(data_plot_Country())
    
    data_to_plot <- data_plot_Country()
    
    top_for_colors <- data_to_plot %>%
      filter(Country != "Other Country") %>%
      arrange(desc(Total_Net))
    
    num_countries <- nrow(data_to_plot)
    full_palette <- generate_palette(num_countries, c("#DB6400", "#272121"))
    
    top_colors <- head(full_palette, -1)
    other_color <- tail(full_palette, 1)
    
    color_map <- c(
      setNames(top_colors, top_for_colors$Country),
      "Other Country" = other_color
    )
    
    data_to_plot$bar_colors <- color_map[as.character(data_to_plot$Country)]
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Country,
      y = ~Total_Net,
      type = 'bar',
      marker = list(color = ~bar_colors),
      hovertemplate = '%{x}<br>Total Net: %{y:$,.0f}<extra></extra>',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      textfont = list(color = ~bar_colors)
    ) %>%
      layout(
        title = "Total Net by Country",
        xaxis = list(title = "", categoryorder = "array", categoryarray = ~Country),
        yaxis = list(title = "Total Net")
      )
    
    p
  })
  
  output$table_Country <- DT::renderDT({
    # Use raw filtered data to show ALL countries in the table
    req(filtered_data_1())
    
    # Calculate Full List (No "Other Country" grouping)
    df <- filtered_data_1() %>%
      group_by(Country) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Net))
    
    DT::datatable(
      df,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        autoWidth = TRUE
      ),
      colnames = c("Country", "Total Net")
    ) %>%
      DT::formatCurrency("Total_Net", currency = "$", digits = 0)
  })
  
  # Consignee Plot
  data_plot_Consignee <- eventReactive(input$calcButton_1, {
    req(filtered_data_1())
    
    data_temp <- filtered_data_1() %>%
      group_by(Consignee) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Net))
    
    top_9_consignees <- data_temp %>% slice_head(n = 9)
    other_consignees <- data_temp %>%
      slice_tail(n = max(0, nrow(data_temp) - 9)) %>%
      summarise(Consignee = "Other Consignees", Total_Net = sum(Total_Net, na.rm = TRUE))
    
    data_plot_Consignee <- bind_rows(top_9_consignees, other_consignees)
    data_plot_Consignee$Consignee <- factor(data_plot_Consignee$Consignee, levels = data_plot_Consignee$Consignee)
    
    return(data_plot_Consignee)
  })
  
  output$plot_Consignee <- renderPlotly({
    req(data_plot_Consignee())
    
    data_to_plot <- data_plot_Consignee()
    
    top_for_colors <- data_to_plot %>%
      filter(Consignee != "Other Consignees") %>%
      arrange(desc(Total_Net))
    
    num_consignees <- nrow(data_to_plot)
    full_palette <- generate_palette(num_consignees, c("#DB6400", "#272121"))
    
    top_colors <- head(full_palette, -1)
    other_color <- tail(full_palette, 1)
    
    color_map <- c(
      setNames(top_colors, top_for_colors$Consignee),
      "Other Consignees" = other_color
    )
    
    data_to_plot$bar_colors <- color_map[as.character(data_to_plot$Consignee)]
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Consignee,
      y = ~Total_Net,
      type = 'bar',
      marker = list(color = ~bar_colors),
      hovertemplate = '%{x}<br>Total Net: %{y:$,.0f}<extra></extra>',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      textfont = list(color = ~bar_colors)
    ) %>%
      layout(
        title = "Total Net by Consignee",
        xaxis = list(title = "", categoryorder = "array", categoryarray = ~Consignee),
        yaxis = list(title = "Total Net")
      )
    
    p
  })
  
  output$table_Consignee <- DT::renderDT({
    # Use the raw filtered data, NOT the plot data (so we don't get "Other Consignees")
    req(filtered_data_1())
    
    # Calculate Full List
    df <- filtered_data_1() %>%
      group_by(Consignee) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Net)) # Sort highest to lowest
    
    DT::datatable(
      df,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        autoWidth = TRUE
      ),
      colnames = c("Consignee", "Total Net")
    ) %>%
      DT::formatCurrency("Total_Net", currency = "$", digits = 0)
  })
  
  # Category Plot
  data_plot_Category <- eventReactive(input$calcButton_1, {
    req(filtered_data_1())
    
    filtered_data_1() %>%
      group_by(Category) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop")
  })
  
  output$plot_Category <- renderPlotly({
    req(data_plot_Category())
    
    data_to_plot <- data_plot_Category() %>%
      arrange(desc(Total_Net))
    
    num_categories <- nrow(data_to_plot)
    sorted_palette <- generate_palette(num_categories, c("#DB6400", "#272121"))
    
    data_to_plot$bar_colors <- sorted_palette
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Category,
      y = ~Total_Net,
      type = 'bar',
      marker = list(color = ~bar_colors),
      hovertemplate = '%{x}<br>Total Net: %{y:$,.0f}<extra></extra>',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      textfont = list(color = ~bar_colors)
    ) %>%
      layout(
        title = "Total Net by Category",
        xaxis = list(title = "", categoryorder = "array", categoryarray = ~Category),
        yaxis = list(title = "Total Net")
      )
    
    p
  })
  
  output$table_Category <- DT::renderDT({
    req(data_plot_Category())
    
    # Get data
    df <- data_plot_Category() %>%
      arrange(desc(Total_Net)) # Ensure table is sorted by value
    
    DT::datatable(
      df,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        autoWidth = TRUE
      ),
      colnames = c("Category", "Total Net")
    ) %>%
      DT::formatCurrency("Total_Net", currency = "$", digits = 0)
  })
  
  
  # Medicine Plot
  data_plot_Medicine <- eventReactive(input$calcButton_1, {
    req(filtered_data_1())
    
    data_temp <- filtered_data_1() %>%
      group_by(Medicine) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Net))
    
    top_9_medicines <- data_temp %>% slice_head(n = 9)
    other_medicines <- data_temp %>%
      slice_tail(n = max(0, nrow(data_temp) - 9)) %>%
      summarise(Medicine = "Other Products", Total_Net = sum(Total_Net, na.rm = TRUE))
    
    data_plot_Medicine <- bind_rows(top_9_medicines, other_medicines)
    data_plot_Medicine$Medicine <- factor(data_plot_Medicine$Medicine, levels = data_plot_Medicine$Medicine)
    
    return(data_plot_Medicine)
  })
  
  output$plot_Medicine <- renderPlotly({
    req(data_plot_Medicine())
    
    data_to_plot <- data_plot_Medicine()
    
    top_for_colors <- data_to_plot %>%
      filter(Medicine != "Other Products") %>%
      arrange(desc(Total_Net))
    
    num_medicines <- nrow(data_to_plot)
    full_palette <- generate_palette(num_medicines, c("#DB6400", "#272121"))
    
    top_colors <- head(full_palette, -1)
    other_color <- tail(full_palette, 1)
    
    color_map <- c(
      setNames(top_colors, top_for_colors$Medicine),
      "Other Products" = other_color
    )
    
    data_to_plot$bar_colors <- color_map[as.character(data_to_plot$Medicine)]
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Medicine,
      y = ~Total_Net,
      type = 'bar',
      marker = list(color = ~bar_colors),
      hovertemplate = '%{x}<br>Total Net: %{y:$,.0f}<extra></extra>',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      textfont = list(color = ~bar_colors)
    ) %>%
      layout(
        title = "Total Net by Medicine",
        yaxis = list(title = "Total Net"),
        xaxis = list(title = "", categoryorder = "array", categoryarray = ~Medicine)
      )
    
    p
  })
  
  output$table_Medicine <- DT::renderDT({
    # Use raw filtered data to show ALL medicines in the table
    req(filtered_data_1())
    
    # Calculate Full List
    df <- filtered_data_1() %>%
      group_by(Medicine) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Net))
    
    DT::datatable(
      df,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        autoWidth = TRUE
      ),
      colnames = c("Medicine", "Total Net")
    ) %>%
      DT::formatCurrency("Total_Net", currency = "$", digits = 0)
  })
  
  # Heat Map - New Exports Timeline
  data_heat_Map <- eventReactive(input$calcButton_1, {
    req(filtered_data_1(), input$V_Year_1)
    
    exported_data <- filtered_data_1() %>%
      filter(New_Export == "Yes") %>%
      select(Country, Medicine, !!sym(input$V_Year_1)) %>%
      rename("Year" = input$V_Year_1)
    
    timeline_data <- exported_data %>%
      group_by(Country, Year) %>%
      summarise(
        Exported_Medicines = paste(Medicine, collapse = "<br>"),
        .groups = 'drop'
      )
    
    return(timeline_data)
  })
  
  output$plot_heat_map <- renderPlotly({
    req(data_heat_Map())
    
    plot_data <- data_heat_Map()
    
    p <- plot_ly(
      data = plot_data,
      x = ~Year,
      y = ~Country,
      type = 'scatter',
      mode = 'markers',
      hovertemplate = paste(
        '%{y} - %{x}<br>',
        '------------------<br>',
        'New Exports:<br>',
        '%{customdata}<extra></extra>'
      ),
      customdata = ~Exported_Medicines,
      marker = list(
        color = '#DB6400',
        size = 15,
        symbol = 'circle',
        line = list(color = 'black', width = 1)
      )
    ) %>%
      layout(
        title = "Timeline of New Medicine Exports by Country",
        xaxis = list(
          title = "Year of First Export",
          dtick = 1,
          gridcolor = '#e9e9e9'
        ),
        yaxis = list(
          title = "",
          categoryorder = "total ascending",
          gridcolor = '#e9e9e9'
        ),
        plot_bgcolor = '#fdfdfd'
      )
    
    p
  })
  
  # Forecast data cleaning -----------------------------------------------------------------
  
  d_sales <- reactive({
    req(input$file_sales)
    
    tryCatch({
      read_excel(input$file_sales$datapath, sheet = "Export")
    }, error = function(e) {
      showNotification(paste("Error loading sales file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  d_forcast <- reactive({
    req(input$file_forcast)
    
    tryCatch({
      read_excel(input$file_forcast$datapath, sheet = "Forcast")
    }, error = function(e) {
      showNotification(paste("Error loading forecast file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$Probability_ui_2 <- renderUI({
    req(d_forcast())
    
    min_prob <- min(d_forcast()$Probability, na.rm = TRUE)
    max_prob <- max(d_forcast()$Probability, na.rm = TRUE)
    
    sliderInput("Probability", "Please select range of probability",
                value = c(min_prob, max_prob), min = min_prob, max = max_prob, step = 0.01)
  })
  
  Probability_forcast_2 <- reactive({
    req(input$Probability, d_forcast())
    
    d_forcast() %>%
      filter(Probability >= input$Probability[1] & Probability <= input$Probability[2])
  })
  
  Category_2 <- reactive({
    req(Probability_forcast_2())
    unique(Probability_forcast_2()[["Category"]])
  })
  
  output$Category_ui_2 <- renderUI({
    req(Category_2())
    pickerInput(
      inputId = "Category_2",
      label = "Please select the Categories",
      choices = Category_2(),
      selected = Category_2(),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  
  Category_forcast_2 <- reactive({
    req(Probability_forcast_2())
    d_filtered <- Probability_forcast_2()
    
    if (!("All Categories" %in% input$Category_2)) {
      d_filtered <- d_filtered %>% filter(Category %in% input$Category_2)
    }
    
    return(d_filtered)
  })
  
  Category_sales_2 <- reactive({
    req(d_sales())
    d_filtered <- d_sales()
    
    if (!("All Categories" %in% input$Category_2)) {
      d_filtered <- d_filtered %>% filter(Category %in% input$Category_2)
    }
    
    return(d_filtered)
  })
  
  output$V_Year_ui_2 <- renderUI({
    req(d_forcast())
    selectInput("V_Year_2", "Please select the year variable", 
                choices = colnames(d_forcast()), selected = "Gregorian_year")
  })
  
  Year_2 <- reactive({
    req(Category_forcast_2(), input$V_Year_2)
    unique(Category_forcast_2()[[input$V_Year_2]])
  })
  
  output$Year_ui_2 <- renderUI({
    req(Year_2())
    selectInput("Year_2", "Please select the years", 
                choices = c("All years", Year_2()), 
                multiple = TRUE, selected = "All years")
  })
  
  year_forcast_2 <- reactive({
    req(Category_forcast_2())
    d_filtered <- Category_forcast_2()
    
    if (!("All years" %in% input$Year_2)) {
      d_filtered <- d_filtered %>% filter(!!sym(input$V_Year_2) %in% input$Year_2)
    }
    
    return(d_filtered)
  })
  
  year_sales_2 <- reactive({
    req(Category_sales_2())
    d_filtered <- Category_sales_2()
    
    if (!("All years" %in% input$Year_2)) {
      d_filtered <- d_filtered %>% filter(!!sym(input$V_Year_2) %in% input$Year_2)
    }
    
    return(d_filtered)
  })
  
  Manufacturer_2 <- reactive({
    req(year_forcast_2())
    unique(year_forcast_2()[["Manufacturer"]])
  })
  
  output$Manufacturer_ui_2 <- renderUI({
    req(Manufacturer_2())
    selectInput("Manufacturer_2", "Please select the manufacturers", 
                choices = c("All manufacturers", Manufacturer_2()), 
                multiple = TRUE, selected = "All manufacturers")
  })
  
  manufacturer_forcast_2 <- reactive({
    req(year_forcast_2())
    d_filtered <- year_forcast_2()
    
    if (!("All manufacturers" %in% input$Manufacturer_2)) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_2)
    }
    
    return(d_filtered)
  })
  
  manufacturer_sales_2 <- reactive({
    req(year_sales_2())
    d_filtered <- year_sales_2()
    
    if (!("All manufacturers" %in% input$Manufacturer_2)) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_2)
    }
    
    return(d_filtered)
  })
  
  Country_2 <- reactive({
    req(manufacturer_forcast_2())
    manufacturer_forcast_2_sorted <- manufacturer_forcast_2() %>% arrange(Country)
    unique(manufacturer_forcast_2_sorted[["Country"]])
  })
  
  output$Country_ui_2 <- renderUI({
    req(Country_2())
    pickerInput(
      inputId = "Country_2",
      label = "Please select the countries",
      choices = Country_2(),
      selected = Country_2(),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  
  country_forcast_2 <- reactive({
    req(manufacturer_forcast_2())
    d_filtered <- manufacturer_forcast_2()
    
    if (!("All countries" %in% input$Country_2)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_2)
    }
    
    return(d_filtered)
  })
  
  country_sales_2 <- reactive({
    req(manufacturer_sales_2())
    d_filtered <- manufacturer_sales_2()
    
    if (!("All countries" %in% input$Country_2)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_2)
    }
    
    return(d_filtered)
  })
  
  Medicine_2 <- reactive({
    req(country_forcast_2())
    unique(country_forcast_2()[["Medicine"]])
  })
  
  output$Medicine_ui_2 <- renderUI({
    req(Medicine_2())
    selectInput("Medicine_2", "Please select the medicines", 
                choices = c("All medicines", Medicine_2()), 
                multiple = TRUE, selected = "All medicines")
  })
  
  medicine_forcast_2 <- reactive({
    req(country_forcast_2())
    d_filtered <- country_forcast_2()
    
    if (!("All medicines" %in% input$Medicine_2)) {
      d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine_2)
    }
    
    return(d_filtered)
  })
  
  medicine_sales_2 <- reactive({
    req(country_sales_2())
    d_filtered <- country_sales_2()
    
    if (!("All medicines" %in% input$Medicine_2)) {
      d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine_2)
    }
    
    return(d_filtered)
  })
  
  Dosage_2 <- reactive({
    req(medicine_forcast_2())
    unique(medicine_forcast_2()[["Dosage"]])
  })
  
  output$Dosage_ui_2 <- renderUI({
    req(Dosage_2())
    selectInput("Dosage_2", "Please select the dosages", 
                choices = c("All Dosages", Dosage_2()), 
                multiple = TRUE, selected = "All Dosages")
  })
  
  filtered_forcast_2 <- reactive({
    req(medicine_forcast_2())
    d_filtered <- medicine_forcast_2()
    
    if (!("All Dosages" %in% input$Dosage_2)) {
      d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage_2)
    }
    
    return(d_filtered)
  })
  
  filtered_sales_2 <- reactive({
    req(medicine_sales_2())
    d_filtered <- medicine_sales_2()
    
    if (!("All Dosages" %in% input$Dosage_2)) {
      d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage_2)
    }
    
    return(d_filtered)
  })
  
  
  # Combine forecast and sales data
  forcast_target_Net <- eventReactive(input$calcButton_2, {
    req(filtered_forcast_2(), filtered_sales_2())
    
    d_forcast_filtered <- filtered_forcast_2() %>%
      mutate(Weighted = Value * Probability) %>%
      group_by(!!sym(input$V_Year_2), Manufacturer, Medicine, Dosage, Country) %>%
      summarise(
        Weighted = sum(Weighted, na.rm = TRUE),
        Value = sum(Value, na.rm = TRUE),
        .groups = "drop"
      )
    
    d_sales_filtered <- filtered_sales_2() %>%
      group_by(!!sym(input$V_Year_2), Manufacturer, Medicine, Dosage, Country) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop")
    
    result <- d_forcast_filtered %>%
      full_join(d_sales_filtered, by = c(input$V_Year_2, "Manufacturer", "Medicine", "Dosage", "Country")) %>%
      mutate(
        Total_Net = if_else(is.na(Total_Net), 0, Total_Net),
        Weighted = if_else(is.na(Weighted), 0, Weighted),
        Percent_Weighted = if_else(
          Weighted == 0,
          0,
          round((Total_Net / Weighted) * 100, 2)
        )
      )
    
    return(result)
  })
  
  # Forecast Value Boxes ---------------------------------------------------------------

  forecast_card_data <- eventReactive(input$calcButton_2, {
    req(forcast_target_Net())
    forcast_target_Net()
  })
  
  output$forecast_card <- renderUI({
    df <- forecast_card_data()
    req(df, nrow(df) > 0)
    
    total_forecast <- sum(df$Weighted, na.rm = TRUE)
    
    value_box(
      title = "Total Forecast",
      # Updated: Scale by 1e-6 (million) and add "M" suffix
      value = scales::dollar(total_forecast, accuracy = 0.01, scale = 1e-6, suffix = "M"),
      showcase = bsicons::bs_icon("graph-up-arrow", class = "text-primary"), 
      theme = "white",
      class = "shadow-sm border-start border-5 border-primary",
      
      # Explanation Section
      div(class = "pt-2",
          p(class = "text-muted small mb-0", 
            "Risk-adjusted revenue projection based on weighted probabilities.")
      )
    )
  })
  
  output$actual_card <- renderUI({
    df <- forecast_card_data()
    req(df, nrow(df) > 0)
    
    total_actual <- sum(df$Total_Net, na.rm = TRUE)
    total_forecast <- sum(df$Weighted, na.rm = TRUE)
    
    # Calculate Variance/Achievement
    pct_achieved <- if(total_forecast > 0) (total_actual / total_forecast) else 0
    
    value_box(
      title = "Actual Revenue",
      # Main Value: Millions with M suffix
      value = scales::dollar(total_actual, accuracy = 0.01, scale = 1e-6, suffix = "M"),
      showcase = bsicons::bs_icon("cash-stack", class = "text-success"), 
      theme = "white",
      class = "shadow-sm border-start border-5 border-success", 
      
      # Explanation with Achievement Metric
      div(class = "pt-2",
          p(class = "text-muted small mb-0", 
            paste0("Realized net sales. Current achievement: ", 
                   scales::percent(pct_achieved, accuracy = 0.1), 
                   " of target."))
      )
    )
  })
  
  output$top_country_card <- renderUI({
    df <- forecast_card_data()
    req(df, nrow(df) > 0)
    
    top_c <- df %>%
      group_by(Country) %>%
      summarise(total_weighted = sum(Weighted, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_weighted)) %>%
      slice(1)
    
    req(nrow(top_c) > 0)
    
    value_box(
      title = "Top Market",
      value = top_c$Country,
      showcase = bsicons::bs_icon("globe-americas", class = "text-info"), 
      theme = "white",
      class = "shadow-sm border-start border-5 border-info", 
      
      # Explanation: Millions with M suffix
      div(class = "pt-2",
          p(class = "text-muted small mb-0", 
            paste("Single largest market contributor, projecting", 
                  scales::dollar(top_c$total_weighted, accuracy = 0.01, scale = 1e-6, suffix = "M"), 
                  "in value."))
      )
    )
  })
  
  output$top_medicine_card <- renderUI({
    df <- forecast_card_data()
    req(df, nrow(df) > 0)
    
    top_m <- df %>%
      group_by(Medicine) %>%
      summarise(total_weighted = sum(Weighted, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_weighted)) %>%
      slice(1)
    
    req(nrow(top_m) > 0)
    
    value_box(
      title = "Key Product",
      value = top_m$Medicine,
      showcase = bsicons::bs_icon("capsule", class = "text-warning"), 
      theme = "white",
      class = "shadow-sm border-start border-5 border-warning", 
      
      # Explanation: Millions with M suffix
      div(class = "pt-2",
          p(class = "text-muted small mb-0", 
            paste("Leading SKU by volume, accounting for", 
                  scales::dollar(top_m$total_weighted, accuracy = 0.01, scale = 1e-6, suffix = "M"), 
                  "of forecast."))
      )
    )
  })
  
  
  # Forecast plots ---------------------------------------------------------------
  
  data_plot_Year_Forcast <- eventReactive(input$calcButton_2, {
    req(forcast_target_Net(), input$V_Year_2)
    
    forcast_target_Net() %>%
      group_by(!!sym(input$V_Year_2)) %>%
      summarise(
        Total_Net = sum(Total_Net, na.rm = TRUE),
        Forcast = sum(Weighted, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      gather(type, out, -!!sym(input$V_Year_2)) %>%
      arrange(!!sym(input$V_Year_2))
  })
  
  output$plot_Year_Forcast <- renderPlotly({
    req(data_plot_Year_Forcast(), input$V_Year_2)
    
    df <- data_plot_Year_Forcast()
    year_col <- sym(input$V_Year_2)
    
    df_to_plot <- df %>%
      mutate(x_year = factor(!!year_col))
    
    num_types <- n_distinct(df_to_plot$type)
    professional_palette <- generate_palette(num_types, c("#272121", "#FF7315"))
    
    p <- plot_ly(
      data = df_to_plot,
      x = ~x_year,
      y = ~out,
      color = ~type,
      colors = professional_palette,
      type = 'bar',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      hovertemplate = paste0(
        'Year: %{x}<br>',
        '%{fullData.name}: %{y:$,.2f}<extra></extra>'
      )
    ) %>%
      layout(
        title = "Total Net vs. Forecast by Year",
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "Amount",
          range = c(0, max(df_to_plot$out, na.rm = TRUE) * 1.15)
        ),
        barmode = 'group',
        legend = list(title = list(text = ''))
      )
    
    p
  })
  
  data_plot_Manufacturer_Forcast <- eventReactive(input$calcButton_2, {
    req(forcast_target_Net())
    
    forcast_target_Net() %>%
      group_by(Manufacturer) %>%
      summarise(
        Total_Net = sum(Total_Net, na.rm = TRUE),
        Forcast = sum(Weighted, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      gather(type, out, -Manufacturer)
  })
  
  output$plot_Manufacturer_Forcast <- renderPlotly({
    req(data_plot_Manufacturer_Forcast())
    
    df <- data_plot_Manufacturer_Forcast()
    
    manufacturer_order <- df %>%
      group_by(Manufacturer) %>%
      summarise(total_out = sum(out, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_out)) %>%
      pull(Manufacturer)
    
    num_types <- n_distinct(df$type)
    professional_palette <- generate_palette(num_types, c("#272121", "#FF7315"))
    
    p <- plot_ly(
      data = df,
      x = ~Manufacturer,
      y = ~out,
      color = ~type,
      colors = professional_palette,
      type = 'bar',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      hovertemplate = paste0(
        'Manufacturer: %{x}<br>',
        '%{fullData.name}: %{y:$,.2f}<extra></extra>'
      )
    ) %>%
      layout(
        title = "Total Net vs. Forecast by Manufacturer",
        xaxis = list(
          title = "Manufacturer",
          categoryorder = "array",
          categoryarray = manufacturer_order,
          tickangle = -45
        ),
        yaxis = list(
          title = "Amount",
          range = c(0, max(df$out, na.rm = TRUE) * 1.15)
        ),
        barmode = 'group',
        legend = list(title = list(text = '')),
        margin = list(b = 150)
      )
    
    p
  })
  
  data_plot_Medicine_Forcast <- eventReactive(input$calcButton_2, {
    req(forcast_target_Net())
    
    data_temp <- forcast_target_Net() %>%
      group_by(Medicine) %>%
      summarise(
        Total_Net = sum(Total_Net, na.rm = TRUE),
        Forecast = sum(Weighted, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Net))
    
    top_9_medicines <- data_temp %>% slice_head(n = 9)
    other_medicines <- data_temp %>%
      slice_tail(n = max(0, nrow(data_temp) - 9)) %>%
      summarise(
        Medicine = "Other medicine",
        Total_Net = sum(Total_Net),
        Forecast = sum(Forecast)
      )
    
    combined_df <- bind_rows(top_9_medicines, other_medicines)
    
    long_df <- combined_df %>%
      tidyr::pivot_longer(cols = c("Total_Net", "Forecast"), names_to = "type", values_to = "out")
    
    long_df$Medicine <- factor(long_df$Medicine, levels = c(top_9_medicines$Medicine, "Other medicine"))
    
    long_df
  })
  
  output$plot_Medicine_Forcast <- renderPlotly({
    req(data_plot_Medicine_Forcast())
    
    df <- data_plot_Medicine_Forcast()
    
    medicine_order <- levels(df$Medicine)
    num_types <- n_distinct(df$type)
    professional_palette <- generate_palette(num_types, c("#272121", "#FF7315"))
    
    p <- plot_ly(
      data = df,
      x = ~Medicine,
      y = ~out,
      color = ~type,
      colors = professional_palette,
      type = 'bar',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      hovertemplate = paste0(
        'Medicine: %{x}<br>',
        '%{fullData.name}: %{y:$,.2f}<extra></extra>'
      )
    ) %>%
      layout(
        title = "Total Net vs. Forecast by Medicine",
        xaxis = list(title = "Medicine", categoryorder = "array", categoryarray = medicine_order, tickangle = -45),
        yaxis = list(title = "Amount", range = c(0, max(df$out, na.rm = TRUE) * 1.15)),
        barmode = 'group',
        legend = list(title = list(text = '')),
        margin = list(b = 150)
      )
    
    p
  })
  
  data_plot_Country_Forcast <- eventReactive(input$calcButton_2, {
    req(forcast_target_Net())
    
    data_temp <- forcast_target_Net() %>%
      group_by(Country) %>%
      summarise(
        Total_Net = sum(Total_Net, na.rm = TRUE),
        Forecast = sum(Weighted, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Net))
    
    top_9_countries <- data_temp %>% slice_head(n = 9)
    other_countries <- data_temp %>%
      slice_tail(n = max(0, nrow(data_temp) - 9)) %>%
      summarise(
        Country = "Other country",
        Total_Net = sum(Total_Net),
        Forecast = sum(Forecast)
      )
    
    combined_df <- bind_rows(top_9_countries, other_countries)
    
    long_df <- combined_df %>%
      tidyr::pivot_longer(cols = c("Total_Net", "Forecast"), names_to = "type", values_to = "out")
    
    long_df$Country <- factor(long_df$Country, levels = c(top_9_countries$Country, "Other country"))
    
    long_df
  })
  
  output$plot_Country_Forcast <- renderPlotly({
    req(data_plot_Country_Forcast())
    
    df <- data_plot_Country_Forcast()
    
    country_order <- levels(df$Country)
    num_types <- n_distinct(df$type)
    professional_palette <- generate_palette(num_types, c("#272121", "#FF7315"))
    
    p <- plot_ly(
      data = df,
      x = ~Country,
      y = ~out,
      color = ~type,
      colors = professional_palette,
      type = 'bar',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      hovertemplate = paste0(
        'Country: %{x}<br>',
        '%{fullData.name}: %{y:$,.2f}<extra></extra>'
      )
    ) %>%
      layout(
        title = "Total Net vs. Forecast by Country",
        xaxis = list(title = "Country", categoryorder = "array", categoryarray = country_order, tickangle = -45),
        yaxis = list(title = "Amount", range = c(0, max(df$out, na.rm = TRUE) * 1.15)),
        barmode = 'group',
        legend = list(title = list(text = '')),
        margin = list(b = 150)
      )
    
    p
  })
  
  # Forecast tables ---------------------------------------------------------------
  
  output$data_2 <- renderReactable({
    req(forcast_target_Net())
    
    data <- forcast_target_Net() %>%
      mutate(Remain = if_else(Total_Net > Weighted, 0, Weighted - Total_Net)) %>%
      arrange(desc(Remain))
    
    reactable(
      data,
      pagination = TRUE,
      defaultPageSize = 15,
      pageSizeOptions = c(10, 20, 50),
      filterable = TRUE,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      columns = list(
        Total_Net = colDef(
          format = colFormat(separators = TRUE, digits = 0),
          defaultSortOrder = "desc"
        ),
        Weighted = colDef(
          format = colFormat(separators = TRUE, digits = 0),
          defaultSortOrder = "desc"
        ),
        Remain = colDef(
          format = colFormat(separators = TRUE, digits = 0),
          defaultSortOrder = "desc"
        ),
        Value = colDef(
          format = colFormat(separators = TRUE, digits = 0),
          defaultSortOrder = "desc"
        ),
        Percent_Weighted = colDef(
          name = "Achievement %",
          style = function(value) {
            numeric_value <- as.numeric(value)
            if (is.na(numeric_value)) return(list(background = "white"))
            
            normalized <- (numeric_value - 50) / 50
            color <- scales::col_numeric("RdYlGn", domain = c(-1, 1))(normalized)
            
            list(
              background = color, 
              color = if (numeric_value < 20 || numeric_value > 90) "white" else "black"
            )
          },
          format = colFormat(suffix = "%")
        )
      ),
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px"
      )
    )
  })
  
  output$downloadTable2 <- downloadHandler(
    filename = function() {
      req(forcast_target_Net())
      paste("Forecast-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(forcast_target_Net())
      data <- forcast_target_Net() %>%
        mutate(Remain = if_else(Total_Net > Weighted, 0, Weighted - Total_Net)) %>%
        arrange(desc(Remain))
      write.csv(data, file, row.names = FALSE)
    }
  )

  # MA cleaning -------------------------------------------------------------
  data_MA <- reactive({
    req(input$file_MA)
    
    tryCatch({
      ex=read_excel(input$file_MA$datapath, sheet = "Export") %>% 
        select(Gregorian_year,Category,Country,Consignee,Manufacturer,Medicine,Dosage,Total_Net)
      ex_with_dosage <- ex %>%
        filter(!is.na(Dosage) & Dosage != "")
      
      ex_without_dosage <- ex %>%
        filter(is.na(Dosage) | Dosage == "")
      
      # 2) Join each part
      joined_with_dosage <- ex_with_dosage %>%
        full_join(
          MA(),
          by = c("Medicine", "Manufacturer", "Country", "Gregorian_year", "Dosage")
        )
      
      joined_without_dosage <- ex_without_dosage %>%
        full_join(
          MA() %>% select(-Dosage),
          by = c("Medicine", "Manufacturer", "Country", "Gregorian_year")
        )
      
      # 3) Append (stack) them again
      result <- bind_rows(joined_with_dosage, joined_without_dosage)%>%
        relocate(Dosage, .after = Medicine) %>%
        mutate(Status = if_else(is.na(Status), "Without MA Approval", Status))
    }, error = function(e) {
      showNotification(paste("Error loading Export file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  Year_MA <- reactive({
    req(data_MA())
    unique(data_MA()[["Gregorian_year"]])
  })
  output$Year_ui_MA <- renderUI({
    req(Year_MA())
    selectInput("Year_MA", "Please select the years", 
                choices = c("All years", Year_MA()), 
                multiple = TRUE, selected = "All years")
  })
  year_data_MA <- reactive({
    req(data_MA())
    d_filtered <- data_MA()
    
    if (!("All years" %in% input$Year_MA)) {
      d_filtered <- d_filtered %>% filter(Gregorian_year %in% input$Year_MA)
    }
    
    return(d_filtered)
  })
  MA_year <- reactive({
    req(MA())
    d_filtered <- MA()
    
    if (!("All years" %in% input$Year_MA)) {
      d_filtered <- d_filtered %>% filter(Gregorian_year %in% input$Year_MA)
    }
    
    return(d_filtered)
  })
  

  Manufacturer_MA <- reactive({
    req(year_data_MA())
    unique(year_data_MA()[["Manufacturer"]])
  })
  output$Manufacturer_ui_MA <- renderUI({
    req(Manufacturer_MA())
    selectInput("Manufacturer_MA", "Please select the manufacturers", 
                choices = c("All manufacturers", Manufacturer_MA()), 
                multiple = TRUE, selected = "All manufacturers")
  })
  manufacturer_data_MA <- reactive({
    req(year_data_MA())
    d_filtered <- year_data_MA()
    
    if (!("All manufacturers" %in% input$Manufacturer_MA)) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_MA)
    }
    
    return(d_filtered)
  })
  MA_Manufacturer <- reactive({
    req(MA_year())
    d_filtered <- MA_year()
    
    if (!("All manufacturers" %in% input$Manufacturer_MA)) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_MA)
    }
    
    return(d_filtered)
  })
  
    
  Country_MA <- reactive({
    req(manufacturer_data_MA())
    manufacturer_data_1_sorted <- manufacturer_data_MA() %>% arrange(Country)
    unique(manufacturer_data_1_sorted[["Country"]])
  })
  output$Country_ui_MA <- renderUI({
    req(Country_MA())
    pickerInput(
      inputId = "Country_MA",
      label = "Please select the countries",
      choices = Country_MA(),
      selected = Country_MA(),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
  country_data_MA <- reactive({
    req(manufacturer_data_MA())
    d_filtered <- manufacturer_data_MA()
    
    if (!("All countries" %in% input$Country_MA)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_MA)
    }
    
    return(d_filtered)
  })
  MA_country <- reactive({
    req(MA_Manufacturer())
    d_filtered <- MA_Manufacturer()
    
    if (!("All countries" %in% input$Country_MA)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_MA)
    }
    
    return(d_filtered)
  })
  
  
  Medicine_MA <- reactive({
    req(country_data_MA())
    unique(country_data_MA()[["Medicine"]])
  })
  output$Medicine_ui_MA <- renderUI({
    req(Medicine_MA())
    selectInput("Medicine_MA", "Please select the medicines", 
                choices = c("All medicines", Medicine_MA()), 
                multiple = TRUE, selected = "All medicines")
  })
  medicine_data_MA <- reactive({
    req(country_data_MA())
    d_filtered <- country_data_MA()
    
    if (!("All medicines" %in% input$Medicine_MA)) {
      d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine_MA)
    }
    
    return(d_filtered)
  })
  MA_medicine <- reactive({
    req(MA_country())
    d_filtered <- MA_country()
    
    if (!("All medicines" %in% input$Medicine_MA)) {
      d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine_MA)
    }
    
    return(d_filtered)
  })
  
  
  Dosage_MA <- reactive({
    req(medicine_data_MA())
    unique(medicine_data_MA()[["Dosage"]])
  })
  output$Dosage_ui_MA <- renderUI({
    req(Dosage_MA())
    selectInput("Dosage_MA", "Please select the dosages", 
                choices = c("All Dosages", Dosage_MA()), 
                multiple = TRUE, selected = "All Dosages")
  })
  filtered_data_MA <- reactive({
    req(medicine_data_MA())
    d_filtered <- medicine_data_MA()
    
    if (!("All Dosages" %in% input$Dosage_MA)) {
      d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage_MA)
    }
    
    return(d_filtered)
  })
  MA_1 <- reactive({
    req(MA_medicine())
    d_filtered <- MA_medicine()
    
    if (!("All Dosages" %in% input$Dosage_MA)) {
      d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage_MA)
    }
    
    return(d_filtered)
  })
  MA_1_waide <- reactive({
    req(MA_1(), MA_wide())
    
    g=sort(unique(MA_1()$id))
    
    d_filtered=MA_wide() %>% 
      filter(id %in% g)
    
    return(d_filtered)
  })


  # MA plots ============================================
  data_plot_year_MA <- eventReactive(input$calcButton_MA, {
    req(filtered_data_MA())
    
    df <- filtered_data_MA() %>% 
      mutate(Total_Net = if_else(is.na(Total_Net), 0, Total_Net))
    
    required_cols <- c("Gregorian_year", "Status", "Total_Net")
    req(all(required_cols %in% names(df)))
    
    df %>%
      group_by(Gregorian_year, Status) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop") %>%
      mutate(Gregorian_year = factor(Gregorian_year))
  })
  
  output$plot_year_MA <- renderPlotly({
    req(data_plot_year_MA())
    
    data_to_plot <- data_plot_year_MA()
    
    # Colors for Status
    num_status <- dplyr::n_distinct(data_to_plot$Status)
    status_palette <- generate_palette(num_status, c("#DB6400", "#272121"))
    
    p <- ggplot2::ggplot(
      data_to_plot,
      ggplot2::aes(
        x = Gregorian_year,
        y = Total_Net,
        fill = Status,
        text = paste0(
          "Year: ", Gregorian_year, "<br>",
          "Status: ", Status, "<br>",
          "Total Net: ", scales::dollar(Total_Net, accuracy = 1)
        )
      )
    ) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_fill_manual(values = status_palette, name = "MA Status") +
      ggplot2::labs(
        title = "Total Net by Year (Stacked by MA Status)",
        x = "Year",
        y = "Total Net"
      ) +
      ggplot2::scale_y_continuous(labels = scales::dollar_format(accuracy = 1)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(hjust = 0.5)
      )
    
    plotly::ggplotly(p, tooltip = "text")
  })
  
  # --- 3. Table Output (NEW) ---
  output$table_year_MA <- DT::renderDT({
    req(data_plot_year_MA())
    
    # Get data
    df <- data_plot_year_MA()
    
    # Create a clean DataTable
    DT::datatable(
      df,
      rownames = FALSE,
      extensions = 'Buttons', # Add download buttons (useful for regulatory data)
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10
      ),
      colnames = c("Year", "Status", "Total Net")
    ) %>%
      DT::formatCurrency("Total_Net", currency = "$", digits = 0)
  })
  
  # MA tables ---------------------------------------------------------------
  output$data_MA_11 <- renderReactable({
    req(MA_1_waide())
    
    reactable(
      MA_1_waide(),
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
  
  
  output$downloadTable_MA <- downloadHandler(
    filename = function() {
      req(MA_1_waide())
      paste("MA-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- MA_1_waide()
      # Convert list columns (and everything else) to character
      df_flat <- as.data.frame(lapply(df, function(col) {
        if (is.list(col)) {
          vapply(col, function(x) paste(as.character(x), collapse = "; "), character(1))
        } else {
          col
        }
      }), stringsAsFactors = FALSE)
      
      write.csv(df_flat, file, row.names = FALSE)
    }
  )
  
  filtered_data_MA_table=eventReactive(input$calcButton_MA, {
    req(filtered_data_MA())
    filtered_data_MA()
  })
  output$data_11 <- renderReactable({
    req(filtered_data_MA_table())
    
    reactable(
      filtered_data_MA_table(),
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
      req(filtered_data_MA())
      paste("table2-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- filtered_data_MA()
      # Convert list columns (and everything else) to character
      df_flat <- as.data.frame(lapply(df, function(col) {
        if (is.list(col)) {
          vapply(col, function(x) paste(as.character(x), collapse = "; "), character(1))
        } else {
          col
        }
      }), stringsAsFactors = FALSE)
      
      write.csv(df_flat, file, row.names = FALSE)
    }
  )
  

  
  # MA Value Box Rendering ============================================

  export_card_data <- eventReactive(input$calcButton_MA, {
    req(filtered_data_MA())
    filtered_data_MA()
  })
  

  output$total_net_MA_card <- renderUI({
    df <- export_card_data()
    req(df, nrow(df) > 0)
    
    # Data Prep
    total_net_With_MA <- sum(df$Total_Net[df$Status == "With MA Approval"], na.rm = TRUE)
    total_net_Without_MA <- sum(df$Total_Net[df$Status == "Without MA Approval"], na.rm = TRUE)
    grand_total <- total_net_With_MA + total_net_Without_MA
    
    value_box(
      title = "Net Exports by MA Status",
      value = scales::dollar(grand_total, accuracy = 0.001, scale = 1e-6, suffix = "M"),
      showcase = bsicons::bs_icon("bank2", class = "text-info"), # Cyan Icon
      theme = "white",
      # Cyan accent border to match the "Info/Data" theme
      class = "shadow-sm border-start border-5 border-info", 
      height = "180px",
      
      # Breakdown with colored icons for readability on white
      div(class = "pt-2",
          div(class = "d-flex align-items-center gap-2",
              bsicons::bs_icon("check-circle-fill", class = "text-success"), # Green Check
              span("With MA:", class = "text-muted"), 
              strong(scales::dollar(total_net_With_MA, accuracy = 0.001, scale = 1e-6, suffix = "M"))
          ),
          div(class = "d-flex align-items-center gap-2",
              bsicons::bs_icon("x-circle-fill", class = "text-danger"), # Red X
              span("No MA:", class = "text-muted"), 
              strong(scales::dollar(total_net_Without_MA, accuracy = 0.001, scale = 1e-6, suffix = "M"))
          )
      )
    )
  })
  
  export_card_data_1 <- eventReactive(input$calcButton_MA, {
    req(MA_1_waide())
    MA_1_waide()
  })
  
  output$Ma_card <- renderUI({
    df <- export_card_data_1()
    req(df, nrow(df) > 0)
    
    # Data Prep
    MA_new <- nrow(df[df$Project_Status == "new",])
    MA_re_registration <- nrow(df[df$Project_Status == "re-registration",])
    MA_expired <- nrow(df[df$Expiration_status == "Expired",])
    total_MA <- MA_new + MA_re_registration
    
    value_box(
      title = "Active MA Approvals",
      value = scales::number(total_MA, accuracy = 1),
      showcase = bsicons::bs_icon("file-earmark-medical", class = "text-primary"), # Blue Icon
      theme = "white",
      # Blue accent border for "Regulatory/Official" theme
      class = "shadow-sm border-start border-5 border-primary", 
      height = "180px",
      
      # Breakdown
      div(class = "pt-2 small",
          div(
            bsicons::bs_icon("plus-circle", class = "text-success"), # Green Plus
            span("New:", class = "text-muted"), 
            strong(scales::number(MA_new, accuracy = 1))
          ),
          div(
            bsicons::bs_icon("arrow-repeat", class = "text-primary"), # Blue Repeat
            span("Re-reg:", class = "text-muted"), 
            strong(scales::number(MA_re_registration, accuracy = 1))
          ),
          div(
            bsicons::bs_icon("exclamation-triangle", class = "text-warning"), # Orange Warning
            span("Expired:", class = "text-muted"), 
            strong(scales::number(MA_expired, accuracy = 1))
          )
      )
    )
  })
  
  
  # output$total_net_MA_card <- renderUI({
  #   df <- export_card_data()
  #   req(df, nrow(df) > 0)
  #   
  #   total_net_With_MA <- sum(df$Total_Net[df$Status == "With MA Approval"], na.rm = TRUE)
  #   total_net_Without_MA <- sum(df$Total_Net[df$Status == "Without MA Approval"], na.rm = TRUE)
  #   grand_total <- total_net_With_MA + total_net_Without_MA
  #   
  #   value_box(
  #     title = HTML("Total Net Exports<br><small><span style='color: #1f77b4;'>With MA:</span> ", 
  #                  scales::dollar(total_net_With_MA, accuracy = 1),
  #                  " | ",
  #                  "<span style='color: #d62728;'>Without MA:</span> ", 
  #                  scales::dollar(total_net_Without_MA, accuracy = 1), "</small>"),
  #     value = tags$span(scales::dollar(grand_total, accuracy = 1), 
  #                       style = "font-size: 1.4rem; font-weight: bold;"),
  #     showcase = bsicons::bs_icon("cash-coin"),
  #     theme = "primary",
  #     height = "180px",
  #     div(HTML(paste0(
  #       "<small style='color: #666;'>Grand Total: ", 
  #       scales::dollar(grand_total, accuracy = 1), "</small>"
  #     )), style = "margin-top: 5px;")
  #   )
  # })
  # 
  # output$Ma_card <- renderUI({
  #   df <- Ma_card_data()
  #   req(df, nrow(df) > 0)
  #   
  #   MA_new <- nrow(df[df$Project_Status == "new",])
  #   MA_re_registration <- nrow(df[df$Project_Status == "re-registration",])
  #   MA_expired <- nrow(df[df$Expiration_status == "Expired",])
  #   total_MA <- MA_new + MA_re_registration
  #   
  #   value_box(
  #     title = HTML("MA Approvals<br><small><span style='color: #1f77b4;'>New MA:</span> ", 
  #                  scales::number(MA_new, accuracy = 1),
  #                  " | ",
  #                  "<span style='color: #d62728;'>Re-registered:</span> ", 
  #                  scales::number(MA_re_registration, accuracy = 1),
  #                  " | ",
  #                  "<span style='color: #ff7f0e;'>Expired:</span> ", 
  #                  scales::number(MA_expired, accuracy = 1), "</small>"),
  #     value = tags$span(scales::number(total_MA, accuracy = 1), 
  #                       style = "font-size: 1.4rem; font-weight: bold;"),
  #     showcase = bsicons::bs_icon("cash-coin"),
  #     theme = "primary",
  #     height = "180px",
  #     div(HTML(paste0(
  #       "<small style='color: #666;'>Grand Total: ", 
  #       scales::number(total_MA, accuracy = 1), "</small>"
  #     )), style = "margin-top: 5px;")
  #   )
  # })
  

  
  # -------------------------------------------------------------------------
  # FORECAST COMPARISON LOGIC (UPDATED FOR QTY, VALUE, PROBABILITY, WEIGHTED)
  # -------------------------------------------------------------------------
  
  # 1. Read Sheet Names upon Upload
  available_sheets <- reactive({
    req(input$file_compare)
    readxl::excel_sheets(input$file_compare$datapath)
  })
  
  # 2. Render Sheet Selectors
  output$compare_sheet_ui_old <- renderUI({
    req(available_sheets())
    selectInput("sheet_old", "Select BASE (Old) Sheet:", choices = available_sheets(), selected = head(available_sheets(), 1))
  })
  
  output$compare_sheet_ui_new <- renderUI({
    req(available_sheets())
    # Try to select the second sheet by default if available
    default_sel <- if(length(available_sheets()) > 1) available_sheets()[2] else available_sheets()[1]
    selectInput("sheet_new", "Select TARGET (New) Sheet:", choices = available_sheets(), selected = default_sel)
  })
  
  # 3. Main Comparison Calculation
  comparison_results <- eventReactive(input$btn_run_compare, {
    req(input$file_compare, input$sheet_old, input$sheet_new)
    
    tryCatch({
      # Read Data
      df_old_raw <- read_excel(input$file_compare$datapath, sheet = input$sheet_old)
      df_new_raw <- read_excel(input$file_compare$datapath, sheet = input$sheet_new)
      
      # --- CLEANING & INDEXING ---
      clean_numeric <- function(x) {
        x_clean <- gsub("[^0-9.-]", "", as.character(x))
        as.numeric(x_clean)
      }
      
      # 1. FINAL KEYS
      join_keys <- c("Gregorian_year", "Estim_Shipment", "Country", "Medicine", "Dosage", "Category")
      
      # Prepare DF OLD (Clean ALL 4 variables)
      df_old <- df_old_raw %>%
        mutate(
          QTY = clean_numeric(QTY),
          Value = clean_numeric(Value),          # Added
          Weighted = clean_numeric(Weighted),
          Probability = clean_numeric(Probability)
        ) %>%
        group_by(across(all_of(join_keys))) %>%
        mutate(dup_id = row_number()) %>%
        ungroup()
      
      # Prepare DF NEW (Clean ALL 4 variables)
      df_new <- df_new_raw %>%
        mutate(
          QTY = clean_numeric(QTY),
          Value = clean_numeric(Value),          # Added
          Weighted = clean_numeric(Weighted),
          Probability = clean_numeric(Probability)
        ) %>%
        group_by(across(all_of(join_keys))) %>%
        mutate(dup_id = row_number()) %>%
        ungroup()
      
      # 2. MATCH USING KEYS + DUP_ID
      final_keys <- c(join_keys, "dup_id")
      
      # -----------------------------------------
      
      # Find ADDED Rows
      added <- anti_join(df_new, df_old, by = final_keys) %>% select(-dup_id)
      
      # Find REMOVED Rows
      removed <- anti_join(df_old, df_new, by = final_keys) %>% select(-dup_id)
      
      # Find MODIFIED Rows
      common_rows <- inner_join(
        df_old %>% select(all_of(final_keys), 
                          W_Old = Weighted, Q_Old = QTY, P_Old = Probability, V_Old = Value),
        df_new %>% select(all_of(final_keys), 
                          W_New = Weighted, Q_New = QTY, P_New = Probability, V_New = Value),
        by = final_keys
      )
      
      modified <- common_rows %>%
        mutate(
          w_new = coalesce(W_New, 0), w_old = coalesce(W_Old, 0),
          q_new = coalesce(Q_New, 0), q_old = coalesce(Q_Old, 0),
          p_new = coalesce(P_New, 0), p_old = coalesce(P_Old, 0),
          v_new = coalesce(V_New, 0), v_old = coalesce(V_Old, 0),
          
          # Calculate Differences
          Diff_Weighted = w_new - w_old,
          Diff_QTY = q_new - q_old,
          Diff_Prob = p_new - p_old,
          Diff_Value = v_new - v_old
        ) %>%
        # Filter to keep ONLY rows with real changes in any of the 4 vars
        filter(abs(Diff_Weighted) > 1 | abs(Diff_QTY) > 0 | abs(Diff_Prob) > 0.001 | abs(Diff_Value) > 1) %>%
        select(-w_new, -w_old, -q_new, -q_old, -p_new, -p_old, -v_new, -v_old, -dup_id)
      
      list(added = added, removed = removed, modified = modified)
      
    }, error = function(e) {
      showNotification(paste("Error during comparison:", e$message), type = "error")
      NULL
    })
  })
  
  # -------------------------------------------------------------------------
  # OUTPUTS: Value Boxes
  # -------------------------------------------------------------------------
  
  output$box_diff_value <- renderUI({
    res <- comparison_results()
    if(is.null(res)) return(NULL)
    
    net_change <- sum(res$modified$Diff_Weighted, na.rm=T) + 
      sum(res$added$Weighted, na.rm=T) - 
      sum(res$removed$Weighted, na.rm=T)
    
    color <- if(net_change >= 0) "success" else "danger"
    icon_name <- if(net_change >= 0) "arrow-up-circle" else "arrow-down-circle"
    
    value_box(
      title = "Net Weighted Impact",
      value = scales::dollar(net_change),
      showcase = bsicons::bs_icon(icon_name),
      theme = color
    )
  })
  
  output$box_diff_qty <- renderUI({
    res <- comparison_results()
    if(is.null(res)) return(NULL)
    
    net_qty <- sum(res$modified$Diff_QTY, na.rm=T) + 
      sum(res$added$QTY, na.rm=T) - 
      sum(res$removed$QTY, na.rm=T)
    
    value_box(
      title = "Net Quantity Change",
      value = scales::comma(net_qty),
      showcase = bsicons::bs_icon("box-seam"),
      theme = "primary"
    )
  })
  
  output$box_new_records <- renderUI({
    res <- comparison_results()
    value_box(title = "New Records", value = nrow(res$added), theme = "info", showcase = bsicons::bs_icon("plus-square"))
  })
  
  output$box_removed_records <- renderUI({
    res <- comparison_results()
    value_box(title = "Removed Records", value = nrow(res$removed), theme = "secondary", showcase = bsicons::bs_icon("dash-square"))
  })
  
  # -------------------------------------------------------------------------
  # OUTPUTS: Tables
  # -------------------------------------------------------------------------
  
  # Reusable Style Function
  style_change <- function(value) {
    if (is.na(value)) return(list(color = "black"))
    if (value > 0) list(color = "#28a745", fontWeight = "bold") # Green
    else if (value < 0) list(color = "#dc3545", fontWeight = "bold") # Red
    else list(color = "black")
  }
  
  output$table_diff_modified <- renderReactable({
    req(comparison_results())
    df <- comparison_results()$modified
    
    reactable(df,
              columns = list(
                # 1. QTY
                Diff_QTY = colDef(name = "QTY Change", format = colFormat(separators = TRUE, digits = 0), style = function(value) style_change(value)),
                
                # 2. Value (Gross)
                Diff_Value = colDef(name = "Value Change", format = colFormat(currency = "USD", digits = 0), style = function(value) style_change(value)),
                
                # 3. Probability
                Diff_Prob = colDef(name = "Prob Change", format = colFormat(percent = TRUE, digits = 1), style = function(value) style_change(value)),
                
                # 4. Weighted Value
                Diff_Weighted = colDef(name = "Weighted Change", format = colFormat(currency = "USD", digits = 0), style = function(value) style_change(value)),
                
                # Context
                P_Old = colDef(name = "Old Prob%", format = colFormat(percent = TRUE, digits = 0)),
                P_New = colDef(name = "New Prob%", format = colFormat(percent = TRUE, digits = 0))
              ),
              filterable = TRUE, striped = TRUE, highlight = TRUE
    )
  })
  
  output$table_diff_added <- renderReactable({
    req(comparison_results())
    reactable(comparison_results()$added, filterable = TRUE, striped = TRUE)
  })
  
  output$table_diff_removed <- renderReactable({
    req(comparison_results())
    reactable(comparison_results()$removed, filterable = TRUE, striped = TRUE)
  })
  
  # -------------------------------------------------------------------------
  # OUTPUTS: Plots
  # -------------------------------------------------------------------------
  
  output$plot_diff_country <- renderPlotly({
    req(comparison_results())
    res <- comparison_results()
    
    # Aggregate changes by Country (Using Weighted Value)
    mod_agg <- res$modified %>% group_by(Country) %>% summarise(Change = sum(Diff_Weighted, na.rm=T))
    add_agg <- res$added %>% group_by(Country) %>% summarise(Change = sum(Weighted, na.rm=T))
    rem_agg <- res$removed %>% group_by(Country) %>% summarise(Change = -sum(Weighted, na.rm=T))
    
    total_agg <- bind_rows(mod_agg, add_agg, rem_agg) %>%
      group_by(Country) %>%
      summarise(Total_Change = sum(Change, na.rm=T)) %>%
      filter(Total_Change != 0) %>%
      arrange(desc(Total_Change))
    
    plot_ly(total_agg, x = ~reorder(Country, Total_Change), y = ~Total_Change, type = 'bar',
            marker = list(color = ~ifelse(Total_Change > 0, '#28a745', '#dc3545'))) %>%
      layout(title = "Net Weighted Change by Country", xaxis = list(title = ""), yaxis = list(title = "Change in USD"))
  })
  
  output$plot_diff_medicine <- renderPlotly({
    req(comparison_results())
    res <- comparison_results()
    
    # Similar aggregation for Medicine
    mod_agg <- res$modified %>% group_by(Medicine) %>% summarise(Change = sum(Diff_Weighted, na.rm=T))
    add_agg <- res$added %>% group_by(Medicine) %>% summarise(Change = sum(Weighted, na.rm=T))
    rem_agg <- res$removed %>% group_by(Medicine) %>% summarise(Change = -sum(Weighted, na.rm=T))
    
    total_agg <- bind_rows(mod_agg, add_agg, rem_agg) %>%
      group_by(Medicine) %>%
      summarise(Total_Change = sum(Change, na.rm=T)) %>%
      filter(Total_Change != 0) %>%
      arrange(desc(Total_Change))
    
    plot_ly(total_agg, x = ~reorder(Medicine, Total_Change), y = ~Total_Change, type = 'bar',
            marker = list(color = ~ifelse(Total_Change > 0, '#17a2b8', '#fd7e14'))) %>%
      layout(title = "Net Weighted Change by Medicine", xaxis = list(title = ""), yaxis = list(title = "Change in USD"))
  })
  
}

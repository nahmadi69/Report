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
    req(data_1())
    d_filtered <- data_1()
    
    if (!("All Categories" %in% input$Category_1)) {
      d_filtered <- d_filtered %>% filter(Category %in% input$Category_1)
    }
    
    return(d_filtered)
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
    req(manufacturer_data_1())
    d_filtered <- manufacturer_data_1()
    
    if (!("All countries" %in% input$Country_1)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_1)
    }
    
    return(d_filtered)
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
    req(country_data_1())
    d_filtered <- country_data_1()
    
    if (!("All consignees" %in% input$Consignee_1)) {
      d_filtered <- d_filtered %>% filter(Consignee %in% input$Consignee_1)
    }
    
    return(d_filtered)
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
      value = tags$span(scales::dollar(total_net, accuracy = 1), style = "font-size: 1.2rem;"),
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
      value = tags$span(top_c$Country, style = "font-size: 1.2rem;"),
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
      value = tags$span(top_m$Medicine, style = "font-size: 1.2rem;"),
      showcase = bsicons::bs_icon("capsule"),
      p(class = "small", paste("Value:", scales::dollar(top_m$total_net, accuracy = 1))),
      theme = "primary",
      height = "150px"
    )
  })
  
  # MA plots ============================================
  
  data_plot_year_MA <- eventReactive(input$calcButton_1, {
    req(filtered_data_1(), input$V_Year_1)
    
    filtered_data_1() %>%
      group_by(!!sym(input$V_Year_1), Status) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE), .groups = "drop")
  })
  
  output$plot_year_MA <- renderPlotly({
    req(data_plot_year_MA(), input$V_Year_1)
    
    data_to_plot <- data_plot_year_MA() %>%
      mutate(x_year = factor(!!sym(input$V_Year_1)))
    
    num_years <- n_distinct(data_to_plot$x_year)
    professional_palette <- generate_palette(num_years, c("#272121", "#DB6400"))
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Status,
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
        title = "Total Net by MA Status and Year",
        xaxis = list(title = "MA Status", tickangle = 0),
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
  
  # Sales plots -----------------------------------------------------
  
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
      value = tags$span(scales::dollar(total_forecast, accuracy = 1), style = "font-size: 1.2rem;"),
      showcase = bsicons::bs_icon("graph-up-arrow"),
      theme = "primary",
      height = "150px",
      p(HTML("&nbsp;"))
    )
  })
  
  output$actual_card <- renderUI({
    df <- forecast_card_data()
    req(df, nrow(df) > 0)
    
    total_actual <- sum(df$Total_Net, na.rm = TRUE)
    
    value_box(
      title = "Total Actual Sales",
      value = tags$span(scales::dollar(total_actual, accuracy = 1), style = "font-size: 1.2rem;"),
      showcase = bsicons::bs_icon("cash-stack"),
      theme = "primary",
      height = "150px",
      p(HTML("&nbsp;"))
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
      title = "Top Forecast Country",
      value = tags$span(top_c$Country, style = "font-size: 1.2rem;"),
      showcase = bsicons::bs_icon("globe-americas"),
      p(class = "small", paste("Forecast:", scales::dollar(top_c$total_weighted, accuracy = 1))),
      theme = "primary",
      height = "150px"
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
      title = "Top Forecast Medicine",
      value = tags$span(top_m$Medicine, style = "font-size: 1.2rem;"),
      showcase = bsicons::bs_icon("capsule"),
      p(class = "small", paste("Forecast:", scales::dollar(top_m$total_weighted, accuracy = 1))),
      theme = "primary",
      height = "150px"
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
}

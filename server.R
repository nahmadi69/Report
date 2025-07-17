function(input, output, session) {

  Country_code=read_xlsx("Country_code.xlsx")
  
  
  # Sales data cleaning -----------------------------------------------------
  
  data_1 <- reactive({
    if (is.null(input$file_1)) {
      return(NULL)  
    }
    read_excel(input$file_1$datapath,sheet = "Export")
  })
 
  Category_1 <- reactive({
    req(data_1())
    unique(data_1()["Category"])
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
    
    if (!("All Categories" %in% input$Category_1 )) {
      d_filtered <- d_filtered %>% filter(Category %in% input$Category_1)
    }
    return(data.frame(d_filtered))
  })
  
  
  output$V_Year_ui_1 <- renderUI({
    req(data_1())
    selectInput("V_Year_1", "Please select the year variable", choices = colnames(data_1()),selected = "Gregorian_year")
  })
  Year_1 <- reactive({
    req(Category_data_1(),input$V_Year_1)
    unique(Category_data_1()[input$V_Year_1])
  })
  output$Year_ui_1 <- renderUI({
    req(Year_1(),input$V_Year_1)
    selectInput("Year_1", "Please select the years", choices = c("All years",Year_1()),multiple = TRUE, selected = "All years")
  })
  year_data_1 <- reactive({
    req(Category_data_1())
    d_filtered <- Category_data_1()
    
    if (!("All years" %in% input$Year_1 )) {
      d_filtered <- d_filtered %>% filter(!!sym(input$V_Year_1) %in% input$Year_1)
    }
    return(data.frame(d_filtered))
  })
  
  
  output$V_Month_ui_1 <- renderUI({
    req(data_1())
    selectInput("V_Month_1", "Please select the Month variable", choices = colnames(data_1()),selected = "Gregorian_month")
  })
  Month_1 <- reactive({
    req(year_data_1(),input$V_Month_1)
    d=year_data_1() %>% arrange(!!!sym(input$V_Month_1))
    unique(d[input$V_Month_1])
  })
  output$Month_ui_1 <- renderUI({
    req(Month_1(),input$V_Month_1)
    selectInput("Month_1", "Please select the Months", choices = c("All Months",Month_1()),multiple = TRUE, selected = "All Months")
  })
  Month_data_1 <- reactive({
    req(year_data_1())
    d_filtered <- year_data_1()
    
    if (!("All Months" %in% input$Month_1 )) {
      d_filtered <- d_filtered %>% filter(!!sym(input$V_Month_1) %in% input$Month_1)
    }
    return(data.frame(d_filtered))
  })
  
 
  Manufacturer_1 <- reactive({
    req(Month_data_1())
    unique(Month_data_1()["Manufacturer"])
  })
  output$Manufacturer_ui_1 <- renderUI({
    req(Manufacturer_1())
    selectInput("Manufacturer_1", "Please select the manufacturers", choices = c("All manufacturers",Manufacturer_1()),multiple = TRUE, selected = "All manufacturers")
  })
  manufacturer_data_1 <- reactive({
    req(Month_data_1())
    d_filtered <- Month_data_1()
    
    if (!("All manufacturers" %in% input$Manufacturer_1)) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_1)
    }
    return(data.frame(d_filtered))
  })
  
  
  Country_1 <- reactive({
    req(manufacturer_data_1())
    unique(manufacturer_data_1()["Country"])
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
    
    if (!("All countries" %in% input$Country_1 )) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_1)
    }
    return(data.frame(d_filtered))
  })
  
  
  Medicine_1 <- reactive({
    req(manufacturer_data_1())
    unique(manufacturer_data_1()["Medicine"])
  })
  output$Medicine_ui_1 <- renderUI({
    req(Medicine_1())
    # sorted_medicines <- sort(Medicine_1()) # Sort the countries in ascending order
    selectInput("Medicine_1", "Please select the medicines", choices = c("All medicines", Medicine_1()),multiple = TRUE, selected = "All medicines")
  })
  medicine_data_1 <- reactive({
    req(manufacturer_data_1())
    d_filtered <- manufacturer_data_1()
    
    if (!("All medicines" %in% input$Medicine_1)) {
      d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine_1)
    }
    return(data.frame(d_filtered))
  })
  
  
  Dosage_1 <- reactive({
    req(medicine_data_1())
    unique(medicine_data_1()["Dosage"])
  })
  output$Dosage_ui_1 <- renderUI({
    req(Dosage_1())
    selectInput("Dosage_1", "Please select the dosages", choices = c("All Dosages",Dosage_1()),multiple = TRUE , selected = "All Dosages")
  })
  filtered_data_1 <- reactive({
    req(medicine_data_1())
    d_filtered <- medicine_data_1()
    
    if (!("All Dosages" %in% input$Dosage_1)) {
      d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage_1)
    }
    return(data.frame(d_filtered))
  })
  
  
  # Sales tabels -----------------------------------------------------
  data_table_1=eventReactive(input$calcButton_1,{
    
    vars <- syms(c(input$V_Year_1,input$V_Month_1,"Category", "Manufacturer","Country","Consignee",
                   "Medicine","Dosage","Total_Net",
                   "Total_Gross","QTY"))
    vars <- vars[!vars %in% as.character(NULL)]
    data_table <- filtered_data_1() %>% select(!!!vars)
    data_table <- data_table %>% arrange(desc(Total_Net)) %>% 
      mutate(across("Total_Net", ~ formatC(.x, format = "f", digits = 0, flag = "", big.mark = ",", small.mark = "")),
             across("Total_Gross", ~ formatC(.x, format = "f", digits = 0, flag = "", big.mark = ",", small.mark = "")))
  })
  
  output$data_1 <- renderReactable({
    req(data_table_1())
    data_table <- data_table_1()
    reactable(
      data_table,
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
      paste("table1-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_table_1(), file, row.names = FALSE)
    }
  )
  
  data_plot_Country1 <- eventReactive(input$calcButton_1,{
    req(filtered_data_1(),input$V_Year_1) 
    
    data_temp <- filtered_data_1() %>% 
      group_by(!!!sym(input$V_Year_1)) %>% 
      summarise(Total_Net= round(sum(Total_Net,na.rm=TRUE))
                # ,
                # Quantity=round(sum(QTY,na.rm=TRUE))
      ) 
    return(data_temp)
  })
  output$data_country111 <- renderReactable({
    req(data_plot_Country1())
    data_table <- data_plot_Country1()
    reactable(
      data_table,
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
  output$downloadTable111 <- downloadHandler(
    filename = function() {
      paste("table1-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_plot_Country1(), file, row.names = FALSE)
    }
  )
  
  data_plot_Country_year <- eventReactive(input$calcButton_1,{
    req(filtered_data_1(),input$V_Year_1) 
    
    data_temp <- filtered_data_1() %>% 
      group_by(!!!sym(input$V_Year_1),Country) %>% 
      summarise(Total_Net= round(sum(Total_Net,na.rm=TRUE))
                # ,
                # Quantity=round(sum(QTY,na.rm=TRUE))
      ) 
    return(data_temp)
  })
  output$downloadTable_country <- downloadHandler(
    filename = function() {
      paste("table1-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_plot_Country_year(), file, row.names = FALSE)
    }
  )
  
  data_plot_Medicine_year <- eventReactive(input$calcButton_1,{
    req(filtered_data_1(),input$V_Year_1) 
    
    data_temp <- filtered_data_1() %>% 
      group_by(!!!sym(input$V_Year_1),Medicine) %>% 
      summarise(Total_Net= round(sum(Total_Net,na.rm=TRUE))
                # ,
                # Quantity=round(sum(QTY,na.rm=TRUE))
      ) 
    return(data_temp)
  })
  output$downloadTable_medicine <- downloadHandler(
    filename = function() {
      paste("table1-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_plot_Medicine_year(), file, row.names = FALSE)
    }
  )
  # NEW: Export Value Box Rendering ============================================
  
  export_card_data <- eventReactive(input$calcButton_1, {
    req(filtered_data_1())
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
      p(HTML("&nbsp;")) # Placeholder for alignment
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
  
  # Sales plots -----------------------------------------------------
  
  data_plot_year <- eventReactive(input$calcButton_1,{
    req(filtered_data_1(),input$V_Year_1) 
    
    data_temp <- filtered_data_1() %>%
      group_by(!!!sym(input$V_Year_1),Manufacturer) %>% 
      summarise(Total_Net= sum(Total_Net,na.rm=TRUE)
                # ,
                # Quantity=sum(QTY,na.rm=TRUE)
                ) %>% 
      filter(Manufacturer!="Arvand Pharmed")
    
    data_plot_year <- data_temp
    
    return(data_plot_year)
  })
  output$plot_year <- renderPlotly({
    req(data_plot_year(), input$V_Year_1)
    
    data_to_plot <- data_plot_year() %>%
      mutate(x_year = factor(!!sym(input$V_Year_1)))
    
    # 1. Re-introduce the color generation logic
    num_years <- n_distinct(data_to_plot$x_year)
    color_generator <- colorRampPalette(c("#808080", "#DB6400"))
    professional_palette <- color_generator(num_years)
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Manufacturer,
      y = ~Total_Net,
      color = ~x_year,
      # 2. Apply the generated palette
      colors = professional_palette,
      type = 'bar',
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      hovertemplate = paste0(
        '<b>%{x}</b><br>',
        'Year: %{fullData.name}<br>',
        'Total Net: %{y:$,.0f}<extra></extra>'
      )
    ) %>%
      layout(
        title = "Total Net by Manufacturer and Year",
        xaxis = list(
          title = "Manufacturer",
          tickangle = -45
        ),
        yaxis = list(
          title = "Total Net",
          range = c(0, max(data_to_plot$Total_Net, na.rm = TRUE) * 1.15)
        ),
        barmode = 'group',
        legend = list(title = list(text = '<b> Year </b>')),
        margin = list(b = 150)
      )
    
    p
  })
  
  data_plot_Country <- eventReactive(input$calcButton_1, {
    req(filtered_data_1())
    
    data_temp <- filtered_data_1() %>%
      group_by(Country) %>%
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE)) %>%
      arrange(desc(Total_Net))
    
    top_5_countries <- data_temp %>%
      slice_head(n = 5)
    
    other_countries <- data_temp %>%
      slice_tail(n = -5) %>%
      summarise(Country = "Other Country", Total_Net = sum(Total_Net, na.rm = TRUE))
    
    data_plot_Country <- bind_rows(top_5_countries, other_countries)
    
    return(data_plot_Country)
  })
  output$plot_Country <- renderPlotly({
    req(data_plot_Country())
    
    data_to_plot <- data_plot_Country() %>%
      arrange(desc(Total_Net))
    
    num_countries <- nrow(data_to_plot)
    
    color_generator <- colorRampPalette(c("#DB6400", "#808080"))
    
    sorted_palette <- color_generator(num_countries)
    
    data_to_plot$bar_colors <- sorted_palette
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Country,
      y = ~Total_Net,
      type = 'bar',
      marker = list(color = ~bar_colors),
      hovertemplate = '<b>%{x}</b><br>Total Net: %{y:$,.0f}<extra></extra>',
      
      # Add these two lines to display the values on the bars
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      textfont = list(color = ~bar_colors) # Add this line to color the text
      
    ) %>%
      layout(
        title = "Total Net by Country",
        xaxis = list(title = "", categoryorder = "array", categoryarray = ~Country),
        yaxis = list(title = "Total Net")
      )
    
    p
  })  
  
  data_plot_Manufacturer <- eventReactive(input$calcButton_1,{
    req(filtered_data_1()) 
    
    data_temp <- filtered_data_1() %>% 
      group_by(Category) %>% 
      summarise(Total_Net= sum(Total_Net,na.rm=TRUE)
                # ,
                # Quantity=sum(QTY,na.rm=TRUE)
                ) 
    
    data_plot_Manufacturer <- data_temp
    
    return(data_plot_Manufacturer)
  })
  output$plot_Manufacturer <- renderPlotly({
    req(data_plot_Manufacturer())
    
    data_to_plot <- data_plot_Manufacturer() %>%
      arrange(desc(Total_Net))
    
    num_categories <- nrow(data_to_plot)
    
    color_generator <- colorRampPalette(c("#DB6400", "#808080"))
    
    sorted_palette <- color_generator(num_categories)
    
    data_to_plot$bar_colors <- sorted_palette
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Category,
      y = ~Total_Net,
      type = 'bar',
      marker = list(color = ~bar_colors),
      hovertemplate = '<b>%{x}</b><br>Total Net: %{y:$,.0f}<extra></extra>',
      
      # Add these two lines to display the values on the bars
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      textfont = list(color = ~bar_colors) # Add this line to color the text
      
    ) %>%
      layout(
        title = "Total Net by Category",
        xaxis = list(title = "", categoryorder = "array", categoryarray = ~Category),
        yaxis = list(title = "Total Net")
      )
    
    p
    
  })
  
  data_plot_Medicine <- eventReactive(input$calcButton_1,{
    req(filtered_data_1()) 
    
    data_temp <- filtered_data_1() %>% 
      group_by(Medicine) %>% 
      summarise(Total_Net= sum(Total_Net,na.rm=TRUE)
                # ,
                # Quantity=sum(QTY,na.rm=TRUE)
                )
    x <- unique(data_temp %>%  top_n(7, Total_Net) %>% pull(Medicine))
    data_plot_Medicine <- data_temp %>%  filter(Medicine %in% x)
    
    return(data_plot_Medicine)
  })
  output$plot_Medicine <- renderPlotly({
    req(data_plot_Medicine())
    
    # Sort the data from largest to smallest to prepare for color mapping
    data_to_plot <- data_plot_Medicine() %>%
      arrange(desc(Total_Net))
    
    # Count the number of bars
    num_medicines <- nrow(data_to_plot)
    
    # Create the color generator (from orange to gray)
    color_generator <- colorRampPalette(c("#DB6400", "#808080"))
    
    # Generate the palette based on the number of medicines
    sorted_palette <- color_generator(num_medicines)
    
    # Add the generated colors as a column in the data frame
    data_to_plot$bar_colors <- sorted_palette
    
    p <- plot_ly(
      data = data_to_plot,
      x = ~Medicine,
      y = ~Total_Net,
      type = 'bar',
      marker = list(color = ~bar_colors),
      hovertemplate = '<b>%{x}</b><br>Total Net: %{y:$,.0f}<extra></extra>',
      
      # Add these two lines to display the values on the bars
      texttemplate = '%{y:$,.0f}',
      textposition = 'outside',
      textfont = list(color = ~bar_colors) # Add this line to color the text
      
    ) %>%
      layout(
        title = "Total Net by Medicine",
        yaxis = list(title = "Total Net"),
        # Ensure the bar chart order matches our sorted data
        xaxis = list(title = "", categoryorder = "array", categoryarray = ~Medicine)
      )
    
    p
  })
  
  data_heat_Map <- eventReactive(input$calcButton_1, {
    req(filtered_data_1(), input$V_Year_1)
    
    # This part of your code is good, it gets all "Yes" exports
    exported_data <- filtered_data_1() %>%
      filter(New_Export == "Yes") %>%
      select(Country, Medicine, !!sym(input$V_Year_1)) %>%
      rename("Year" = input$V_Year_1)
    
    # NEW: Summarize the data to create the hover-text
    # For each country and year, paste together all the medicines
    timeline_data <- exported_data %>%
      group_by(Country, Year) %>%
      summarise(
        Exported_Medicines = paste(Medicine, collapse = "<br>"), # Use <br> for line breaks in the tooltip
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
      # Define the hover text
      hovertemplate = paste(
        '<b>%{y}</b> - %{x}<br>',
        '------------------<br>',
        '<b>New Exports:</b><br>',
        '%{customdata}<extra></extra>' # Display the list of medicines
      ),
      customdata = ~Exported_Medicines, # Provide the medicine list to the hovertemplate
      marker = list(
        color = '#DB6400', # A nice, vibrant color for the dots
        size = 15,
        symbol = 'circle',
        line = list(
          color = 'black',
          width = 1
        )
      )
    ) %>%
      layout(
        title = "Timeline of New Medicine Exports by Country",
        xaxis = list(
          title = "Year of First Export",
          dtick = 1, # Ensure every year is shown as a tick
          gridcolor = '#e9e9e9'
        ),
        yaxis = list(
          title = "",
          categoryorder = "total ascending", # Countries with more activity appear higher
          gridcolor = '#e9e9e9'
        ),
        plot_bgcolor = '#fdfdfd' # A light background color
      )
    
    p
  })
  
  
  # 
  # Forcast data cleaning -----------------------------------------------------------------
  
  d_sales <- reactive({
    if (is.null(input$file_sales)) {
      return(NULL)  
    }
    read_excel(input$file_sales$datapath,sheet = "Export")
  })
  
  d_forcast <- reactive({
    if (is.null(input$file_forcast)) {
      return(NULL)  
    }
    read_excel(input$file_forcast$datapath,sheet = "Forcast")
  })
  
  output$Probability_ui_2 <- renderUI({
    req(d_forcast())  # Ensure d_forcast() has been evaluated
    
    # Determine the default range based on the data
    min_prob <- min(d_forcast()$Probability)
    max_prob <- max(d_forcast()$Probability)
    
    sliderInput("Probability", "Please select range of probability", 
                value = c(min_prob, max_prob), min = min_prob, max = max_prob, step = 0.01)
  })
  Probability_forcast_2 <- reactive({
    req(input$Probability, d_forcast())  # Ensure input$Probability and d_forcast() are available
    
    d_filtered <- d_forcast() %>%
      filter(Probability >= input$Probability[1] & Probability <= input$Probability[2])
    
    return(d_filtered)
  })
  
  Category_2 <- reactive({
    req(Probability_forcast_2())
    unique(Probability_forcast_2()["Category"])
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
    
    if (!("All Categories" %in%   input$Category_2)) {
      d_filtered <- d_filtered %>% filter(Category %in% input$Category_2)
    }
    return(data.frame(d_filtered))
  })
  Category_sales_2 <- reactive({
    req(d_sales())
    d_filtered <- d_sales()
    
    if (!("All Categories" %in%   input$Category_2)) {
      d_filtered <- d_filtered %>% filter(Category %in% input$Category_2)
    }
    return(data.frame(d_filtered))
  })
  
  
  
  output$V_Year_ui_2 <- renderUI({
    req(d_forcast())
    selectInput("V_Year_2", "Please select the year variable", choices = colnames(d_forcast()),selected = "Gregorian_year")
  })
  Year_2 <- reactive({
    req(Category_forcast_2(),input$V_Year_2)
    unique(Category_forcast_2()[input$V_Year_2])
  })
  output$Year_ui_2 <- renderUI({
    req(Year_2())
    selectInput("Year_2", "Please select the years", choices = c("All years",Year_2()),multiple =TRUE ,selected ="All years" )
  })
  year_forcast_2 <- reactive({
    req(Category_forcast_2())
    d_filtered <- Category_forcast_2()
    
    if (!("All years" %in% input$Year_2)) {
      d_filtered <- d_filtered %>% filter(!!sym(input$V_Year_2) %in% input$Year_2)
    }
    return(data.frame(d_filtered))
  })
  year_sales_2 <- reactive({
    req(Category_sales_2())
    d_filtered <- Category_sales_2()
    
    if (!("All years" %in% input$Year_2)) {
      d_filtered <- d_filtered %>% filter(!!sym(input$V_Year_2) %in% input$Year_2)
    }
    return(data.frame(d_filtered))
  })
  
  
  Manufacturer_2 <- reactive({
    req(year_forcast_2())
    unique(year_forcast_2()["Manufacturer"])
  })
  output$Manufacturer_ui_2 <- renderUI({
    req(Manufacturer_2())
    selectInput("Manufacturer_2", "Please select the manufacturers", choices = c("All manufacturers",Manufacturer_2()),multiple = TRUE,selected = "All manufacturers")
  })
  manufacturer_forcast_2 <- reactive({
    req(year_forcast_2())
    d_filtered <- year_forcast_2()
    
    if (!("All manufacturers" %in% input$Manufacturer_2 )) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_2)
    }
    return(data.frame(d_filtered))
  })
  manufacturer_sales_2 <- reactive({
    req(year_sales_2())
    d_filtered <- year_sales_2()
    
    if (!("All manufacturers" %in% input$Manufacturer_2 )) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_2)
    }
    return(data.frame(d_filtered))
  })
  
  
  Country_2 <- reactive({
    req(manufacturer_forcast_2())
    unique(manufacturer_forcast_2()["Country"])
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
    return(data.frame(d_filtered))
  })
  country_sales_2 <- reactive({
    req(manufacturer_sales_2())
    d_filtered <- manufacturer_sales_2()
    
    if (!("All countries" %in% input$Country_2)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_2)
    }
    return(data.frame(d_filtered))
  })
  
  
  Medicine_2 <- reactive({
    req(manufacturer_forcast_2())
    unique(manufacturer_forcast_2()["Medicine"])
  })
  output$Medicine_ui_2 <- renderUI({
    req(Medicine_2())
    # sorted_medicines <- sort(Medicine_2()) # Sort the countries in ascending order
    selectInput("Medicine_2", "Please select the medicines", choices = c("All medicines", Medicine_2()),multiple = TRUE,selected = "All medicines")
  })
  medicine_forcast_2 <- reactive({
    req(manufacturer_forcast_2())
    d_filtered <- manufacturer_forcast_2()
    
    if (!("All medicines" %in% input$Medicine_2)) {
      d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine_2)
    }
    return(data.frame(d_filtered))
  })
  medicine_sales_2 <- reactive({
    req(manufacturer_sales_2())
    d_filtered <- manufacturer_sales_2()
    
    if (!("All medicines" %in% input$Medicine_2)) {
      d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine_2)
    }
    return(data.frame(d_filtered))
  })
  
  Dosage_2 <- reactive({
    req(medicine_forcast_2())
    unique(medicine_forcast_2()["Dosage"])
  })
  output$Dosage_ui_2 <- renderUI({
    req(Dosage_2())
    selectInput("Dosage_2", "Please select the dosages", choices = c("All Dosages",Dosage_2()),multiple = TRUE,selected ="All Dosages" )
  })
  filtered_forcast_data_2 <- reactive({
    req(medicine_forcast_2())
    d_filtered <- medicine_forcast_2()
    
    if (!("All Dosages" %in% input$Dosage_2)) {
      d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage_2)
    }
    return(data.frame(d_filtered))
  })
  filtered_sales_data_2 <- reactive({
    req(medicine_sales_2())
    d_filtered <- medicine_sales_2()
    
    if (!("All Dosages" %in% input$Dosage_2)) {
      d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage_2)
    }
    return(data.frame(d_filtered))
  })
  
  output$type_ui_2 <- renderUI({
    req(filtered_forcast_data_2())
    selectInput("type_2", "Please select the the type of forcast", choices = c("All types","Predicted","Unpredicted"),selected ="All Dosages")
  })
  
  forcast_target_percent <- eventReactive(input$calcButton_2,{
    req(filtered_sales_data_2(), filtered_forcast_data_2(),input$V_Year_2)
    
    summarized_data <- filtered_forcast_data_2() %>%
      group_by(!!!sym(input$V_Year_2), Manufacturer, Country, Category, Medicine, Dosage) %>% 
      summarize(Value = sum(Value, na.rm = TRUE),
                Weighted = sum(Weighted, na.rm = TRUE),
                Probability=mean(Probability,na.rm=TRUE)) %>% 
      mutate(Forcast="Predicted")
    
    joined_data <- left_join(summarized_data,
                             filtered_sales_data_2() %>%
                               group_by(!!!sym(input$V_Year_2), Manufacturer, Country, Category, Medicine, Dosage) %>% 
                               summarize(Total_Net = sum(Total_Net, na.rm = TRUE)),
                             by = c(input$V_Year_2, "Manufacturer", "Country", "Category", "Medicine", "Dosage")) 
    
    joined_data <- joined_data %>%
      mutate(#Percent_value = round(100 * Net / Value, 2),
        Percent_Weighted =if_else(!is.na(Total_Net),if_else(Weighted!=0,round(100 * Total_Net / Weighted, 2),round(100 * Total_Net / Value, 2)),0),
        Value = round(Value,0),
        Weighted = round(Weighted, 0),
        Total_Net = round(Total_Net, 0),
        Forcast=if_else(is.na(Forcast),"Unpredicted",Forcast))
    
    if(input$type_2 != "All types"){
      joined_data=joined_data %>% 
        filter(Forcast==input$type_2) 
    }
    
    return(joined_data)
  })
  forcast_target_Net <- eventReactive(input$calcButton_2,{
    req(filtered_sales_data_2(), filtered_forcast_data_2(),input$V_Year_2, input$type_2)
    
    summarized_data <- filtered_forcast_data_2() %>%
      group_by(!!!sym(input$V_Year_2), Manufacturer, Country, Category, Medicine, Dosage) %>% 
      summarize(Value = sum(Value, na.rm = TRUE),
                Weighted = sum(Weighted, na.rm = TRUE),
                Probability=round(mean(Probability,na.rm=TRUE),3)) %>% 
      mutate(Forcast="Predicted")
    
    joined_data <- full_join(summarized_data,
                             filtered_sales_data_2() %>% 
                               filter(!!sym(input$V_Year_2) %in% unique(summarized_data[[input$V_Year_2]]))%>%
                               group_by(!!!sym(input$V_Year_2), Manufacturer, Country, Category, Medicine, Dosage) %>% 
                               summarize(Total_Net = sum(Total_Net, na.rm = TRUE)),
                             by = c(input$V_Year_2, "Manufacturer", "Country", "Category", "Medicine", "Dosage")) 
    
    joined_data <- joined_data %>%
      mutate(#Percent_value = round(100 * Net / Value, 2),
        Percent_Weighted =if_else(!is.na(Total_Net),if_else(Weighted!=0,round(100 * Total_Net / Weighted, 2),round(100 * Total_Net / Value, 2)),0),
        Value = round(Value,0),
        Weighted = round(Weighted, 0),
        Total_Net = round(Total_Net, 0),
        Forcast=if_else(is.na(Forcast),"Unpredicted",Forcast)) 
    
    if(input$type_2 != "All types"){
      joined_data=joined_data %>% 
        filter(Forcast==input$type_2) 
    }
    
    return(joined_data)
  })
  
  # Forecast Value Box Rendering ===================================================
  
  card_data <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net())
  })
  
  # Card 1: Total Forecast (Weighted)
  output$forecast_card <- renderUI({
    df <- card_data()
    req(nrow(df) > 0)
    total_w <- sum(df$Weighted, na.rm = TRUE)
    
    value_box(
      title = "Total Forecast (Weighted)",
      value = scales::dollar(total_w, accuracy = 1),
      showcase = bsicons::bs_icon("graph-up-arrow"),
      theme = "primary",
      p(class = "small", "Based on selected filters")
    )
  })
  
  # Card 2: Total Actual Export
  output$actual_card <- renderUI({
    df <- card_data()
    req(nrow(df) > 0)
    total_n <- sum(df$Total_Net, na.rm = TRUE)
    
    value_box(
      title = "Total Actual Export",
      value = scales::dollar(total_n, accuracy = 1),
      showcase = bsicons::bs_icon("box-seam"),
      theme = "info",
      p(class = "small", "Matching forecast period")
    )
  })
  
  # # Card 3: Forecast Achievement Rate
  # output$achievement_card <- renderUI({
  #   df <- card_data()
  #   req(nrow(df) > 0)
  #   total_w <- sum(df$Weighted, na.rm = TRUE)
  #   total_n <- sum(df$Total_Net, na.rm = TRUE)
  #   
  #   rate <- if (total_w > 0) (total_n / total_w) * 100 else 0
  #   
  #   theme_color <- case_when(
  #     rate >= 90 ~ "success",
  #     rate >= 60 ~ "warning",
  #     TRUE ~ "danger"
  #   )
  #   
  #   value_box(
  #     title = "Forecast Achievement",
  #     value = paste0(round(rate, 1), "%"),
  #     showcase = bsicons::bs_icon("trophy"),
  #     theme = theme_color,
  #     p(class = "small", "Actual vs. Weighted Forecast")
  #   )
  # })
  
  # Card 4: Top Forecasted Country
  output$top_country_card <- renderUI({
    df <- card_data()
    req(nrow(df) > 0)
    
    top_c <- df %>%
      filter(!is.na(Country)) %>%
      group_by(Country) %>%
      summarise(total_weighted = sum(Weighted, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_weighted)) %>%
      slice(1)
    
    req(nrow(top_c) > 0)
    
    value_box(
      title = "Top Forecasted Country",
      value = top_c$Country,
      showcase = bsicons::bs_icon("globe-americas"),
      theme = "secondary",
      p(class = "small", paste("Forecast:", scales::dollar(top_c$total_weighted, accuracy = 1)))
    )
  })
  
  # Card 5: Top Forecasted Medicine
  output$top_medicine_card <- renderUI({
    df <- card_data()
    req(nrow(df) > 0)
    
    top_m <- df %>%
      filter(!is.na(Medicine)) %>%
      group_by(Medicine) %>%
      summarise(total_weighted = sum(Weighted, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_weighted)) %>%
      slice(1)
    
    req(nrow(top_m) > 0)
    
    value_box(
      title = "Top Forecasted Medicine",
      value = top_m$Medicine,
      showcase = bsicons::bs_icon("capsule-pill"),
      theme = "secondary",
      p(class = "small", paste("Forecast:", scales::dollar(top_m$total_weighted, accuracy = 1)))
    )
  })
  
  # Forcast plots ---------------------------------------------------------------
  
  data_plot_Year_Forcast <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net()) 
    
    data_temp <- forcast_target_Net() %>%
      group_by(!!sym(input$V_Year_2)) %>% 
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE),
                Forcast = sum(Weighted, na.rm = TRUE)) %>% 
      gather(type,out, -!!sym(input$V_Year_2))
    
    data_plot_Year_Forcast <- data_temp %>% arrange(!!sym(input$V_Year_2))
    
    return(data_plot_Year_Forcast)
  })
  output$plot_Year_Forcast <- renderPlotly({
    req(data_plot_Year_Forcast(), input$V_Year_2)
    
    df <- data_plot_Year_Forcast()
    year_col <- sym(input$V_Year_2)
    
    p <- ggplot(df, aes(x = factor(!!year_col), y = out, fill = type, group = type,
                        # The 'text' aesthetic is for the plotly hover tooltip
                        text = paste("Year:", !!year_col, "<br>", type, ":", scales::dollar(out)))) +
      geom_bar(stat = "identity", position = "dodge") +
      
      # --- ADDED THIS LAYER ---
      # This adds the text labels on top of the bars
      geom_text(
        aes(label = scales::dollar(out, accuracy = 1)), # Set the label to the 'out' value, formatted as dollars
        position = position_dodge(width = 0.9),         # Dodge labels to align with bars
        vjust = -0.5,                                   # Adjust vertically to sit above the bar
        size = 3,                                       # Set font size
        fontface = "bold"                               # Make the text bold for better visibility
      ) +
      # ----------------------
    
    scale_y_continuous(labels = scales::dollar_format(), 
                       # Expand the y-axis slightly to make room for labels on tall bars
                       expand = expansion(mult = c(0, .1))) +
      theme_minimal() +
      labs(x = "Year", y = "Amount", fill = "") +
      scale_fill_manual(values = c("Total_Net" = '#DB6400', "Forcast" = "#5EAAA8"))
    
    ggplotly(p, tooltip = "text")
  })
  
  
  data_plot_Manufacturer_Forcast <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net()) 
    
    data_temp <- forcast_target_Net() %>%
      group_by(Manufacturer) %>% 
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE),
                Forcast = sum(Weighted, na.rm = TRUE)) %>% 
      gather(type,out, -Manufacturer)
    
    data_plot_Manufacturer_Forcast <- data_temp
    
    return(data_plot_Manufacturer_Forcast)
  })
  output$plot_Manufacturer_Forcast <- renderPlotly({
    req(data_plot_Manufacturer_Forcast())
    
    df <- data_plot_Manufacturer_Forcast()
    
    p <- ggplot(df, aes(x = reorder(Manufacturer, -out), y = out, fill = type, group = type,
                        # This 'text' aesthetic is for the interactive hover tooltip
                        text = paste("Manufacturer:", Manufacturer, "<br>", type, ":", scales::dollar(out)))) +
      geom_bar(stat = "identity", position = "dodge") +
      
      # --- ADDED THIS LAYER ---
      # This adds the text labels on top of the bars
      geom_text(
        aes(label = scales::dollar(out, accuracy = 1)), # Format the label as a dollar value
        position = position_dodge(width = 0.9),         # Dodge labels to align with bars
        vjust = -0.5,                                   # Adjust vertically to sit above the bar
        size = 3,                                       # Set font size
        fontface = "bold"                               # Make the label bold
      ) +
      # ----------------------
    
    scale_y_continuous(labels = scales::dollar_format(),
                       # Expand the y-axis to make room for labels on tall bars
                       expand = expansion(mult = c(0, .1))) +
      theme_minimal() +
      labs(x = "Manufacturer", y = "Amount", fill = "") +
      scale_fill_manual(values = c("Total_Net" = '#DB6400', "Forcast" = "#5EAAA8")) +
      
      # --- ADDED THIS THEME ADJUSTMENT ---
      # Rotate x-axis labels to prevent overlap if names are long
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # -----------------------------------
    
    ggplotly(p, tooltip = "text")
  })  
  
  data_plot_Medicine_Forcast <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net()) 
    
    data_temp <- forcast_target_Net() %>%
      group_by(Medicine)%>% 
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE),
                Forcast = sum(Weighted, na.rm = TRUE)) %>% 
      gather(type,out, -Medicine)
    
    x <- unique(data_temp %>% top_n(30, out) %>% pull(Medicine))
    data_plot_Medicine_Forcast <- data_temp %>%  filter(Medicine %in% x)
    
    return(data_plot_Medicine_Forcast)
  })
  output$plot_Medicine_Forcast <- renderPlotly({
    req(data_plot_Medicine_Forcast())
    
    df <- data_plot_Medicine_Forcast()
    
    p <- ggplot(df, aes(x = reorder(Medicine, out), y = out, fill = type,
                        # Text aesthetic for the interactive hover tooltip
                        text = paste("Medicine:", Medicine, "<br>", type, ":", scales::dollar(out)))) +
      geom_bar(stat = "identity", position = "dodge") +
      
      # --- ADDED THIS LAYER ---
      # Adds text labels next to the bars
      geom_text(
        aes(label = scales::dollar(out, accuracy = 1)), # Format label as a dollar value
        position = position_dodge(width = 0.9),         # Dodge labels to align with bars
        hjust = -0.1,                                   # Adjust horizontally to sit just outside the bar
        size = 3.5,                                     # Set font size
        fontface = "bold"                               # Make the label bold
      ) +
      # ----------------------
    
    # The y-axis scale still refers to the 'y' aesthetic ('out' value), even after the flip
    scale_y_continuous(labels = scales::dollar_format(),
                       # Expand the axis to make room for labels
                       expand = expansion(mult = c(0, .1))) +
      
      coord_flip() + # Flip the coordinate system to make bars horizontal
      theme_minimal() +
      labs(x = "Medicine", y = "Amount", fill = "") +
      scale_fill_manual(values = c("Total_Net" = '#DB6400', "Forcast" = "#5EAAA8"))
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 900)
  })  
  
  data_plot_Country_Forcast <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net()) 
    
    data_temp <- forcast_target_Net() %>%
      group_by(Country)%>% 
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE),
                Forcast = sum(Weighted, na.rm = TRUE)) %>% 
      gather(type,out, -Country)
    
    x <- unique(data_temp %>% top_n(30, out) %>% pull(Country))
    data_plot_Country_Forcast <- data_temp %>%  filter(Country %in% x)
    
    return(data_plot_Country_Forcast)
  })
  output$plot_Country_Forcast <- renderPlotly({
    req(data_plot_Country_Forcast())
    
    df <- data_plot_Country_Forcast()
    
    p <- ggplot(df, aes(x = reorder(Country, out), y = out, fill = type,
                        # Text aesthetic for the interactive hover tooltip
                        text = paste("Country:", Country, "<br>", type, ":", scales::dollar(out)))) +
      geom_bar(stat = "identity", position = "dodge") +
      
      # --- ADDED THIS LAYER ---
      # This adds the text labels next to the horizontal bars
      geom_text(
        aes(label = scales::dollar(out, accuracy = 1)), # Format the label as a dollar value
        position = position_dodge(width = 0.9),         # Dodge labels to align with bars
        hjust = -0.1,                                   # Adjust horizontally to sit just outside the bar's end
        size = 3.5,                                     # Set font size
        fontface = "bold"                               # Make the label bold
      ) +
      # ----------------------
    
    # Adjust the scale for the 'y' aesthetic ('out' value)
    scale_y_continuous(labels = scales::dollar_format(),
                       # Expand the axis to make room for labels
                       expand = expansion(mult = c(0, .1))) +
      
      coord_flip() + # Make the bars horizontal
      theme_minimal() +
      labs(x = "Country", y = "Amount", fill = "") +
      scale_fill_manual(values = c("Total_Net" = '#DB6400', "Forcast" = "#5EAAA8"))
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 900)
  })  
  
  data_plot_Medicine_Forcast_Net <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net()) 
    
    data_temp <- forcast_target_Net() %>% 
      group_by(Medicine,Forcast) %>% 
      summarise(out= sum(Total_Net,na.rm=TRUE), .groups = 'drop') %>% 
      rename(type=Forcast) %>% ungroup()
    x <- unique(data_temp %>% top_n(30, out) %>% pull(Medicine))
    data_plot_Medicine_Forcast_Net <- data_temp %>%  filter(Medicine %in% x)
    
    return(data_plot_Medicine_Forcast_Net)
  })
  output$plot_Medicine_Forcast_Net <- renderPlotly({
    req(data_plot_Medicine_Forcast_Net())
    
    df <- data_plot_Medicine_Forcast_Net() %>% 
      mutate(type=factor(type,levels=c("Predicted","Unpredicted")))
    
    p <- ggplot(df, aes(x = reorder(Medicine, out), y = out, fill = type,
                        text = paste("Medicine:", Medicine, "<br>", type, ":", scales::dollar(out)))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = scales::dollar_format())+
      coord_flip() +
      theme_minimal() +
      labs(x = "Medicine", y = "Total Net", fill = "Forecast Type") +
      scale_fill_manual(values = c("Predicted" = '#DB6400', "Unpredicted" = '#808080'))
    
    ggplotly(p, tooltip = "text") %>%
      layout(height = 900)
  })
  
  
  data_plot_Manufacturer_Forcast_Net <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net()) 
    
    data_temp <- forcast_target_Net() %>% 
      group_by(Manufacturer,Forcast) %>% 
      summarise(out= sum(Total_Net,na.rm=TRUE), .groups = 'drop') %>% 
      rename(type=Forcast) %>% ungroup()
    
    data_plot_Manufacturer_Forcast_Net <- data_temp 
    
    return(data_plot_Manufacturer_Forcast_Net)
  })
  output$plot_Manufacturer_Forcast_Net <- renderPlotly({
    req(data_plot_Manufacturer_Forcast_Net())
    
    df <- data_plot_Manufacturer_Forcast_Net()
    
    p <- ggplot(df, aes(x = reorder(Manufacturer, -out), y = out, fill = type,
                        text = paste("Manufacturer:", Manufacturer, "<br>", type, ":", scales::dollar(out)))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = scales::dollar_format())+
      theme_minimal() +
      labs(x = "Manufacturer", y = "Total Net", fill = "Forecast Type") +
      scale_fill_manual(values = c("Predicted" = '#DB6400', "Unpredicted" = '#808080'))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Forcast tables ---------------------------------------------------------------
  
  output$data_2 <- renderReactable({
    req(forcast_target_Net())
    
    data <- forcast_target_Net() %>%
      mutate(Remain = if_else(Total_Net > Weighted, 0, Weighted - Total_Net)) %>%
      arrange(desc(Remain), desc(Weighted), desc(Total_Net)) %>%
      mutate(across(c(Total_Net, Weighted, Remain, Value), 
                    ~ dollar(., accuracy = 1)))
    
    
    reactable(
      data,
      pagination = TRUE,
      defaultPageSize = 15,
      pageSizeOptions = c(10, 20, 50),
      filterable = TRUE,
      searchable = TRUE,
      columns = list(
        Percent_Weighted = colDef(
          name = "Achievement %",
          style = function(value) {
            # Ensure value is numeric before cutting
            numeric_value <- as.numeric(value)
            if (is.na(numeric_value)) return(list(background = "white"))
            
            # Use a diverging color scale
            normalized <- (numeric_value - 50) / 50 # Normalize around 50%
            color <- scales::col_numeric("RdYlGn", domain = c(-1, 1))(normalized)
            list(background = color, color = if(numeric_value < 20 || numeric_value > 90) "white" else "black")
          },
          format = colFormat(suffix = "%")
        )
      ),
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
  
  output$downloadTable2 <- downloadHandler(
    filename = function() {
      paste("Forecast-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(forcast_target_Net()
                , file, row.names = FALSE)
    }
  )
}

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
                  img(src = "CinnaGen_Logo.png", height = 80, width = 160, style = "object-fit: contain; margin-right: 20px;"),
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
                      uiOutput("Country_ui_1"),
                      uiOutput("Manufacturer_ui_1"),
                      uiOutput("Medicine_ui_1"),
                      uiOutput("Dosage_ui_1"),
                      actionButton("calcButton_1", "Calculate", class = "btn-primary btn-block mt-3")
                    )
                  ),
                  card(
                    card_footer("Author: Naser Ahmadi", a(href = "mailto:Naserahmadi3002@gmail.com", "Email"))
                  )
                ),
                accordion(
                  accordion_panel(
                    "Data Table",
                    style = "width: 1500px;",
                    reactableOutput("data_1"),
                    downloadButton("downloadTable1", "Download Table", class = "btn-sm btn-secondary mt-2")
                  ),
                  accordion_panel("Year Plot", style = "width: 1500px;", plotlyOutput('plot_year')),
                  accordion_panel("Country Plot", style = "width: 1500px;", plotlyOutput('plot_Country')),
                  accordion_panel(
                    "Table Country",
                    style = "width: 1500px;",
                    reactableOutput("data_country111"),
                    downloadButton("downloadTable111", "Download Table", class = "btn-sm btn-secondary mt-2")
                  ),      
                  accordion_panel("Manufacturer Plot", style = "width: 1500px;", plotlyOutput("plot_Manufacturer")),
                  accordion_panel("Medicine Plot", style = "width: 1500px;", plotlyOutput("plot_Medicine")),
                  accordion_panel("Heat Map", style = "width: 1500px;", plotlyOutput("plot_heat_map")),
                  accordion_panel("Net Map", style = "width: 1500px;", plotlyOutput('Map_Net'))
                )
              )
    ),
    nav_panel("Forecast",
              layout_sidebar(
                sidebar = sidebar(
                  img(src = "CinnaGen_Logo.png", height = 80, width = 160, style = "object-fit: contain; margin-right: 20px;"),
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
                      uiOutput("Country_ui_2"),
                      uiOutput("Manufacturer_ui_2"),
                      uiOutput("Medicine_ui_2"),
                      uiOutput("Dosage_ui_2"),
                      uiOutput("type_ui_2"),
                      actionButton("calcButton_2", "Calculate", class = "btn-primary btn-block mt-3")
                    )
                  ),
                  card(
                    card_footer("Author: Naser Ahmadi", a(href = "mailto:Naserahmadi3002@gmail.com", "Email"))
                  )
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

# Define the server
server <- function(input, output, session) {
  #Country codes is using for maps
  Country_code=read_xlsx("Country_code.xlsx")
  
  # Sales data cleaning -----------------------------------------------------
  
  data_1 <- reactive({
    if (is.null(input$file_1)) {
      return(NULL)  
    }
    read_excel(input$file_1$datapath,sheet = "Export")
  })
  # output$V_Category_ui_1 <- renderUI({
  #   req(data_1())
  #   selectInput("V_Category_1", "Please select the category variable", choices = colnames(data_1()))
  # })
  Category_1 <- reactive({
    req(data_1())
    unique(data_1()["Category"])
  })
  output$Category_ui_1 <- renderUI({
    req(Category_1())
    # sorted_Categorys <- sort(Category_1()) # Sort the countries in ascending order
    selectInput("Category_1", "Please select the Categories", choices = c("All Categories", Category_1()),multiple = TRUE, selected = "All Categories")
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
  
  
  # output$V_Country_ui_1 <- renderUI({
  #   req(data_1())
  #   selectInput("V_Country_1", "Please select the country variable", choices = colnames(data_1()))
  # })
  Country_1 <- reactive({
    req(Month_data_1())
    unique(Month_data_1()["Country"])
  })
  output$Country_ui_1 <- renderUI({
    req(Country_1())
    selectInput("Country_1", "Please select the countries", choices = c("All countries", Country_1()),multiple = TRUE,selected = "All countries")
  })
  country_data_1 <- reactive({
    req(Month_data_1())
    d_filtered <- Month_data_1()
    
    if (!("All countries" %in% input$Country_1 )) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_1)
    }
    return(data.frame(d_filtered))
  })
  
  
  # output$V_Manufacturer_ui_1 <- renderUI({
  #   req(data_1())
  #   selectInput("V_Manufacturer_1", "Please select the manufacturer variable", choices = colnames(data_1()))
  # })
  Manufacturer_1 <- reactive({
    req(country_data_1())
    unique(country_data_1()["Manufacturer"])
  })
  output$Manufacturer_ui_1 <- renderUI({
    req(Manufacturer_1())
    selectInput("Manufacturer_1", "Please select the manufacturers", choices = c("All manufacturers",Manufacturer_1()),multiple = TRUE, selected = "All manufacturers")
  })
  manufacturer_data_1 <- reactive({
    req(country_data_1())
    d_filtered <- country_data_1()
    
    if (!("All manufacturers" %in% input$Manufacturer_1)) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_1)
    }
    return(data.frame(d_filtered))
  })
  
  
  # output$V_Medicine_ui_1 <- renderUI({
  #   req(data_1())
  #   selectInput("V_Medicine_1", "Please select the medicine variable", choices = colnames(data_1()))
  # })
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
  
  
  # output$V_Dosage_ui_1 <- renderUI({
  #   req(data_1())
  #   selectInput("V_Dosage_1", "Please select the dosage variable", choices = colnames(data_1()))
  # })
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
  
  
  # output$V_Net_ui_1 <- renderUI({
  #   req(data_1())
  #   selectInput("V_Net_1", "Please select the total net variable", choices = colnames(data_1()))
  # })
  # output$V_Gross_ui_1 <- renderUI({
  #   req(data_1())
  #   selectInput("V_Gross_1", "Please select the total gross variable", choices = colnames(data_1()))
  # })
  # output$V_QTY_ui_1 <- renderUI({
  #   req(data_1())
  #   selectInput("V_QTY_1", "Please select the quantity variable", choices = colnames(data_1()))
  # })
  # output$V_Par_ui_1 <- renderUI({
  #   req(data_1())
  #   selectInput("V_Par_1", "Please select the consignee variable", choices = colnames(data_1()))
  # })
  
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
  
  # Sales plots -----------------------------------------------------
  
  data_plot_year <- eventReactive(input$calcButton_1,{
    req(filtered_data_1(),input$V_Year_1) 
    
    data_temp <- filtered_data_1() %>%
      group_by(!!!sym(input$V_Year_1)) %>% 
      summarise(Total_Net= sum(Total_Net,na.rm=TRUE),
                Total_Gross= sum(Total_Gross,na.rm=TRUE),
                Quantity=sum(QTY,na.rm=TRUE))
    # %>% 
    #   gather(type,out,-!!sym(input$V_Year_1))
    
    data_plot_year <- data_temp
    
    return(data_plot_year)
  })
  output$plot_year <- renderPlotly({
    req(data_plot_year(),input$V_Year_1)
    data_plot_year <- data_plot_year() %>% mutate(x=factor(!!!sym(input$V_Year_1)))
    
    p <- plot_ly() %>%
      add_bars(data = data_plot_year, x = ~x, y = ~Quantity, name = 'Quantity',
               marker = list(color = '#D2691E'),text=~comma(round(Quantity,0)), offsetgroup = 1) %>%
      add_bars(data = data_plot_year, x = ~x, y = ~Total_Gross, name = 'Total Gross',
               marker = list(color = '#FFA500'),text=~dollar(round(Total_Gross,0)), yaxis = "y2", offsetgroup = 2) %>%
      add_bars(data = data_plot_year, x = ~x, y = ~Total_Net, name = 'Total Net',
               marker = list(color = '#808080'),text=~dollar(round(Total_Net,0)), yaxis = "y2", offsetgroup = 3) %>%
      layout(
        yaxis2 = list(
          tickfont = list(color = "black"),
          overlaying = "y",
          side = "right",
          title = "Net & Gross"
        ),
        xaxis = list(title = "", tickangle = 0),
        yaxis = list(title = "Quantity"),
        margin = list(b = 100),
        barmode = 'group'
      )
    p
  })
  
  
  # data_plot_year_month <- eventReactive(input$calcButton_1,{
  #   req(filtered_data_1(),input$V_Year_1,input$V_Month_1) 
  #   
  #   data_temp <- filtered_data_1() %>%
  #     mutate(monthly=!!!is.na(input$V_Month_1),
  #            sessional=case_when(
  #       !!!is.na(input$V_Month_1) %in% c(1, 2, 3) ~ "Q1",
  #       !!!is.na(input$V_Month_1) %in% c(4, 5 ,6) ~ "Q2",
  #       !!!is.na(input$V_Month_1) %in% c(7, 8 ,9) ~ "Q3",
  #       !!!is.na(input$V_Month_1) %in% c(10, 11 ,12) ~ "Q4"),
  #       sixmonths=case_when(
  #         !!!is.na(input$V_Month_1) %in% c(1, 2, 3 ,4 ,5 ,6) ~ "The first six month",
  #         !!!is.na(input$V_Month_1) %in% c(7 ,8 ,9 ,10 ,11 ,12) ~ "The last six month")
  #     )
  #   # %>% 
  #   #   gather(type,out,-!!sym(input$V_Year_1))
  #   
  #   data_plot_year_month <- data_temp
  #   
  #   return(data_plot_year_month)
  # })
  # output$plot_year_month <- renderPlotly({
  #   req(data_plot_year_month(),input$V_Year_1)
  #   data_plot_year_month <- data_plot_year_month() %>% 
  #     mutate(x=factor(!!!sym(input$V_Year_1)))
  #   
  #   col <- c('#D2691E','#FFA500','#808080')
  #   
  #   if(input$date_type=="Monthly"){
  #     
  #     d=data_plot_year_month %>% group_by(!!!sym(input$V_Year_1),monthly)
  #     
  #     data_list <- split(d, data_plot_currency11$Currency_type)
  #     plot_list <- list()
  #     for(i in 1:length(data_list)){
  #       max_paid <- max(data_list[[i]]$Paid) * 1.2
  #       plot_list[[i]] <- plot_ly(data_list[[i]], 
  #                                 x = ~reorder(paid_type, -Paid), 
  #                                 y = ~Paid, 
  #                                 color = ~Manufacturer, 
  #                                 colors = col, 
  #                                 type = 'bar', 
  #                                 barmode = 'group', 
  #                                 showlegend = if(i == 1) TRUE else FALSE) %>% 
  #         layout(xaxis = list(title = "Year"), 
  #                yaxis = list(title = "", tickformat = "$,.0f", range = c(0, max_paid)), 
  #                bargap = 0.1, 
  #                font = list(size = 12), 
  #                margin = list(l = 50, r = 50, t = 50, b = 50), 
  #                annotations = list(
  #                  list(
  #                    text = names(data_list)[i], 
  #                    x = 6, 
  #                    y = 0, 
  #                    showarrow = TRUE, 
  #                    font = list(size = 10)
  #                  ),
  #                  list(
  #                    text = names(data_list)[i], 
  #                    x = 0.5, 
  #                    y = -0.2, 
  #                    showarrow = FALSE, 
  #                    font = list(size = 10)
  #                  )
  #                ))
  #       # %>% 
  #       # add_text(x = ~reorder(paid_type, -Paid), 
  #       #          y = ~Paid, 
  #       #          text = ~scales::comma(Paid), 
  #       #          textangle = 90, 
  #       #          textposition = "outside", 
  #       #          offset = 1, 
  #       #          textfont = list(color = "black"))
  #     }
  #     
  #     fig <- subplot(plot_list, nrows = length(data_list), margin = 0.05, shareY = TRUE) %>% 
  #       layout(legend = list(traceorder = "group", itemsizing = "constant"), 
  #              showlegend = TRUE)
  #     
  #     fig
  #     
  #   }else{
  #     
  #     
  #   }
  # 
  # })
  # 
  
  data_plot_Country <- eventReactive(input$calcButton_1,{
    req(filtered_data_1()) 
    
    data_temp <- filtered_data_1() %>% 
      group_by(Country) %>% 
      summarise(Total_Net= sum(Total_Net,na.rm=TRUE),
                Total_Gross= sum(Total_Gross,na.rm=TRUE),
                Quantity=sum(QTY,na.rm=TRUE)) 
    
    x <- unique(data_temp %>% top_n(7, Total_Net) %>% pull(Country))
    data_plot_Country <- data_temp %>%  
      filter(Country %in% x) 
    
    return(data_plot_Country)
  })
  output$plot_Country <- renderPlotly({
    req(data_plot_Country())
    data_plot_Country <- data_plot_Country()
    
    p <- plot_ly() %>%
      add_bars(data = data_plot_Country, x = ~reorder(Country,-Total_Net), y = ~Quantity, name = 'Quantity',
               marker = list(color = '#D2691E'),text=~comma(round(Quantity,0)), offsetgroup = 1) %>% 
      add_bars(data = data_plot_Country, x = ~reorder(Country,-Total_Net), y = ~Total_Gross, name = 'Total Gross',
               marker = list(color = '#FFA500'),text=~dollar(round(Total_Gross,0)), yaxis = "y2", offsetgroup = 2) %>%
      add_bars(data = data_plot_Country, x = ~reorder(Country,-Total_Net), y = ~Total_Net, name = 'Total Net',
               marker = list(color = '#808080'),text=~dollar(round(Total_Net,0)), yaxis = "y2", offsetgroup = 3) %>%
      
      layout(
        yaxis2 = list(
          tickfont = list(color = "black"),
          overlaying = "y",
          side = "right",
          title = "Net & Gross"
        ),
        xaxis = list(title = "", tickangle = 0),
        yaxis = list(title = "Quantity"),
        margin = list(b = 100),
        barmode = 'group'
      ) 
    
    p
    
  })
  
  data_plot_Country1 <- eventReactive(input$calcButton_1,{
    req(filtered_data_1()) 
    
    data_temp <- filtered_data_1() %>% 
      group_by(Country) %>% 
      summarise(Total_Net= round(sum(Total_Net,na.rm=TRUE)),
                Total_Gross= round(sum(Total_Gross,na.rm=TRUE)),
                Quantity=round(sum(QTY,na.rm=TRUE))) 
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
  
  data_plot_Manufacturer <- eventReactive(input$calcButton_1,{
    req(filtered_data_1()) 
    
    data_temp <- filtered_data_1() %>% 
      group_by(Manufacturer) %>% 
      summarise(Total_Net= sum(Total_Net,na.rm=TRUE),
                Total_Gross= sum(Total_Gross,na.rm=TRUE),
                Quantity=sum(QTY,na.rm=TRUE)) 
    # %>% 
    #   gather(type,out,-Manufacturer)
    
    data_plot_Manufacturer <- data_temp
    
    return(data_plot_Manufacturer)
  })
  output$plot_Manufacturer <- renderPlotly({
    req(data_plot_Manufacturer())
    data_plot_Manufacturer <- data_plot_Manufacturer() 
    
    p <- plot_ly() %>%
      add_bars(data = data_plot_Manufacturer, x = ~reorder(Manufacturer,-Total_Net), y = ~Quantity, name = 'Quantity',
               marker = list(color = '#D2691E'),text=~comma(round(Quantity,0)), offsetgroup = 1) %>%
      add_bars(data = data_plot_Manufacturer, x = ~reorder(Manufacturer,-Total_Net), y = ~Total_Gross, name = 'Total Gross',
               marker = list(color = '#FFA500'),text=~dollar(round(Total_Gross,0)), yaxis = "y2", offsetgroup = 2) %>%
      add_bars(data = data_plot_Manufacturer, x = ~reorder(Manufacturer,-Total_Net), y = ~Total_Net, name = 'Total Net',
               marker = list(color = '#808080'),text=~dollar(round(Total_Net,0)), yaxis = "y2", offsetgroup = 3) %>%
      layout(
        yaxis2 = list(
          tickfont = list(color = "black"),
          overlaying = "y",
          side = "right",
          title = "Net & Gross"
        ),
        xaxis = list(title = "", tickangle = 0),
        yaxis = list(title = "Quantity"),
        margin = list(b = 100),
        barmode = 'group'
      )
    p
    
    
  })
  
  
  data_plot_Medicine <- eventReactive(input$calcButton_1,{
    req(filtered_data_1()) 
    
    data_temp <- filtered_data_1() %>% 
      group_by(Medicine) %>% 
      summarise(Total_Net= sum(Total_Net,na.rm=TRUE),
                Total_Gross= sum(Total_Gross,na.rm=TRUE),
                Quantity=sum(QTY,na.rm=TRUE))
    # %>% 
    #   gather(type,out,-Medicine)
    x <- unique(data_temp %>%  top_n(7, Total_Net) %>% pull(Medicine))
    data_plot_Medicine <- data_temp %>%  filter(Medicine %in% x)
    
    return(data_plot_Medicine)
  })
  output$plot_Medicine <- renderPlotly({
    req(data_plot_Medicine())
    data_plot_Medicine <- data_plot_Medicine()
    
    p <- plot_ly() %>%
      add_bars(data = data_plot_Medicine, x = ~reorder(Medicine,-Total_Net), y = ~Quantity, name = 'Quantity',
               marker = list(color = '#D2691E'),text=~comma(round(Quantity,0)), offsetgroup = 1) %>% 
      add_bars(data = data_plot_Medicine, x = ~reorder(Medicine,-Total_Net), y = ~Total_Gross, name = 'Total Gross',
               marker = list(color = '#FFA500'),text=~dollar(round(Total_Gross,0)), yaxis = "y2", offsetgroup = 2) %>%
      add_bars(data = data_plot_Medicine, x = ~reorder(Medicine,-Total_Net), y = ~Total_Net, name = 'Total Net',
               marker = list(color = '#808080'),text=~dollar(round(Total_Net,0)), yaxis = "y2", offsetgroup = 3) %>%
      
      layout(
        yaxis2 = list(
          tickfont = list(color = "black"),
          overlaying = "y",
          side = "right",
          title = "Net & Gross"
        ),
        xaxis = list(title = "", tickangle = 0),
        yaxis = list(title = "Quantity"),
        margin = list(b = 100),
        barmode = 'group'
      ) 
    
    p
  })
  
  
  data_plot_Map <- eventReactive(input$calcButton_1,{
    req(filtered_data_1()) 
    
    data_temp <- filtered_data_1() %>%
      left_join(Country_code) %>%
      group_by(Country, CountryCode) %>% 
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE),
                Total_Gross = sum(Total_Gross, na.rm = TRUE),
                QTY = sum(QTY, na.rm = TRUE))
    
    data_plot_Map <- data_temp 
    
    return(data_plot_Map)
  })
  output$Map_Net <- renderPlotly({
    world_map <- plot_geo()
    
    world_map <- add_trace(world_map, 
                           z = ~as.numeric(data_plot_Map()[["Total_Net"]]),  # Convert categories to numeric
                           locations = ~data_plot_Map()$CountryCode, 
                           text = ~paste("Country: ", data_plot_Map()[["Country"]], "<br>Value: ", 
                                         paste0("$", formatC(data_plot_Map()[["Total_Net"]], format = "f", digits = 0, flag = "", big.mark = ",", small.mark = ""))), 
                           type = 'choropleth', 
                           locationmode = 'ISO-3',
                           colorscale = 'Viridis',  # You can choose any color scale here
                           showscale = FALSE)  # Show the color scale legend
    
    world_map <- layout(world_map, 
                        title="Total Net",
                        geo = list(
                          showframe = TRUE, 
                          showcoastlines = TRUE, 
                          projection = list(type = "mercator"),
                          showcountries = TRUE # Add country borders
                        ),
                        width = 1000,  # Set the width in pixels
                        height = 800)
    world_map
  })
  
  
  data_heat_Map <- eventReactive(input$calcButton_1,{
    req(filtered_data_1(),input$V_Year_1) 
    
    exported_data <- filtered_data_1() %>%
      filter(New_Export == "Yes") %>%
      select(Country, Medicine, !!!sym(input$V_Year_1),New_Export) %>% 
      rename("Year"=input$V_Year_1)
    
    # Define the year range explicitly if needed
    min_year <- min(exported_data$Year)
    max_year <- max(exported_data$Year)
    
    all_combinations <- expand.grid(
      Medicine = unique(exported_data$Medicine),
      Country = unique(exported_data$Country),
      Year = seq(min_year, max_year) 
    )
    
    complete_data <- left_join(all_combinations, exported_data, by = c("Medicine", "Country","Year"))
    complete_data$New_Export[is.na(complete_data$New_Export)] <- "No"
    
    return(complete_data)
  })
  output$plot_heat_map <- renderPlotly({
    req(data_heat_Map())
    
    p <- ggplot(data_heat_Map(), aes(x = Medicine, y = Country, fill = New_Export)) +
      geom_tile(alpha = 0.8) +  # Add transparency to the fill colors
      scale_fill_manual(values = c("Yes" = '#FFA500', "No" = '#808080'), name = "New export") +  # Lighter shades of green and red
      geom_tile(color = "black", fill = NA) +  # Add black border lines
      facet_wrap(~ Year) +
      labs(title = "Exported Medicine Heatmap",
           x = "Medicine", y = "Country") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      layout(width = 1400,  # Set the width in pixels
             height = 400)  # Set the initial width and height
  })
  
  
  
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
    # sorted_Categorys <- sort(Category_2()) # Sort the countries in ascending order
    selectInput("Category_2", "Please select the Categories", choices = c("All Categories", Category_2()),multiple =TRUE,selected = "All Categories" )
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
  
  Country_2 <- reactive({
    req(year_forcast_2())
    unique(year_forcast_2()["Country"])
  })
  output$Country_ui_2 <- renderUI({
    req(Country_2())
    selectInput("Country_2", "Please select the countries", choices = c("All countries", Country_2()),multiple = TRUE,selected = "All countries")
  })
  country_forcast_2 <- reactive({
    req(year_forcast_2())
    d_filtered <- year_forcast_2()
    
    if (!("All countries" %in% input$Country_2)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_2)
    }
    return(data.frame(d_filtered))
  })
  country_sales_2 <- reactive({
    req(year_sales_2())
    d_filtered <- year_sales_2()
    
    if (!("All countries" %in% input$Country_2)) {
      d_filtered <- d_filtered %>% filter(Country %in% input$Country_2)
    }
    return(data.frame(d_filtered))
  })
  
  Manufacturer_2 <- reactive({
    req(country_forcast_2())
    unique(country_forcast_2()["Manufacturer"])
  })
  output$Manufacturer_ui_2 <- renderUI({
    req(Manufacturer_2())
    selectInput("Manufacturer_2", "Please select the manufacturers", choices = c("All manufacturers",Manufacturer_2()),multiple = TRUE,selected = "All manufacturers")
  })
  manufacturer_forcast_2 <- reactive({
    req(country_forcast_2())
    d_filtered <- country_forcast_2()
    
    if (!("All manufacturers" %in% input$Manufacturer_2 )) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_2)
    }
    return(data.frame(d_filtered))
  })
  manufacturer_sales_2 <- reactive({
    req(country_sales_2())
    d_filtered <- country_sales_2()
    
    if (!("All manufacturers" %in% input$Manufacturer_2 )) {
      d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer_2)
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
  
  # Forcast plots ---------------------------------------------------------------
  
  data_plot_Year_Forcast <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net()) 
    
    data_temp <- forcast_target_Net() %>%
      group_by(!!!sym(input$V_Year_2)) %>% 
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE),
                Weighted = sum(Weighted, na.rm = TRUE),
                Value = sum(Value, na.rm=TRUE )) %>% 
      gather(type,out, -!!sym(input$V_Year_2))
    
    data_plot_Year_Forcast <- data_temp %>% arrange(!!sym(input$V_Year_2))
    
    return(data_plot_Year_Forcast)
  })
  output$plot_Year_Forcast <- renderPlotly({
    req(data_plot_Year_Forcast())
    data_plot_Year_Forcast <- data_plot_Year_Forcast() %>% mutate(!!sym(input$V_Year_2):=factor(!!!sym(input$V_Year_2),levels=seq(min(!!!sym(input$V_Year_2)),max(!!!sym(input$V_Year_2)),by=1)))
    
    p <- ggplot(data_plot_Year_Forcast,
                aes(x = !!sym(input$V_Year_2), y = out, fill = type, group = type)) +
      geom_bar(aes(y = out, fill = type), stat = "identity", position = "dodge") +
      geom_text(aes(x = !!sym(input$V_Year_2), y = out, label = scales::dollar(out),
                    tooltip = paste("Year: ", !!sym(input$V_Year_2), "\n", "Total: ", scales::dollar(out))),
                vjust = -0.25, position = position_dodge(0.9), check_overlap = TRUE) +
      scale_y_continuous(labels = function(x) scales::dollar(x))+
      theme(axis.text.x=element_text(angle=0, hjust=1),
            panel.background = element_blank(),
            strip.background = element_rect(colour="#99C0E8", fill="white"),
            legend.position = 'right'
      ) +
      labs(x = "Year", y = "", fill = "", color = "") +
      scale_fill_manual("", 
                        labels = c("Total Net", "Weighted", "Value"),
                        values = c('#D2691E','#FFA500','#808080')) 
    
    ggplotly(p, tooltip = "text")
    
  })
  
  
  data_plot_Manufacturer_Forcast <- eventReactive(input$calcButton_2,{
    req(forcast_target_percent()) 
    
    data_temp <- forcast_target_Net() %>%
      group_by(Manufacturer) %>% 
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE),
                Weighted = sum(Weighted, na.rm = TRUE),
                Value = sum(Value, na.rm=TRUE )) %>% 
      gather(type,out, -Manufacturer)
    
    data_plot_Manufacturer_Forcast <- data_temp
    
    return(data_plot_Manufacturer_Forcast)
  })
  output$plot_Manufacturer_Forcast <- renderPlotly({
    req(data_plot_Manufacturer_Forcast())
    data_plot_Manufacturer_Forcast <- data_plot_Manufacturer_Forcast()
    
    p <- ggplot(data_plot_Manufacturer_Forcast,
                aes(x = reorder(Manufacturer,-out), y = out, fill = type, group = type)) +
      geom_bar(aes(y = out, fill = type), stat = "identity", position = "dodge") +
      geom_text(aes(x = reorder(Manufacturer,-out), y = out, label = scales::dollar(out),
                    tooltip = paste("Manufacturer: ", reorder(Manufacturer,-out), "\n", "Total: ", scales::dollar(out))),
                vjust = -0.25, position = position_dodge(0.9), check_overlap = TRUE) +
      scale_y_continuous(labels = function(x) scales::dollar(x))+
      theme(axis.text.x=element_text(angle=0, hjust=1),
            panel.background = element_blank(),
            strip.background = element_rect(colour="#99C0E8", fill="white"),
            legend.position = 'right'
      ) +
      labs(x = "Manufacturer", y = "", fill = "", color = "") +
      scale_fill_manual("", 
                        labels = c("Total Net", "Weighted", "Value"),
                        values = c('#D2691E','#FFA500','#808080'))     
    ggplotly(p, tooltip = "text")
    
  })
  
  
  data_plot_Medicine_Forcast <- eventReactive(input$calcButton_2,{
    req(forcast_target_percent()) 
    
    data_temp <- forcast_target_percent() %>%
      group_by(Medicine)%>% 
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE),
                Weighted = sum(Weighted, na.rm = TRUE),
                Value = sum(Value, na.rm=TRUE )) %>% 
      gather(type,out, -Medicine)
    
    x <- unique(data_temp %>% top_n(30, out) %>% pull(Medicine))
    data_plot_Medicine_Forcast <- data_temp %>%  filter(Medicine %in% x)
    
    return(data_plot_Medicine_Forcast)
  })
  output$plot_Medicine_Forcast <- renderPlotly({
    req(data_plot_Medicine_Forcast())
    data_plot_Medicine_Forcast <- data_plot_Medicine_Forcast()
    
    p <- ggplot(data_plot_Medicine_Forcast,
                aes(x = reorder(Medicine, out), y = out, fill = type)) +
      geom_bar(aes(y = out, fill = type), stat = "identity", position = "dodge") +
      geom_text(aes(x = reorder(Medicine, out), y = out, fill = type, label = scales::dollar(out),
                    tooltip = paste("Medicine: ", Medicine, "\n", "Total: ", scales::dollar(out))),
                vjust = -0.25, position = position_dodge(0.9), check_overlap = TRUE) +
      scale_y_continuous(labels = function(x) scales::dollar(x))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            panel.background = element_blank(),
            strip.background = element_rect(colour = "#99C0E8", fill = "white"),
            legend.position = 'right'
      ) +
      labs(x = "Medicine", y = "", fill = "") +
      scale_fill_manual("", 
                        labels = c("Total Net", "Weighted", "Value"),
                        values = c('#D2691E','#FFA500','#808080')) +
      coord_flip()
    
    ggplotly(p, tooltip = "text") %>%
      layout(width = 1000, height = 900) %>% # Set the initial width and height
      htmlwidgets::onRender("
      function(el, x) {
        // Add CSS to set overflow and max-height
        el.style.overflowY = 'scroll';
        el.style.maxHeight = '500px'; // Set the maximum height as per your preference
      }
    ")
  })
  
  
  data_plot_Country_Forcast <- eventReactive(input$calcButton_2,{
    req(forcast_target_percent()) 
    
    data_temp <- forcast_target_percent() %>%
      group_by(Country)%>% 
      summarise(Total_Net = sum(Total_Net, na.rm = TRUE),
                Weighted = sum(Weighted, na.rm = TRUE),
                Value = sum(Value, na.rm=TRUE )) %>% 
      gather(type,out, -Country)
    
    x <- unique(data_temp %>% top_n(30, out) %>% pull(Country))
    data_plot_Country_Forcast <- data_temp %>%  filter(Country %in% x)
    
    return(data_plot_Country_Forcast)
  })
  output$plot_Country_Forcast <- renderPlotly({
    req(data_plot_Country_Forcast())
    data_plot_Country_Forcast <- data_plot_Country_Forcast()
    
    p <- ggplot(data_plot_Country_Forcast,
                aes(x = reorder(Country, out), y = out, fill = type)) +
      geom_bar(aes(y = out, fill = type), stat = "identity", position = "dodge") +
      geom_text(aes(x = reorder(Country, out), y = out, fill = type, label = scales::dollar(out),
                    tooltip = paste("Country: ", Country, "\n", "Total: ", scales::dollar(out))),
                vjust = -0.25, position = position_dodge(0.9), check_overlap = TRUE) +
      scale_y_continuous(labels = function(x) scales::dollar(x))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            panel.background = element_blank(),
            strip.background = element_rect(colour = "#99C0E8", fill = "white"),
            legend.position = 'right'
      ) +
      labs(x = "Country", y = "", fill = "") +
      scale_fill_manual("", 
                        labels = c("Total Net", "Weighted", "Value"),
                        values = c('#D2691E','#FFA500','#808080')) +
      coord_flip()
    
    ggplotly(p, tooltip = "text") %>%
      layout(width = 1000, height = 900) %>% # Set the initial width and height
      htmlwidgets::onRender("
      function(el, x) {
        // Add CSS to set overflow and max-height
        el.style.overflowY = 'scroll';
        el.style.maxHeight = '500px'; // Set the maximum height as per your preference
      }
    ")
  })
  
  
  data_plot_Medicine_Forcast_Net <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net()) 
    
    data_temp <- forcast_target_Net() %>% 
      group_by(Medicine,Forcast) %>% 
      summarise(out= sum(Total_Net,na.rm=TRUE)) %>% 
      rename(type=Forcast) %>% ungroup()
    x <- unique(data_temp %>% top_n(30, out) %>% pull(Medicine))
    data_plot_Medicine_Forcast_Net <- data_temp %>%  filter(Medicine %in% x)
    
    return(data_plot_Medicine_Forcast_Net)
  })
  output$plot_Medicine_Forcast_Net <- renderPlotly({
    req(data_plot_Medicine_Forcast_Net())
    data_plot_Medicine_Forcast_Net <- data_plot_Medicine_Forcast_Net() %>% 
      mutate(type=factor(type,levels=c("Predicted","Unpredicted")))
    
    p <- ggplot(data_plot_Medicine_Forcast_Net,
                aes(x = reorder(Medicine, out), y = out, fill = type)) +
      geom_bar(aes(y = out, fill = type), stat = "identity", position = "dodge") +
      geom_text(aes(x = reorder(Medicine, out), y = out, fill = type, label = scales::dollar(out),
                    tooltip = paste("Medicine: ", Medicine, "\n", "Total: ", scales::dollar(out))),
                vjust = -0.25, position = position_dodge(0.9), check_overlap = TRUE) +
      scale_y_continuous(labels = function(x) scales::dollar(x))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            panel.background = element_blank(),
            strip.background = element_rect(colour = "#99C0E8", fill = "white"),
            legend.position = 'right'
      ) +
      labs(x = "Medicine", y = "", fill = "") +
      scale_fill_manual(values = c('#FFA500','#808080')) +
      coord_flip()
    
    ggplotly(p, tooltip = "text") %>%
      layout(width = 1400, height = 900) %>% # Set the initial width and height
      htmlwidgets::onRender("
      function(el, x) {
        // Add CSS to set overflow and max-height
        el.style.overflowY = 'scroll';
        el.style.maxHeight = '500px'; // Set the maximum height as per your preference
      }
    ")
  })
  
  
  data_plot_Manufacturer_Forcast_Net <- eventReactive(input$calcButton_2,{
    req(forcast_target_Net()) 
    
    data_temp <- forcast_target_Net() %>% 
      group_by(Manufacturer,Forcast) %>% 
      summarise(out= sum(Total_Net,na.rm=TRUE)) %>% 
      rename(type=Forcast) %>% ungroup()
    
    data_plot_Manufacturer_Forcast_Net <- data_temp 
    
    return(data_plot_Manufacturer_Forcast_Net)
  })
  output$plot_Manufacturer_Forcast_Net <- renderPlotly({
    req(data_plot_Manufacturer_Forcast_Net())
    data_plot_Manufacturer_Forcast_Net <- data_plot_Manufacturer_Forcast_Net()
    
    p <- ggplot(data_plot_Manufacturer_Forcast_Net,
                aes(x = reorder(Manufacturer, -out), y = out, fill = type)) +
      geom_bar(aes(y = out, fill = type), stat = "identity", position = "dodge") +
      geom_text(aes(x = reorder(Manufacturer, -out), y = out, fill = type, label = scales::dollar(out),
                    tooltip = paste("Manufacturer: ", Manufacturer, "\n", "Total: ", scales::dollar(out))),
                vjust = -0.25, position = position_dodge(0.9), check_overlap = TRUE) +
      scale_y_continuous(labels = function(x) scales::dollar(x))+
      theme(axis.text.x = element_text(angle = 0, hjust = 1),
            panel.background = element_blank(),
            strip.background = element_rect(colour = "#99C0E8", fill = "white"),
            legend.position = 'right'
      ) +
      labs(x = "Manufacturer", y = "", fill = "") +
      scale_fill_manual(values = c('#FFA500','#808080')) 
    # scale_color_discrete("", labels = c("Total Net"))+
    # coord_flip()
    
    ggplotly(p, tooltip = "text")  %>%
      layout(width = 1400, height = 400)
  })
  
  # Forcast tables ---------------------------------------------------------------
  
  
  library(reactable)
  library(dplyr)
  library(RColorBrewer)
  
  output$data_2 <- renderReactable({
    req(forcast_target_Net())
    
    data <- forcast_target_Net() %>% 
      arrange(desc(Value), desc(Total_Net)) %>% 
      mutate(across(c("Total_Net", "Weighted", "Value"), 
                    ~ formatC(.x, format = "f", digits = 0, flag = "", big.mark = ",", small.mark = "")))
    
    reactable(
      data,
      pagination = TRUE,
      defaultPageSize = 15,
      pageSizeOptions = c(10, 20, 50),
      columns = list(
        Percent_Weighted = colDef(
          style = function(value) {
            color <- cut(
              as.numeric(value),
              breaks = c(-Inf, 30, 50, 70, 100, Inf),
              labels = brewer.pal(5, "RdYlGn"),
              include.lowest = TRUE
            )
            list(backgroundColor = color)
          }
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
      paste("Forcast-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(forcast_target_Net()
                , file, row.names = FALSE)
    }
  )
  
  
  
  
  
}
# Run the app
shinyApp(ui, server)

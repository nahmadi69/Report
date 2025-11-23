# server_filter_module.R

# Server module for filtering export data with multiple reactive filters
export_filter_module_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    filtered_data <- reactive({
      d_filtered <- data()

      # Category filter
      if (!is.null(input$Category) && !('All Categories' %in% input$Category)) {
        d_filtered <- d_filtered %>% filter(Category %in% input$Category)
      }

      # Year filter
      if (!is.null(input$Year) && !('All years' %in% input$Year)) {
        d_filtered <- d_filtered %>% filter(!!sym(input$V_Year) %in% input$Year)
      }

      # Month filter
      if (!is.null(input$Month) && !('All Months' %in% input$Month)) {
        d_filtered <- d_filtered %>% filter(!!sym(input$V_Month) %in% input$Month)
      }

      # Manufacturer filter
      if (!is.null(input$Manufacturer) && !('All manufacturers' %in% input$Manufacturer)) {
        d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer)
      }

      # Country filter
      if (!is.null(input$Country) && !('All countries' %in% input$Country)) {
        d_filtered <- d_filtered %>% filter(Country %in% input$Country)
      }

      # Consignee filter
      if (!is.null(input$Consignee) && !('All consignees' %in% input$Consignee)) {
        d_filtered <- d_filtered %>% filter(Consignee %in% input$Consignee)
      }

      # Medicine filter
      if (!is.null(input$Medicine) && !('All medicines' %in% input$Medicine)) {
        d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine)
      }

      # Dosage filter
      if (!is.null(input$Dosage) && !('All Dosages' %in% input$Dosage)) {
        d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage)
      }

      d_filtered
    })

    return(filtered_data)
  })
}


# Modules for UI inputs using filtered data choices
export_filter_module_ui <- function(id, data) {
  ns <- NS(id)
  tagList(
    pickerInput(
      ns("Category"), label = "Please select the Categories",
      choices = unique(data()[["Category"]]), selected = unique(data()[["Category"]]),
      options = list(`actions-box` = TRUE), multiple = TRUE
    ),
    selectInput(ns("V_Year"), "Please select the year variable", choices = colnames(data()), selected = "Gregorian_year"),
    uiOutput(ns("Year_ui")),
    selectInput(ns("V_Month"), "Please select the Month variable", choices = colnames(data()), selected = "Gregorian_month"),
    uiOutput(ns("Month_ui")),
    selectInput(ns("Manufacturer"), "Please select the manufacturers", choices = unique(data()[["Manufacturer"]]), multiple = TRUE, selected = unique(data()[["Manufacturer"]])),
    pickerInput(ns("Country"), "Please select the countries", choices = unique(data()[["Country"]]), selected = unique(data()[["Country"]]), options = list(`actions-box` = TRUE), multiple = TRUE),
    pickerInput(ns("Consignee"), "Please select the consignees", choices = unique(data()[["Consignee"]]), selected = unique(data()[["Consignee"]]), options = list(`actions-box` = TRUE), multiple = TRUE),
    selectInput(ns("Medicine"), "Please select the medicines", choices = unique(data()[["Medicine"]]), multiple = TRUE, selected = unique(data()[["Medicine"]])),
    selectInput(ns("Dosage"), "Please select the dosages", choices = unique(data()[["Dosage"]]), multiple = TRUE, selected = unique(data()[["Dosage"]]))
  )
}


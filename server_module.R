# server_module.R

# This module encapsulates filtering for data_1 (export data).
filter_data_module_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    filtered_data <- reactive({
      d_filtered <- data()
      if (!("All Categories" %in% input$Category)) {
        d_filtered <- d_filtered %>% filter(Category %in% input$Category)
      }
      if (!("All years" %in% input$Year)) {
        d_filtered <- d_filtered %>% filter(!!sym(input$V_Year) %in% input$Year)
      }
      if (!("All Months" %in% input$Month)) {
        d_filtered <- d_filtered %>% filter(!!sym(input$V_Month) %in% input$Month)
      }
      if (!("All manufacturers" %in% input$Manufacturer)) {
        d_filtered <- d_filtered %>% filter(Manufacturer %in% input$Manufacturer)
      }
      if (!("All countries" %in% input$Country)) {
        d_filtered <- d_filtered %>% filter(Country %in% input$Country)
      }
      if (!("All consignees" %in% input$Consignee)) {
        d_filtered <- d_filtered %>% filter(Consignee %in% input$Consignee)
      }
      if (!("All medicines" %in% input$Medicine)) {
        d_filtered <- d_filtered %>% filter(Medicine %in% input$Medicine)
      }
      if (!("All Dosages" %in% input$Dosage)) {
        d_filtered <- d_filtered %>% filter(Dosage %in% input$Dosage)
      }
      d_filtered
    })

    return(filtered_data)
  })
}

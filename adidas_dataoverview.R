adidas_dataoverview_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(title="Data Overview", status = "primary", width=12, dataTableOutput(ns("dataoverview")))
    ),
    fluidRow(
      box(title= "Quick Analysis", status = "primary", width=12,
          column(width=4,selectizeInput(ns('category'), label="Choose the Determinant for your Quick Analysis", choices = c("Retailer", "Product", "Region"), multiple = FALSE)))
    ),
    
    fluidRow(
      box(title = "Result of your Quick Analysis (in Mio $)", status = "primary", width = 6, dataTableOutput(ns("datafiltered"))),
      box(title = "Visualisation of your Quick Analysis (in Mio $)", status = "primary", width = 6, plotlyOutput(ns("datavisualisation")))
    )
  )
  
}


adidas_dataoverview <- function(input, output, session) {
  

# Overview ----------------------------------------------------

  
  #Creating a Datatable which shows selected columns of the Raw Data
  output$dataoverview <- renderDataTable({
    
    temp <- adidas %>%
      select(Retailer, `Invoice Date`, City, Product, `Units Sold`, `Total Sales`, `Operating Margin`)
    
    DT::datatable(temp, filter='bottom', options=list(pageLength=10))
    
  })
  
  

# Quick Analysis ----------------------------------------------

  #Data Preparation: Grouping the data based on the chosen category (Retailer, Product or Region) and summarizing Revenue and Profit.
  
  datafiltered_input <- reactive({
    
    temp <- adidas%>%
            group_by_at(input$category) %>%
            summarize(Profit = round(sum(`Operating Profit`)/1000000,2), Revenue = round(sum(`Total Sales`)/1000000,2)) %>%
            ungroup()
    
    temp <- temp %>% mutate(Margin = round(Profit/Revenue,2))

  
    
    return(temp)
      
  })
  
  #Data Table: Visualizing a data table based on the prepared data
  
  output$datafiltered <- renderDataTable({
    
    temp <- datafiltered_input()
    
    DT::datatable(temp, filter='bottom', options=list(pageLength=6))
    
    
  })
  
  #Visualization: Visualizing the prepared data in a bar chart
  
  output$datavisualisation <- renderPlotly({ 
    
    temp <- datafiltered_input()
      
    
    plot_ly(data = temp, x = ~get(input$category)) %>% 
      add_trace(y=~Revenue, type ='bar', name = "Revenue") %>%
      add_trace(y=~Profit, type = "bar", name = "Profit") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""), showlegend = TRUE)
    
  })
  
  
}
  









adidas_descriptive_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(title="Choice", status = "primary", width=12,
          column(width=4, selectizeInput(ns('Retailer'), label="Retailer", choices = NULL, multiple = TRUE)),
          column(width = 4, selectizeInput(ns("Product"), label = "Product", choices = NULL, multiple = TRUE)),
          column(width = 4, selectizeInput(ns("KPI"), label = "Which KPI?", choices = c("Revenue","Profit", "Units Sold"), multiple = FALSE)))
    ),
    fluidRow(
      column(width = 12, verbatimTextOutput(ns("adidas_intro_descriptive")))
    ),
    fluidRow(
      box(title = "Trend", status = "primary", width=8,plotlyOutput(ns("trend"))),
      box(title="Sales Method", status = "primary", width=4, plotlyOutput(ns("method")))
    ),
    
    fluidRow(
      box(title = "States", status = "primary", width=12,plotlyOutput(ns("states")))
    )
  )
  
}


adidas_descriptive <- function(input, output, session) {
  

# Text  -------------------------------------------------------------------

#Rendering a Text, depending on the chosen KPI 

  output$adidas_intro_descriptive <- renderText({
    
    selected_kpi <- input$KPI
    
    if (selected_kpi == "Revenue") {
      text <- "This report analyzes the Revenue of Adidas."}
    else if (selected_kpi == "Profit") {
      text <- "This report analyzes the Profit of Adidas."}
    else {
      text <- "This report analyzes the Units Sold by Adidas"}

    HTML(text)
    
  })
  
  

# Filter Boxes ------------------------------------------------------------


  updateSelectizeInput(session, 'Retailer', choices=unique(adidas$Retailer), server = TRUE)
  updateSelectizeInput(session, "Product", choices=unique(adidas$Product), server = TRUE)
  
  

# Reactive Variables ------------------------------------------------------

  #Updating the Variables based on changes in the input for Retailer and Product, allowing dynamic filtering.
  
  selected <- reactiveValues(Retailer = NULL,
                             Product = NULL)
  
  filtered <- reactiveValues(Retailer = unique(adidas$Retailer),
                             Product = unique(adidas$Product))
  
  observeEvent(eventExpr = input$Retailer, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Retailer <- input$Retailer
    filtered$Retailer <- if(is.null(selected$Retailer)) unique(adidas$Retailer) else selected$Retailer
  })
  
  observeEvent(eventExpr = input$Product, ignoreNULL = FALSE, ignoreInit = TRUE, {
    selected$Product <- input$Product
    filtered$Product <- if(is.null(selected$Product)) unique(adidas$Product) else selected$Product
  })
  


# Trend_Analysis ----------------------------------------------------------

  #Data Preparation: Filtering the data depending on the chosen Retailers and Products and summarising the chosen KPI (Revenue, Profit or Units Sold)
  data_input_salestrend <- reactive({
    
    selected_kpi <- switch(input$KPI, 
                          "Revenue" = "Total Sales",
                          "Profit" = "Operating Profit",
                          "Units Sold" = "Units Sold")
    
    temp <- adidas %>%
      filter(Retailer %in% filtered$Retailer, Product %in% filtered$Product) %>%
      group_by(Month)%>%
      summarise(total = sum(get(selected_kpi))) %>%
      ungroup()
    
    return(temp)

  })
  
  #Data Visualization: Adding a Regression line and visualizing the Data which was prepared before in a line chart
  output$trend <- renderPlotly({
    
    temp <- data_input_salestrend()
    
    temp_2 <- temp %>% 
      mutate(Regression = fitted(lm(data = temp, total ~ Month)))
    
    plot_ly(temp_2,x = ~Month) %>%
      add_lines(y=~total, name = input$KPI)%>%
      add_lines(y=~Regression, name = "Regression") %>%
      layout(xaxis = list(title = "Months"), yaxis = list(title = input$KPI), showlegend = TRUE)
    
  })
  

# Sales Method Analysis ----------------------------------------------------
  
  #Data Preparation: Filtering the data depending on the chosen Retailers and Products and summarising the chosen KPI (Revenue, Profit or Units Sold) 

  data_input_salesmethod <- reactive({
    
    selected_kpi <- switch(input$KPI, 
                           "Revenue" = "Total Sales",
                           "Profit" = "Operating Profit",
                           "Units Sold" = "Units Sold")
    
    temp <- adidas %>%
      filter(Retailer %in% filtered$Retailer, Product %in% filtered$Product) %>%
      group_by(`Sales Method`)%>%
      summarise(total = sum(get(selected_kpi))) %>%
      ungroup()
    
    return(temp)
    
  })
  
  #Data Visualization: Visualizing the Data which was prepared before in a pie chart
  output$method <- renderPlotly({ 
    
    temp <- data_input_salesmethod()
    
    plot_ly() %>% 
      add_pie(data = temp, labels = ~`Sales Method`, values= ~total, name= 'Sales per Category', rotation=90) %>%
      layout(title='', showlegend=TRUE, legend = list(font=list(size=12)))
    
  })
  ##Overall, the data_input_salestrend reactive expression filters the adidas data based on the selected retailers and products and 
  ##summarizes the chosen KPI for each month. The resulting dataset, temp, can be used for further trend analysis and visualization in the 
  ##Shiny application.

# Region Analysis ---------------------------------------------------------
  
  #Data Preparation: Filtering the data depending on the chosen Retailers and Products and summarising the chosen KPI (Revenue, Profit or Units Sold)
  data_input_region <- reactive({
    
    selected_kpi <- switch(input$KPI, 
                           "Revenue" = "Total Sales",
                           "Profit" = "Operating Profit",
                           "Units Sold" = "Units Sold")
    
    temp <- adidas %>%
      filter(Retailer %in% filtered$Retailer, Product %in% filtered$Product) %>%
      group_by(State)%>%
      summarise(total = sum(get(selected_kpi))) %>%
      ungroup()
    
    return(temp)
    
  })
  
  #Data Visualization: Visualizing the Data which was prepared before in a bar chart
  output$states <- renderPlotly({ 
    
    temp <- data_input_region()
    
    
    plot_ly(data = temp, x = ~State) %>% 
      add_trace(y=~total, type ='bar', name = input$KPI) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""), showlegend = TRUE)
  })
  
  
}
 
##Overall, the data_input_region reactive expression filters the adidas data based on the selected retailers and products and summarizes 
##the chosen KPI for each region (state). The resulting dataset, temp, can be used for further region analysis and visualization in the
## Shiny application.


server <- function(input, output){
  
  renderTable <- function(){
    filteredStockData <- stockData
      #Filter data by user exchange selection
    if(length(input$exchange_select)){
      filteredStockData <- subset(filteredStockData, Exchange %in% input$exchange_select)
    }
    #Filter data by user sector selection
    if(length(input$sectors_select)){
      filteredStockData <- subset(filteredStockData, Sector %in% input$sectors_select)
    }
    #Filter data by user industries selection
    if(length(input$industries_select)){
      filteredStockData <- subset(filteredStockData, Industry %in% input$industries_select)
    }
    
    output$stocks <- DT::renderDataTable(filteredStockData, options = list(
      pageLength = 10,
      initComplete = JS("function(settings, json) {console.log('Done.');}")
    ))
  }
  
  # output$names_select <- renderUI({
  #   
  #   filteredStockData <- stockData
  #   #Filter data by user exchange selection
  #   if(length(input$exchange_select)){
  #     filteredStockData <- subset(filteredStockData, Exchange %in% input$exchange_select)
  #   }
  #   #Filter data by user sector selection
  #   if(length(input$sectors_select)){
  #     filteredStockData <- subset(filteredStockData, Sector %in% input$sectors_select)
  #   }
  #   #Filter data by user industries selection
  #   if(length(input$industries_select)){
  #     filteredStockData <- subset(filteredStockData, Industry %in% input$industries_select)
  #   }
  # 
  #   namedStocks <- filteredStockData$Symbol
  #   names(namedStocks) = paste(filteredStockData$Symbol, filteredStockData$Name, sep=" - ")
  #   
  #   selectInput(
  #     inputId = "names", 
  #     label = "Choose Stocks",
  #     choices = namedStocks,
  #     multiple = TRUE
  #    )
  # })

  observeEvent({
    input$exchange_select
    input$sectors_select
    input$industries_select
    }, {
    renderTable()
  }, ignoreNULL = FALSE)
  
  reactive(renderTable())
}
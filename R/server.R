# Helpful Resources:
## https://gist.github.com/wch/4026749
## https://gist.github.com/wch/5436415/
server <- function(input, output, session){
  
  # Create an environment for storing data
  symbol_env <- new.env()
  
  require_symbol <- function(symbol, envir = parent.frame()) {
    if (is.null(envir[[symbol]])) {
      envir[[symbol]] <- getSymbols(symbol, auto.assign = FALSE)
    }
    
    envir[[symbol]]
  }
  
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
    #Filter data by user last sale selection
    if(length(input$lastSale_select)){
      filteredStockData <- subset(filteredStockData, LastSale >= input$lastSale_select[1] & LastSale <= input$lastSale_select[2])
    }
    
    output$stocks <- DT::renderDataTable(filteredStockData, options = list(
      pageLength = 10,
      initComplete = JS("function(settings, json) {console.log('Done.');}")
    ))
  }
  
  observeEvent({
    input$exchange_select
    input$sectors_select
    input$industries_select
    input$lastSale_select
    }, {
    renderTable()
  }, ignoreNULL = FALSE)
  
  #Reset filter inputs to initial state
  observeEvent(input$reset_filter, {
    updateSelectInput(session, inputId = "exchange_select", selected = NA)
    updateSelectInput(session, inputId = "sectors_select", selected = NA)
    updateSelectInput(session, inputId = "industries_select", selected = NA)
    updateSliderInput(session, inputId = "lastSale_select", value = c(min(lastSale, na.rm = TRUE), max(lastSale, na.rm = TRUE)))
  })
  
  output$stocks_plot <- renderPlot({
    
    #Get the timeseries for selected rows
    s = input$stocks_rows_selected
    lastSelectSymbol = symbols[s[length(s)]]

    if(length(s)){
      symbol_data <- require_symbol(lastSelectSymbol, symbol_env)
    
      chartSeries(
         symbol_data,
          name = lastSelectSymbol,
          type = input$chart_type,
          theme = "white"
      )
    }
  })
  
  
  #Needed to render table on start
  reactive(renderTable())
}
  # Helpful Resources:
  ## https://gist.github.com/wch/4026749
  ## https://gist.github.com/wch/5436415/
  server <- function(input, output, session){
  
  # Create an environment for storing data
  symbol_env <- new.env() 
  
  require_symbol <- function(symbol, envir = parent.frame()) { #Holt sich für jedes Symbol die TimeSeries Daten! Nur die Daten ohne Rendern
    if (is.null(envir[[symbol]])) {
      envir[[symbol]] <- getSymbols(symbol, auto.assign = FALSE)
    }
    #ELI oder hier kann der Vektor generiert werden. 
    envir[[symbol]]
  
  }
  
  searchResult<- reactive({
    #symbols 
    input$stocks_rows_selected
    subset(stockData, stockData$Symbol %in% shList)
  })
  

  
  data <- reactive({ #Hier wird die Tabelle in Tab 1 entsprechend der Kriterien gefiltert
    filteredStockData <- stockData #Lokale Kopie von StockData Tabelle
    #Filter data by user exchange selection
    if(length(input$exchange_select)){ #exchange select ist von der UI definiert so heisst das Auswahlfeld
       filteredStockData <- subset(filteredStockData, Exchange %in% input$exchange_select) #nehme nur jene wo in der Spate Exchange etwas dabei ist was in der var exchange_select drinnen ist. 
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
    #Filter data by user IPO selection
    if(length(input$ipoYear_select)){
      filteredStockData <- subset(filteredStockData, (IPOyear >= input$ipoYear_select[1] & IPOyear <= input$ipoYear_select[2]) | is.na(IPOyear))
    }

  })
  #Output ist die Tabelle links in Tab1 aber diese aktualisiert sich automatisch aufgrund der Reaktivität
  output$stocks <- DT::renderDataTable(data(), options = list( 
    pageLength = 10,
    initComplete = JS("function(settings, json) {console.log('Done.');}")
  ))
  
  #Reset filter inputs to initial state
  observeEvent(input$reset_filter, { #Listened auf den Rest Button - bei Klick wird ALLES zurpckgesetzt
    updateSelectInput(session, inputId = "exchange_select", selected = NA) #updateselect input ist ne fertig std. Methode und löscht in allesn INPUTS die gesetzten Informationen
    updateSelectInput(session, inputId = "sectors_select", selected = NA)
    updateSelectInput(session, inputId = "industries_select", selected = NA)
    updateSliderInput(session, inputId = "lastSale_select", value = c(min(lastSale, na.rm = TRUE), max(lastSale, na.rm = TRUE))) #den setzt er auf die gesamte Range von min-max
    updateSliderInput(session, inputId = "ipoYear_select", value = c(min(ipoYear, na.rm = TRUE), max(ipoYear, na.rm = TRUE))) #den setzt er auf die gesamte Range von min-max
#    renderTable() Wird nicht benötigt da reaktiv
  })
  observeEvent(input$calculate_portfolio, { #Listened auf den Rest Button - bei Klick wird ALLES zurpckgesetzt
    #Hier kommt der Aufruf der Markowitz Methode zum tragen 
    # read closing prices from Yahoo keeping only the closing prices
    
    ClosingPricesRead <- NULL
    for (Ticker in shList)
      ClosingPricesRead <- cbind(ClosingPricesRead,
                                 getSymbols.yahoo(Ticker, from="1950-01-01", verbose=FALSE, auto.assign=FALSE)[,6]) # [,6] = keep the adjusted prices
    
    # keep only the dates that have closing prices for all tickers
    
    ClosingPrices <- ClosingPricesRead[apply(ClosingPricesRead,1,function(x) all(!is.na(x))),]
    
    # convert prices to daily returns
    
    returns <- as.timeSeries(tail(ClosingPrices,-1) / as.numeric(head(ClosingPrices,-1)) - 1)
    
    
    
    # calculate the efficient frontier
    
     Frontier <- portfolioFrontier(returns, constraints = "LongOnly")
     
     frontierWeights <- getWeights(Frontier)
     
     Spec = portfolioSpec()
     
    # constraints = "minW[0]=0.34"
    
    # plot frontier
   output$frontier <- renderPlot({ 
     Spec = portfolioSpec()
     setSolver(Spec) = "solveRshortExact"
     setTargetRisk(Spec) = .12
     constraints <- c("minW[1:length(shList)]=[input$minimum_weight]","maxW[1:length(shList)]=[input$maximum_weight]", "Short")
     effFrontierShort <- portfolioFrontier(returns, Spec, constraints = constraints)
     weights <- getWeights(effFrontierShort)
     # plot(Frontier,c(1,2,3,4))})  # can also call the plot routine so it only plots the frontier: plot(Frontier,1)

     plot(effFrontierShort, c(1, 2, 3))})
   
   output$frontierWeights <- renderPlot({
     Spec = portfolioSpec()
     setSolver(Spec) = "solveRshortExact"
     setTargetRisk(Spec) = .12
     constraints <- c("minW[1:length(shList)]=[input$minimum_weight]","maxW[1:length(shList)]=[input$maximum_weight]", "Short")
     effFrontierShort <- portfolioFrontier(returns, Spec, constraints = constraints)
     weights <- getWeights(effFrontierShort)
     #Plot Frontier Weights (Need to transpose matrix first)
     barplot(t(weights), main="Frontier Weights", col=cm.colors(ncol(weights)+2), legend=colnames(weights))
     
     effPortShort <- minvariancePortfolio(returns, Spec, constraints=constraints)
     optWeights <- getWeights(effPortShort)
     tanPortShort <- tangencyPortfolio(returns, Spec, constraints=constraints)
     tanWeights <- getWeights(tanPortShort)
     maxR <- maxreturnPortfolio(returns , Spec, constraints=constraints)
     maxWeights <- getWeights(maxR)
   })
   # plot Sharpe ratios for each point on the efficient frontier
   output$sharpe <- renderPlot({
     riskReturnPoints <- frontierPoints(Frontier) # get risk and return values for points on the efficient frontier
     annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                                    targetReturn=riskReturnPoints[,"targetReturn"] * 252)
     riskFreeRate <- 0
     plot((annualizedPoints[,"targetReturn"]-riskFreeRate) / annualizedPoints[,"targetRisk"], xlab="Point on efficient frontier", ylab="Sharpe ratio")
   })
   
   #Plot MVP Weights: Basic Graphs
   output$mvpWeight <- renderPlot({
     mvp <- minvariancePortfolio(returns, spec=portfolioSpec(), constraints="LongOnly")
     mvp
     tangencyPort <- tangencyPortfolio(returns, spec=portfolioSpec(), constraints="LongOnly")
     tangencyPort
     mvpweights <- getWeights(mvp)
     tangencyweights <- getWeights(tangencyPort)
     covRisk(returns, mvpweights)
     varRisk(returns, mvpweights, alpha = 0.05)
     cvarRisk(returns, mvpweights, alpha = 0.05)
     barplot(mvpweights, main="Minimum Variance Portfolio Weights", xlab="Assset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(weights))
   })
  })
  
  observeEvent(input$stocks_rows_selected, { #stocks_rows_selected ist standardvar von DT welche anzeigt wie stocks_* da DT bei uns stocks heisst

    s = input$stocks_rows_selected #Gibt eine Liste mit den ausgewählten Zeilennummern zurück 
    localVec <- vector() 
    removeUI( #RemoveUI Standard Funktion von Shiny
      selector = "#stocks_plot_placeholder .plotContainer", # CSS - Löscht die Charts weg stocks_plot_placeholder haben wir im UI als tag definiert
      multiple = TRUE #alle die er findet werden gelöscht 
    ) #Alle alten Charts löschen und neue generieren sobald etwas neues ausgewählt wurde 
    
    if(length(s)){ #s in 83 defniiert - also wenn zumindest eine Zeile gewählt ist dann geh rein 
      
      i <- 1
      
      while(i <= length(s)){

        #For whatever needed this is necessary
        local({
        
          localI <- i
 
          
          # <<- c(share,symbols[s[localI]])
          symbol <- symbols[s[localI]] #Liste aller CODES der gewählten Aktien auf Basis der Symbols liste die bisher nur die Zeilennummern hatte
          plotId <- paste0("plot", symbol) #Konkateniert mit plot+symbol-spalteninfo 
          #ELI - hier könnte der Vektor der verwendeten Anteile generiert werden.
          localVec[[localI]] <<- symbols[s[localI]]
          insertUI( #insertUI standard Funktion zum einfügen der Charts
            selector = "#stocks_plot_placeholder",
            # wrap element in a div with class for ease of removal
            ui = tags$div(
              plotOutput(plotId), #Standard Funktion generiert mit Platzhalter für jeden Chart da es dynamisch ist muss das in Server nicht in UI stehen
              class = "plotContainer" #das wird benötigt für RemoveUI später "#" steht für iD und "." für Klasse
            )
          )

          symbol_data <- require_symbol(symbol, symbol_env) #Funktion RequireSymbol in Zeile 9 definiert 
          
          output[[plotId]] <- renderPlot({ #Hier rendert er mir die Daten nachdem sie in require Symbol geladen wurden
            chartSeries( #Standard Funktion 
              symbol_data,
              name = symbol,
              type = input$chart_type, #ChartType = was wir auswählen (candle stick usw.)
              theme = "white"
            )
          })
        })
        shList <<- localVec

        i <- i + 1
      }
      
      output$vars <- renderUI({ 
        selectInput("share_selection", "Select your choice", searchResult()[,1])
      })

    }
  }, ignoreNULL = FALSE)
  
  }
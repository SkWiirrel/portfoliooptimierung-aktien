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
    
    envir[[symbol]]
  }
  
  renderTable <- function(){ #führt nach observEvent die Filterung aus
    filteredStockData <- stockData #Lokale Kopie von StockData Tabelle
      #Filter data by user exchange selection
    if(length(input$exchange_select)){
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
    
    output$stocks <- DT::renderDataTable(filteredStockData, options = list( 
      pageLength = 10,
      initComplete = JS("function(settings, json) {console.log('Done.');}")
    ))

    #superselector to assign to global variable
    symbols <<- filteredStockData$Symbol #Von den gefilterten Tabelle (filteredStockData) nur die Symbol-Spalte in form eine Liste namens "Symbols" speichern (zur späteren Anzeige der Charts)
  } #<<- da diese Variable global definiert wurde
  
  observeEvent({ #welche Inputs sollen observiert haben also sobald in diesen Inputs was passiert wird renderTable aufgerufen
    input$exchange_select
    input$sectors_select
    input$industries_select
    input$lastSale_select
    }, {
    renderTable() #Filtert die DT rechts abhängig von der Selektion
  }, ignoreNULL = FALSE) #auch wenn ich Information in einer Input Spalte lösche renderd er die Tabelle neu
  
  #Reset filter inputs to initial state
  observeEvent(input$reset_filter, { #Listened auf den Rest Button - bei Klick wird ALLES zurpckgesetzt
    updateSelectInput(session, inputId = "exchange_select", selected = NA) #updateselect input ist ne fertig std. Methode und löscht in allesn INPUTS die gesetzten Informationen
    updateSelectInput(session, inputId = "sectors_select", selected = NA)
    updateSelectInput(session, inputId = "industries_select", selected = NA)
    updateSliderInput(session, inputId = "lastSale_select", value = c(min(lastSale, na.rm = TRUE), max(lastSale, na.rm = TRUE))) #den setzt er auf die gesamte Range von min-max
    renderTable()
  })
  
  # output$stocks_preview_plot <- renderPlot({
  #   #Get the timeseries for selected rows
  #   s = input$stocks_rows_selected
  #   lastSelectSymbol = symbols[s[length(s)]]
  # 
  #   if(length(s)){
  #     symbol_data <- require_symbol(lastSelectSymbol, symbol_env)
  #   
  #     chartSeries(
  #       symbol_data,
  #       name = lastSelectSymbol,
  #       type = input$chart_type,
  #       theme = "white"
  #     )
  #   }
  # })
  
  
  observeEvent(input$stocks_rows_selected, { #stocks_rows_selected ist standardvar von DT welche anzeigt wie stocks_* da DT bei uns stocks heisst
    
    s = input$stocks_rows_selected #Gibt eine Liste mit den ausgewählten Zeilennummern zurück 
    
    removeUI( #RemoveUI Standard Funktion von Shiny
      selector = "#stocks_plot_placeholder .plotContainer", # CSS - Löscht die Charts weg stocks_plot:placeholder haben wir im UI als tag definiert
      multiple = TRUE #alle die er findet werden gelöscht 
    ) #Alle alten Charts löschen und neue generieren sobald etwas neues ausgewählt wurde 
    
    if(length(s)){ #s in 83 defniiert - also wenn zumindest eine Zeile gewählt ist dann geh rein 
      
      i <- 1
      
      while(i <= length(s)){
        
        #For whatever needed this is necessary
        local({
          
          localI <- i
        
          symbol <- symbols[s[localI]] #Liste aller CODES der gewählten Aktien auf Basis der Symbols liste die bisher nur die Zeilennummern hatte
          plotId <- paste0("plot", symbol) #Konkateniert mit plot+symbol-spalteninfo
          
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
        
        i <- i + 1
      }
    }
  }, ignoreNULL = FALSE)
  
  
  #Needed to render table on start
  #reactive(renderTable()) #
  }
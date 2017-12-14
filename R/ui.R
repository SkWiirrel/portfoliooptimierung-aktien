source("globals.R")


pageTitle <- "Portfoliooptimierung (Aktien)"

ui <- navbarPage( #Die ganz obere schwarze navigationsbar mit dem titel
    title = pageTitle,
    
    tabPanel("1 - Stock Selection",
             sidebarLayout(
               #headerPanel(""),
               #fixedRow(
               #3 Spalten breit muss der untere Code sein
               sidebarPanel(
                 #3,
                 #wellPanel(
                   tags$h4("Filters"),
                   selectInput(
                     inputId = "exchange_select",
                     label = "Exchange Market",
                     choices = exchange,
                     multiple = TRUE,
                     width = '100%'
                   ),
                   selectInput(
                     inputId = "sectors_select",
                     label = "Sectors",
                     choices = sectors[!is.na(sectors)],
                     #Alle Informationen aus der Spalte "Sectors" ausser jene mit NA
                     multiple = TRUE,
                     width = '100%'
                   ),
                   selectInput(
                     inputId = "industries_select",
                     label = "Industries",
                     choices = industries[!is.na(industries)],
                     #is.na(industries) na = not Available
                     multiple = TRUE,
                     width = '100%'
                   ),
                   sliderInput(
                     inputId = "lastSale_select",
                     label = "Last Sale",
                     min = min(lastSale, na.rm = TRUE),
                     max = max(lastSale, na.rm = TRUE),
                     value = c(min(lastSale, na.rm = TRUE), max(lastSale, na.rm = TRUE)),
                     width = '100%'
                   ),
                   sliderInput(
                     inputId = "ipoYear_select",
                     label = "Initial Public Offering (IPO)",
                     step = 1,
                     sep = '',
                     min = min(ipoYear, na.rm = TRUE),
                     max = max(ipoYear, na.rm = TRUE),
                     value = c(min(ipoYear, na.rm = TRUE), max(ipoYear, na.rm = TRUE)),
                     width = '100%'
                   ),
                   actionButton(
                     inputId = "reset_filter",
                     label = "Reset",
                     icon = icon("refresh"),
                     width = '100%',
                     class = "btn-primary"
                   ),
                   numericInput(
                     inputId = "wanted_interest", 
                     label="interest", 
                     value, 
                     min = NA, 
                     max = NA, 
                     step = 0.1,
                    width = '100%'
                   ),
                  htmlOutput("vars"),
                   actionButton(
                     inputId = "calculate_portfolio",
                     label = "Calculate",
                     icon = icon("refresh"),
                     width = '100%',
                     class = "btn-primary"
                   )
                 
               ),
               mainPanel(
                 #9,
                 #9 Spalte breit muss dieser Abschnitt sein (dort wo die aktiendaten drinnen sind)
                 DT::dataTableOutput("stocks")
                 #Platzhalter für die Datatable mit der Benennung "Stocks"
                 #plotOutput("stocks_preview_plot"),
               )
             )),
    tabPanel("2 - Stock Timeseries",
        
                 #wellPanel(
                   tags$h4("Chart Options"),
                   selectInput(
                     inputId = "chart_type",
                     label = "Type",
                     choices = c(
                       "Candlestick" = "candlesticks",
                       "Matchstick" = "matchsticks",
                       "Bar" = "bars",
                       "Line" = "line"
                     )
                   #)
                 ),
            
                 tags$div(id = 'stocks_plot_placeholder') #Tags - baut HTML Tags auf (div) und gibt ihm die Id - stocks_plot_placeholder zum nachträglichen ansprechen in Server
               
               
               ),
    tabPanel("3 - My Portfolio"),
    
    collapsible = FALSE,
    windowTitle = pageTitle,
    fluid = FALSE,
    
    header = tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "css/bootswatch.yeti.css"),
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "css/Footer-white.css"), #Reference: https://tutorialzine.com/2016/10/freebie-5-fantastic-bootstrap-footers
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "css/custom.css")
    ),
    
    footer = includeHTML("www/html/Footer-white.html")
  )
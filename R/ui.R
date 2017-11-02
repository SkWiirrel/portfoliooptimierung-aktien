source("globals.R")

pageTitle <-"Portfoliooptimierung (Aktien)"

ui <- fluidPage(
  
  title = pageTitle,
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "bootswatch.yeti.css"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "custom.css"
    )
  ),
  
  navbarPage(pageTitle),
  
  fluidRow(
    column(3,
       wellPanel(
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
          multiple = TRUE,
          width = '100%'
        ),
        selectInput(
          inputId = "industries_select",
          label = "Industries",
          choices = industries[!is.na(industries)],
          multiple = TRUE,
          width = '100%'
        ),
        sliderInput(
          inputId = "lastSale_select", 
          label = "Last Sale Range",
          min = min(lastSale, na.rm = TRUE), 
          max = max(lastSale, na.rm = TRUE),
          value = c(min(lastSale, na.rm = TRUE), max(lastSale, na.rm = TRUE)),
          width = '100%'
        ),
        actionButton(
          inputId = "reset_filter",
          label = "Reset",
          icon = icon("refresh"),
          width = '100%',
          class = "btn-primary"
        )
       ),
       wellPanel(
         tags$h4("Chart Options"),
         selectInput(
           inputId = "chart_type",
           label = "Type",
           choices = c("Candlestick" = "candlesticks",
                       "Matchstick" = "matchsticks",
                       "Bar" = "bars",
                       "Line" = "line")
         )
       )
    ),
    column(9,
       DT::dataTableOutput("stocks"),
       #plotOutput("stocks_preview_plot"),
       tags$div(id = 'stocks_plot_placeholder') 
    )
  )
)
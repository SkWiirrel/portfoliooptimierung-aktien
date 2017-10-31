source("globals.R")

ui <- fluidPage(
  
  title = "Portfoliooptimierung (Aktien)",
  
  h2("Portfoliooptimierung (Aktien)"),
  
  fluidRow(
    column(4,  
      selectInput(
        inputId = "exchange_select",
        label = "Choose Exchange Market",
        choices = exchange,
        multiple = TRUE
      )
    ),
    column(4, 
      selectInput(
        inputId = "sectors_select",
        label = "Choose Sectors",
        choices = sectors[!is.na(sectors)],
        multiple = TRUE
      )
    ),
    column(4,
      selectInput(
        inputId = "industries_select",
        label = "Choose Industries",
        choices = industries[!is.na(industries)],
        multiple = TRUE
      )
    )
  ),
  
  #htmlOutput("names_select"),
  
  verbatimTextOutput("rows_selected"),
  
  DT::dataTableOutput("stocks")
)
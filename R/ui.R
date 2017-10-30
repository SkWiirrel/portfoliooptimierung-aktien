source("globals.R")

ui <- fluidPage(
  
  selectInput(
    inputId = "exchange_select",
    label = "Choose Exchange Market",
    choices = exchange,
    multiple = TRUE
  ),
  
  selectInput(
  inputId = "sectors_select",
  label = "Choose Sectors",
    choices = sectors[!is.na(sectors)],
    multiple = TRUE
  ),
  
  selectInput(
    inputId = "industries_select",
    label = "Choose Industries",
    choices = industries[!is.na(industries)],
    multiple = TRUE
  ),
  
  #htmlOutput("names_select"),
  
  DT::dataTableOutput("stocks")
)
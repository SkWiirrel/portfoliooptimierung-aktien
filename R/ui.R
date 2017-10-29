source("globals.R")

ui <- fluidPage(
  selectInput(
    inputId = "sectors",
    label = "Choose Sectors",
    choices = sectors,
    multiple = TRUE
  ),
  selectInput(
    inputId = "names",
    label = "Choose Stocks",
    choices = paste(symbols, names, sep=" - "),
    multiple = TRUE
  )
)
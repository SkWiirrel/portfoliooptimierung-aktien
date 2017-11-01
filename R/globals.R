library(shiny)
library(DT) #http://rstudio.github.io/DT/shiny.html
library(quantmod) #Needed for retrieving timeseries
library(TTR) #Needed for the stockSymbols() function
library(rsconnect)


stockData <- stockSymbols()
exchange <- stockData$Exchange
names <- stockData$Name
industries <- stockData$Industry
lastSale <- stockData$LastSale
symbols <- stockData$Symbol
sectors <- stockData$Sector

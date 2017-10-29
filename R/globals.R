library(shiny)
library(quantmod) #Needed for retrieving timeseries
library(TTR) #Needed for the stockSymbols() function

stockData <- stockSymbols()
names <- stockData$Name
symbols <- stockData$Symbol
sectors <- stockData$Sector

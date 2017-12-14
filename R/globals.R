library(shiny)
library(shinydashboard)
library(DT) #http://rstudio.github.io/DT/shiny.html - Datatable - macht die Tabelle in der UI 
library(quantmod) #Needed for retrieving timeseries - Holt sich den Chart für alle Aktien
library(TTR) #Needed for the stockSymbols() function - TTR Daten werden in die DT reingeladen
library(rsconnect) #ShinyApps.io (publish packet) : 24h gratis pro Monat und dann zahlen  
library(timeSeries)
library(tseries)
library(fPortfolio)
library(caTools)


stockData <- stockSymbols() #Kommt vom TTR holt sich die ganzen Stock Data 
exchange <- stockData$Exchange #Zugriff auf die Spalten der StockData tabelle 
names <- stockData$Name
industries <- stockData$Industry
lastSale <- stockData$LastSale
ipoYear <- stockData$IPOyear
symbols <- stockData$Symbol
sectors <- stockData$Sector
shList <- vector()
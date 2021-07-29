#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(DT)
library(readr)
library(tidyverse)

# Set the path to the file.
location <- "./Data/"
# Set the file name.
fileName <- "2016 Electoral and Demographic Data - County Level.csv"

# Read in the data.
countyData <- read_csv(paste0(location, fileName),
                       col_types=cols())

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$tab <- renderDataTable({
        countyData
    })
    
    output$myHistogram <- renderPlot({
      histVar <- input$histVar
      bins <- input$bins
      histLogScale <- input$histLogScale
      
      if (histLogScale) {
        histPlot <- countyData %>%
          mutate(
            tempVar = pull(log(countyData[, histVar]))
          ) %>%
          ggplot(aes(tempVar)) + 
          geom_histogram(bins=bins, fill="purple", color="black") + 
          scale_x_continuous(paste0("ln(", histVar, ")")) +
          scale_y_continuous("Frequency") + 
          ggtitle(paste0("Histogram of ln(", histVar, ")"))
      } else {
        histPlot <- ggplot(countyData, aes_string(histVar)) + 
          geom_histogram(bins=bins, fill="purple", color="black") + 
          scale_y_continuous("Frequency") + 
          ggtitle(paste0("Histogram of ", histVar))
      }
      
      histPlot
    })
    
    return(output)
})

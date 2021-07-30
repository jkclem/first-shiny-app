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
    
    # Create the output plot for the Data Exploration tab.
    output$histogram <- renderPlot({
      # Extract the input variables associated with the histogram.
      histVar <- input$histVar
      bins <- input$bins
      histLogScale <- input$histLogScale
      
      # If the user wants the histogram of ln(variable), display that.
      if (histLogScale) {
        
        # Create the histogram for ln(variable).
        myPlot <- countyData %>%
          # Create a temporay column that is the natural log of the variable.
          mutate(
            tempVar = pull(log(countyData[, histVar]))
          ) %>%
          # Create the histogram.
          ggplot(aes(tempVar)) + 
          geom_histogram(bins=bins, fill="purple", color="black") + 
          # Add axis labels and a title.
          scale_x_continuous(paste0("ln(", histVar, ")")) +
          scale_y_continuous("Frequency") + 
          ggtitle(paste0("Frequency of Counties by ln(", histVar, ")"))
        
        # Otherwise, just return a histogram of the variable.
      } else {
        
        # Create the histogram for the variable.
        myPlot <- ggplot(countyData, aes_string(histVar)) + 
          geom_histogram(bins=bins, fill="purple", color="black") + 
          # Change the y-axis label and a title.
          scale_y_continuous("Frequency") + 
          ggtitle(paste0("Frequency of Counties by ", histVar))
      }
        
      myPlot
    })
    
    # Create the output plot for the Data Exploration tab.
    output$scatter <- renderPlotly({
      
      # Extract the input variables associated with the scatter plot.
      varXLogScale <- input$varXLogScale 
      varYLogScale <- input$varYLogScale
      varX <- input$varX 
      varY <- input$varY
      addRegression <- input$addRegression
      
      # Execute if both are on the natural log scale.
      if (varXLogScale & varYLogScale) {
        scatterPlot <- countyData %>%
          mutate(
            tempVarX = pull(log(countyData[, varX])),
            tempVarY = pull(log(countyData[, varY]))
          ) %>%
          ggplot(aes(tempVarX, tempVarY)) + 
          geom_point(color="purple", alpha=0.33) + 
          scale_x_continuous(paste0("log(", varX, ")")) +
          scale_y_continuous(paste0("log(", varY, ")")) + 
          ggtitle(paste0("Scatter Plot of log(", 
                         varX, ") vs. log(", varY, ")"))
        
        # Execute if only X is on the log scale.
      } else if (varXLogScale & !varYLogScale) {
        scatterPlot <- countyData %>%
          mutate(
            "tempVarX" = pull(log(countyData[, varX]))
          ) %>%
          ggplot(aes_string(x="tempVarX", y=varY)) + 
          geom_point(color="purple", alpha=0.33) + 
          scale_x_continuous(paste0("log(", varX, ")")) +
          scale_y_continuous(varY) + 
          ggtitle(paste0("Scatter Plot of log(", 
                         varX, ") vs. ", varY))
        
        # Execute if only Y is on the log scale.
      } else if (!varXLogScale & varYLogScale) {
        scatterPlot <- countyData %>%
          mutate(
            "tempVarY" = pull(log(countyData[, varY]))
          ) %>%
          ggplot(aes_string(x=varX, y="tempVarY")) + 
          geom_point(color="purple", alpha=0.33) + 
          scale_x_continuous(varX) +
          scale_y_continuous(paste0("log(", varY, ")")) + 
          ggtitle(paste0("Scatter Plot of ", 
                         varX, " vs. log(", varY, ")"))
        
        # Execute if both variables are "un-logged".
      } else {
        scatterPlot <- countyData %>%
          ggplot(aes_string(x=varX, y=varY)) + 
          geom_point(color="purple", alpha=0.33) + 
          scale_x_continuous(varX) +
          scale_y_continuous(varY) + 
          ggtitle(paste0("Scatter Plot of ", 
                         varX, " vs. ", varY))
      }
      
      # Add a smoothed regression line if the user wants it.
      if (addRegression) {
        scatterPlot <- scatterPlot +
          geom_smooth(color="grey20")
      }
      
      myPlot <- ggplotly(scatterPlot)
      
      
      myPlot
    })
    
    return(output)
})

###
#
###

library(shiny)
library(shinyWidgets)
library(DT)
library(readr)
library(tidyverse)
library(imager)
library(plotly)

# Set the path to the file.
location <- "./Data/"
# Set the file name.
fileName <- "2016 Electoral and Demographic Data - County Level.csv"

# Read in the data.
countyData <- read_csv(paste0(location, fileName),
                       col_types=cols())

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ###
  # Data Tab
  ###
  
  # Create a data table output.
  output$tab <- renderDataTable({
    
    # Extract the selected states, winner, and columns.
    selectedStates <- unlist(input$selectedStates)
    selectedWinner <- unlist(input$selectedWinner)
    selectedCols <- unlist(input$selectedCols)
    
    # Filter the data based on user input.
    countyData %>%
      filter(State %in% selectedStates,
             Winner %in% selectedWinner) %>%
      select(selectedCols)
    })
  
  ###
  # Data Exploration Tab
  ###
  
  # Create the output plot for the Data Exploration tab.
  output$histogram <- renderPlot({
    
    # Extract the input variables associated with the histogram.
    histVar <- input$histVar
    bins <- input$bins
    histLogScale <- input$histLogScale
    
    # Extract the selected states, winner, and columns.
    selectedStatesDE <- unlist(input$selectedStatesDE)
    selectedWinnerDE <- unlist(input$selectedWinnerDE)
    
    # Filter the data based on user input.
    filteredCountyData <- countyData %>%
      filter(State %in% selectedStatesDE,
             Winner %in% selectedWinnerDE)
    
    # If the user wants the histogram of ln(variable), display that.
    if (histLogScale) {
      
      # Create the histogram for ln(variable).
      myPlot <- filteredCountyData %>%
        # Create a temporay column that is the natural log of the variable.
        mutate(
          tempVar = pull(log(filteredCountyData[, histVar]))
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
      myPlot <- ggplot(filteredCountyData, aes_string(histVar)) + 
        geom_histogram(bins=bins, fill="purple", color="black") + 
        # Change the y-axis label and a title.
        scale_y_continuous("Frequency") + 
        ggtitle(paste0("Frequency of Counties by ", histVar))
    }
    
    # Display the plot.
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
    
    # Extract the selected states, winner, and columns.
    selectedStatesDE <- unlist(input$selectedStatesDE)
    selectedWinnerDE <- unlist(input$selectedWinnerDE)
    
    # Filter the data based on user input.
    filteredCountyData <- countyData %>%
      filter(State %in% selectedStatesDE,
             Winner %in% selectedWinnerDE)
    
    # Execute if both are on the natural log scale.
    if (varXLogScale & varYLogScale) {
      
      scatterPlot <- filteredCountyData %>%
        # Log transform both variables.
        mutate(
          logVarX = pull(log(filteredCountyData[, varX])),
          logVarY = pull(log(filteredCountyData[, varY]))
          ) %>%
        # Create the scatter plot with semi-opaque purple dots.
        ggplot(aes(logVarX, logVarY)) + 
        geom_point(color="purple", alpha=0.33) + 
        # Add axis labels and a title.
        scale_x_continuous(paste0("log(", varX, ")")) +
        scale_y_continuous(paste0("log(", varY, ")")) + 
        ggtitle(paste0("Scatter Plot of log(", 
                       varX, ") vs. log(", varY, ")"))
      
    # Execute if only X is on the log scale.
    } else if (varXLogScale & !varYLogScale) {
      
      scatterPlot <- filteredCountyData %>%
        # Log transform the X variable.
        mutate(
          "logVarX" = pull(log(filteredCountyData[, varX]))
          ) %>%
        # Create the scatter plot with semi-opaque purple dots.
        ggplot(aes_string(x="logVarX", y=varY)) + 
        geom_point(color="purple", alpha=0.33) + 
        # Add axis labels and a title.
        scale_x_continuous(paste0("log(", varX, ")")) +
        scale_y_continuous(varY) + 
        ggtitle(paste0("Scatter Plot of log(", 
                       varX, ") vs. ", varY))
      
    # Execute if only Y is on the log scale.
    } else if (!varXLogScale & varYLogScale) {
      
      scatterPlot <- filteredCountyData %>%
        # Log transform the Y variable.
        mutate(
          "logVarY" = pull(log(filteredCountyData[, varY]))
          ) %>%
        # Create the scatter plot with semi-opaque purple dots.
        ggplot(aes_string(x=varX, y="logVarY")) + 
        geom_point(color="purple", alpha=0.33) + 
        # Add axis labels and a title.
        scale_x_continuous(varX) +
        scale_y_continuous(paste0("log(", varY, ")")) + 
        ggtitle(paste0("Scatter Plot of ", 
                       varX, " vs. log(", varY, ")"))
    
    # Execute if both variables are "un-logged".
    } else {
      
      scatterPlot <- filteredCountyData %>%
        # Create the scatter plot with semi-opaque purple dots.
        ggplot(aes_string(x=varX, y=varY)) + 
        geom_point(color="purple", alpha=0.33) + 
        # Add axis labels and a title.
        scale_x_continuous(varX) +
        scale_y_continuous(varY) + 
        ggtitle(paste0("Scatter Plot of ", varX, " vs. ", varY))
    }
    
    # Add a smoothed regression line if the user wants it.
    if (addRegression) {
      # Make the smoothed regression line dark gray.
      scatterPlot <- scatterPlot +
        geom_smooth(color="grey20")
    }
    
    # Display the plotly plot.
    ggplotly(scatterPlot)
      
  })
  
  output$numericSummary <- renderDT({
    
    # Extract the selected states, winner, and columns.
    selectedStatesDE <- unlist(input$selectedStatesDE)
    selectedWinnerDE <- unlist(input$selectedWinnerDE)
    # Extract the variables to show.
    numericVars <- unlist(input$numericVars)
    
    # Filter for the rows of interest.
    filteredCountyData <- countyData %>%
      filter(
        State %in% selectedStatesDE,
        Winner %in% selectedWinnerDE
      ) %>%
      select(numericVars)
    
    numericSummary <- do.call(cbind, lapply(filteredCountyData, summary))
    
    as.data.frame(t(numericSummary))
    
  })
    
  return(output)
  
})

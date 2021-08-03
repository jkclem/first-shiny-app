###
# author: John Clements
# date: 07/28/2021
# purpose: Serve as the back-end for the app exploring demographics and
#          voting patterns at a county level.
###

library(shiny)
library(shinyWidgets)
library(DT)
library(readr)
library(tidyverse)
library(imager)
library(plotly)
library(caret)

# Set the path to the file.
location <- "./Data/"
# Set the file name.
fileName <- "2016 Electoral and Demographic Data - County Level.csv"

# Read in the data.
countyData <- read_csv(paste0(location, fileName),
                       col_types=cols())
# Convert Winner to a factor.
countyData %>%
  mutate(
    Winner = as.factor(Winner)
  )

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
  
  # Make the possibly subsetted data downloadable.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data.csv")
    },
    content = function(file) {
      write.csv(
        countyData %>%
          filter(State %in% input$selectedStates,
                 Winner %in% input$selectedWinner) %>%
          select(input$selectedCols), 
        file, 
        row.names = FALSE
        )
    }
  )
  
  ###
  # Data Exploration Tab
  ###
  
  # Create the output plot for the Data Exploration tab.
  output$histogram <- renderPlotly({
    
    # Create a histogram output.
    
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
    ggplotly(myPlot)
    
  })
  
  # Create the output plot for the Data Exploration tab.
  output$scatter <- renderPlotly({
    
    # Create a scatterplot output.
    
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
  
  output$numericSummaryTable <- renderDT({
    
    # Output a numeric summary table of the variables of interest.
    
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
  
  output$countiesWonTable <- renderDT({
    
    # Output a count of the the counties won along with the population and
    # population density of the area won.
    
    # Extract the selected states, and number of digits to round.
    selectedStatesDE <- unlist(input$selectedStatesDE)
    popDensRounding <- unlist(input$popDensRounding)
    
    # Count the counties won, the total population in those counties, and the
    # population density by each candidate in the states selected.
    countyData %>%
      filter(
        State %in% selectedStatesDE,
      ) %>%
      select(Winner, County, TotalPop) %>%
      group_by(Winner) %>%
      summarize(
        "Counties Won" = length(County),
        "Total Population" = sum(TotalPop)
      ) %>%
      mutate(
        "Population Density" = round(`Total Population` / `Counties Won`,
                                     popDensRounding)
      )
    
  })
  
  ###
  # Modeling - Training
  ###
  
  output$minKInput <- renderUI({
    
    # Create the input box for the min number of k in k-NN.
    
    # Find the smallest subsection of the data.
    minProp <- min(input$propTesting, 1/input$numFolds)
    # Find the maximum number of neighbors.
    maxK <- floor(nrow(countyData) * minProp)
    
    numericInput(
      inputId = "minK",
      label = "Min.", 
      min = 1, 
      max = maxK, 
      value = 1)
  })
  
  output$maxKInput <- renderUI({
    
    # Create the input box for the max number of k in k-NN.
    
    # Find the user's min k.
    minK <- input$minK
    # Find the smallest subsection of the data.
    minProp <- min(input$propTesting, 1/input$numFolds)
    
    # Set the max number of neighbors.
    maxK <- floor(nrow(countyData) * minProp)
    
    # Start at 21.
    value <- 21
    
    # If the minK is greater than 21, move it up.
    if (minK > value){
      value <- minK
    }
    
    numericInput(
      inputId = "maxK",
      label = "Max.", 
      min = minK, 
      max = maxK, 
      value = value)
  })
  
  observeEvent(input$trainStart, {
    
    # Test the performance of the three models.
    
    # Get the variables to use for each model.
    logRegVars <- unlist(input$logRegVars)
    knnVars <- unlist(input$knnVars)
    randForVars <- unlist(input$randForVars)
    
    # Get the random seed, proportion of testing, and k-folds params.
    randSeed <- input$randSeed
    propTesting <- input$propTesting
    numFolds <- input$numFolds
    
    # Get the number of Ks to try.
    minK <- input$minK
    maxK <- input$maxK
    numKs <- input$numKs
    ks <- round(seq(minK, maxK, length.out=numKs))
    
    # Get the random forest mtrys.
    randForMtry <- as.numeric(input$randForMtry)
    
    # Set the random seed.
    set.seed(randSeed)
    
    # Get the testing indexes.
    testInd <- sample(
      seq_len(nrow(countyData)), 
      size=floor(nrow(countyData)*propTesting)
      )
    
    # Split into training and testing sets.
    train <- countyData[-testInd, ]
    test <- countyData[testInd, ]
    
    suppressWarnings(library(caret))
    
    # Set the repeated CV params.
    TrControl <- trainControl(
      method="cv",
      number=numFolds
      )
    
    # Evaluate the logistic regression through CV.
    logRegModel = train(
      Winner ~ ., 
      data=train[, c(c("Winner"), logRegVars)],
      method = "glm",
      family = "binomial",
      metric="Accuracy",
      preProcess = c("center","scale"),
      trControl=TrControl
    )
    
    # Let caret choose the best kNN through CV.
    knnModel = train(
      Winner ~ ., 
      data=train[, c(c("Winner"), knnVars)],
      method="knn", 
      metric="Accuracy",
      tuneGrid=expand.grid(k = ks),
      preProcess = c("center","scale"),
      trControl=TrControl
    )
    
    # Let caret choose the best random forest through CV.
    rfModel = train(
      Winner ~ ., 
      data=train[, c(c("Winner"), randForVars)],
      method="rf", 
      metric="Accuracy",
      tuneGrid=expand.grid(mtry = randForMtry),
      preProcess = c("center","scale"),
      trControl=TrControl
      )
    
    # Get test set predictions.
    logRegPreds <- predict(logRegModel, test, type="raw")
    knnPreds <- predict(knnModel, test, type="raw")
    randForPreds <- predict(rfModel, test, type="raw")
    
    # Create the findMode function.
    findMode <- function(x) {
      # From https://www.tutorialspoint.com/r/r_mean_median_mode.htm
      uniqueX <- unique(x)
      uniqueX[which.max(tabulate(match(x, uniqueX)))]
    }
    
    # Find the no-info rate.
    noInfoRate <- mean(findMode(test$Winner) == test$Winner)
    # Find the test set performances.
    accVec <- c(
      noInfoRate,
      mean(logRegPreds == test$Winner, na.rm=TRUE), 
      mean(knnPreds == test$Winner, na.rm=TRUE), 
      mean(randForPreds == test$Winner, na.rm=TRUE)
      )
    
    # Convert to a matrix and percentages.
    accMatrix <- t(as.matrix(accVec)) * 100
    # Add informative column names.
    colnames(accMatrix) <- c(
      "No Information Rate",
      "Logistic Regression",
      paste0("k-NN (k = ", knnModel$bestTune$k, ")"),
      paste0("Random Forest (mtry = ", rfModel$bestTune$mtry, ")")
      )
    # Convert the matrix to a dataframe.
    accTable <- as.data.frame(accMatrix) %>%
      mutate_all(
        round, digits = 3
      ) %>%
      mutate_all(
        paste0, sep="%"
      )
    
    output$accTableOutput <- renderDataTable(
      datatable(accTable)
      )
  })
  
  return(output)
  
})

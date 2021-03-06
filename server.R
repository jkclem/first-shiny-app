###
# author: John Clements
# date: 07/28/2021
# purpose: Serve as the back-end for the app exploring demographics and
#          voting patterns at a county level in the 2016 election.
###

# Load in packages used in the analysis.
library(shiny)
library(shinyWidgets)
library(DT)
library(readr)
library(tidyverse)
library(imager)
library(plotly)
library(caret)
library(rattle)

# Set the file name.
fileName <- "./Data/2016 Electoral and Demographic Data - County Level.csv"

# Read in the data.
countyData <- read_csv(
  fileName,
  col_types=cols()
  )

# Convert Winner to a factor.
countyData %>%
  mutate(
    Winner = as.factor(Winner)
  )

# Set up the back-end.
shinyServer(function(input, output, session) {
  
  ###
  # Data Tab
  ###
  
  output$tab <- renderDataTable({
    
    ###
    # Create a data table output that the user can filter.
    ###
    
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
  
  
  output$downloadData <- downloadHandler(
    
    ###
    # Make the possibly subsetted data downloadable.
    ###
    
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
  
  output$histogram <- renderPlotly({
    
    ###
    # Create a histogram output.
    ###
    
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
  
  output$scatter <- renderPlotly({
    
    ###
    # Create a scatterplot output.
    ###
    
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
    
    ###
    # Output a numeric summary table of the variables of interest.
    ###
    
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
    
    ###
    # Output a count of the the counties won along with the population and
    # average population of the counties won.
    ###
    
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
        "Average Population" = round(`Total Population` / `Counties Won`,
                                     popDensRounding)
      )
    
  })
  
  
  ###
  # Modeling - Info
  ###
  
  output$logRegEx <- renderUI({
    
    ###
    # Show the mathematical formulation of Logistic Regression.
    ###
    
    withMathJax(
      helpText(
      "$$\\ln(\\frac{p_i}{1-p_i}) = \\beta_0 + \\Sigma^k_{j=1}\\beta_jx_{ij}$$"
        )
      )
  })
  
  
  ###
  # Modeling - Training
  ###
  
  output$minCpInput <- renderUI({
    
    ###
    # Create the input box for the min Cp for the tree.
    ###
    
    numericInput(
      inputId = "minCp",
      label = "Min.", 
      min = 0, 
      max = 1000, 
      value = 0.01
      )
  })
  
  output$maxCpInput <- renderUI({
    
    ###
    # Create the input box for the max number of Cp in the tree.
    ###
    
    # Find the user's min Cp.
    minCp <- input$minCp
    
    # Start at 21.
    value <- 10
    
    # If the minCp is greater than 21, move it up.
    if (minCp > value){
      value <- minCp
    }
    
    numericInput(
      inputId = "maxCp",
      label = "Max.", 
      min = minCp, 
      max = 1000, 
      value = value)
  })
  
  observeEvent(input$trainStart, {
    
    ###
    # Test the performance of the three models.
    ###
    
    # Create a Progress object
    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error.
    on.exit(progress$close())
    # Set the message to the user while cross-validation is running.
    progress$set(message = "Performing Cross-Validation", value = 0)
    
    # Get the variables to use for each model.
    logRegVars <- unlist(input$logRegVars)
    treeVars <- unlist(input$treeVars)
    randForVars <- unlist(input$randForVars)
    
    # Get the random seed, proportion of testing, and k-folds params.
    randSeed <- input$randSeed
    propTesting <- input$propTesting
    numFolds <- input$numFolds
    
    # Get the Cps to try.
    minCp <- input$minCp
    maxCp <- input$maxCp
    numCps <- input$numCps
    Cps <- seq(minCp, maxCp, length.out=numCps)
    
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
    
    # Suppress any warning in the fitting process.
    suppressWarnings(library(caret))
    
    # Set the repeated CV params.
    TrControl <- trainControl(
      method="cv",
      number=numFolds
      )
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.2, detail = "Logistic Regression")
    
    # Evaluate the logistic regression through CV.
    logRegModel = train(
      Winner ~ ., 
      data=train[, c(c("Winner"), logRegVars)],
      method = "glm",
      family = "binomial",
      metric="Accuracy",
      trControl=TrControl
    )
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.4, detail = "Classification Tree")
    
    # Let caret choose the best tree through CV.
    treeModel = train(
      Winner ~ ., 
      data=train[, c(c("Winner"), treeVars)],
      method="rpart", 
      metric="Accuracy",
      tuneGrid=expand.grid(cp = Cps),
      trControl=TrControl
    )
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.6, detail = "Random Forest")
    
    # Let caret choose the best random forest through CV.
    rfModel = train(
      Winner ~ ., 
      data=train[, c(c("Winner"), randForVars)],
      method="rf", 
      metric="Accuracy",
      tuneGrid=expand.grid(mtry = randForMtry),
      trControl=TrControl
      )
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.8, detail = "Evaluating Test Set Performance")
    
    # Get test set predictions.
    logRegPreds <- predict(logRegModel, test, type="raw")
    treePreds <- predict(treeModel, test, type="raw")
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
      mean(treePreds == test$Winner, na.rm=TRUE), 
      mean(randForPreds == test$Winner, na.rm=TRUE)
      )
    
    # Convert to a matrix and percentages.
    accMatrix <- t(as.matrix(accVec)) * 100
    # Add informative column names.
    colnames(accMatrix) <- c(
      "No Information Rate",
      "Logistic Regression",
      paste0("Tree (Cp = ", treeModel$bestTune$cp, ")"),
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
    
    # Create the output for the accuracy table.
    output$accTableOutput <- renderDataTable({
      datatable(accTable)
      })
    
    # Create an output for the logistic regression model rounding to 4 decimals.
    output$logRegSummary <- renderDataTable({
      round(as.data.frame(summary(logRegModel)$coef), 4)
      })
    
    # Create a nice tree diagram.
    output$treeSummary <- renderPlot({
      fancyRpartPlot(treeModel$finalModel)
    })
    
    # Create an output for the feature importance plot for random forest model.
    output$rfVarImpPlot <- renderPlot({
      ggplot(varImp(rfModel, type=2)) + 
        geom_col(fill="purple") + 
        ggtitle("Most Important Features by Decrease in Gini Impurity")
      })
    
    # Save the fitted models in a folder.
    saveRDS(logRegModel, "./Fitted Models/logRegModel.rds")
    saveRDS(treeModel, "./Fitted Models/treeModel.rds")
    saveRDS(rfModel, "./Fitted Models/rfModel.rds")
    
  })
  
  
  ###
  # Modeling - Prediction
  ###
  
  output$logRegPredInputs <- renderUI({
    
    ###
    # Create a UI that lets the user input values for the logistic regression 
    # and get a prediction.
    ###
    
    # Get the variables to use for each model.
    logRegVars <- input$logRegVars
    
    # Loop through the variables and create numeric input boxes for them. Use
    # the median of the variable for the default value.
    tags$ul(tagList(
      lapply(logRegVars, function(variable) {
        numericInput(
          inputId = paste0(variable, "Value"),
          label = paste0("Input ", variable, " Value"),
          value = median(pull(countyData[, variable]), na.rm=TRUE),
          step = 0.1
        )
      })
    ))
  })
  
  output$treePredInputs <- renderUI({
    
    ###
    # Create a UI that lets the user input values for the tree model and get a 
    # prediction.
    ###
    
    # Get the variables to use for each model.
    treeVars <- input$treeVars
    
    # Loop through the variables and create numeric input boxes for them. Use
    # the median of the variable for the default value.
    tags$ul(tagList(
      lapply(treeVars, function(variable) {
        numericInput(
          inputId = paste0(variable, "Value"),
          label = paste0("Input ", variable, " Value"),
          value = median(pull(countyData[, variable]), na.rm=TRUE),
          step = 0.1
        )
      })
    ))
  })
  
  output$randForPredInputs <- renderUI({
    
    ###
    # Create a UI that lets the user input values for the random forest model  
    # and get a prediction.
    ###
    
    # Get the variables to use for each model.
    randForVars <- input$randForVars
    
    # Loop through the variables and create numeric input boxes for them. Use
    # the median of the variable for the default value.
    tags$ul(tagList(
      lapply(randForVars, function(variable) {
        numericInput(
          inputId = paste0(variable, "Value"),
          label = paste0("Input ", variable, " Value"),
          value = median(pull(countyData[, variable]), na.rm=TRUE),
          step = 0.1
        )
      })
    ))
  })
  
  observeEvent(input$predStart, {
    
    ###
    # Return predictions when the user wants them.
    ###
    
    # Retrieve the model to use for prediction.
    modelType <- input$modelType
    
    # Load the appropriate model based on user input.
    if (modelType == "logReg"){
      
      # Get the names of the user inputs for the logistic regression model.
      varsOfInterest <- unlist(lapply(input$logRegVars, paste0, sep="Value"))
      # Load in the logistic regression model.
      myModel <- readRDS("./Fitted Models/logRegModel.rds")
      
    } else if (modelType == "tree"){
      
      # Get the names of the user inputs for the tree model.
      varsOfInterest <- unlist(lapply(input$treeVars, paste0, sep="Value"))
      # Load in the tree model.
      myModel <- readRDS("./Fitted Models/treeModel.rds")
      
    } else {
  
      # Get the names of the user inputs for the random forest model.
      varsOfInterest <- unlist(lapply(input$randForVars, paste0, sep="Value"))
      # Load in the random forest model.
      myModel <- readRDS("./Fitted Models/rfModel.rds")
      
    }
    
    # Loop through the user inputs adding them to a vector because you cannot
    # access the variables by simply passing the vector of list elements to 
    # input.
    inputCopy <- c()
    for (variable in varsOfInterest){
      inputCopy <- c(inputCopy, input[[variable]])
    }
    
    # Create a 1-row matrix of the user inputs.
    inputCopy <- t(matrix(inputCopy))
    # Strip "Value" from the variable names to match the column names in 
    # countyData.
    colnames(inputCopy) <- str_remove_all(varsOfInterest, pattern="Value")
    
    # Create a data.frame from the user inputs.
    userInputs <- as.data.frame(inputCopy)
    
    # Get class and probability predictions for the user inputs.
    classPred <- predict(myModel, userInputs, type="raw")
    probPred <- predict(myModel, userInputs, type="prob")
    # Combine them into a single matrix and round probabilities to 5 decimals.
    preds <- cbind(classPred, round(probPred, 5))
    # Add informative column names.
    colnames(preds) <- c(
      "Predicted Winner", 
      "Predicted Prob. of Clinton Win",
      "Predicted Prob. of Trump Win"
      )
    
    # Convert the preds matrix to a data.frame.
    preds <- as.data.frame(preds)
    
    # Return the predictions.
    output$preds <- renderDataTable({
      preds
    })
    
  })
  
  # Return the output.
  return(output)
  
})

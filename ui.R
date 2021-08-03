###
# author: John Clements
# date: 07/28/2021
# purpose: Create the front-end UI for the app exploring demographics and
#          voting patterns at a county level.
###

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(readr)
library(tidyverse)
library(plotly)

# Set the path to the file.
location <- "./Data/"
# Set the file name.
fileName <- "2016 Electoral and Demographic Data - County Level.csv"

# Get the location and name of the image for the About tab.
imageName <- paste0("2016_Nationwide_US_presidential_county_map_shaded_by_",
                    "vote_share.png")

# Read in the data.
countyData <- read_csv(paste0(location, fileName),
                       col_types=cols())

# Save strings with the link to the data sources.
acsDataLink <- "https://www.kaggle.com/muonneutrino/us-census-demographic-data"
voteDataLink <- paste0("https://github.com/tonmcg/", "
                       US_County_Level_Election_Results_08-20/blob/master/",
                       "2016_US_County_Level_Presidential_Results.csv")

# Define UI.
shinyUI(navbarPage(
    
    # Add a title.
    title = "2016 County-Level Demographics and Presidential Electoral Results",
    # Add a theme.
    theme = shinytheme("darkly"),
    
    # Create tabs for the different sections.
    tabsetPanel(
        
        # Create a tab for the about section.
        tabPanel(
            
            # Add a title.
            title="About",
            
            mainPanel(
                # Load in an image of US counties color coded by how they
                # voted in 2016.
                img(
                    src = imageName, 
                    height = '406px', 
                    width = '640px'
                    ),
                
                # Add a section telling the user what this app is for.
                h3("The Purpose of this App"),
                
                "This app explores the relationship between ",
                "demographic data and electoral outcomes in the ",
                "2016 presidential election at a county level.",
                
                
                # Add a section discussing the data.
                h3("The Data"),
                
                "The demographic data is from the 2015 American ",
                "Community Survey (ACS) 5-year estimates. I retrieved it ",
                "from  ",
                a(href = acsDataLink, "here"),
                ". I actually downloaded the census tract level data in ",
                "2019 and aggregated it up to the county level myself.",
                "The ACS includes demographic variables related to the ",
                "gender and ethnic composition, size, and economic ",
                "situation of an area.",
                
                # Add a line break.
                br(),
                br(),
                
                "The county level 2016 presidential vote shares were ",
                "taken from ",
                a(href=voteDataLink,
                  "here"),
                ". Unfortunately, there isn't county-level vote share ",
                "data available for Alaska in this data set. This ", 
                "analysis is restricted to the continental 48 + Hawaii.",
                
                
                # Add a section discussing the tabs.
                h3("The Tabs"),
                
                tags$ul(
                    tags$li(
                        "Data: Shows the raw data"
                        ), 
                    tags$li(
                        "Data Exploration: Visualizes and summarizes the ",
                        "data"
                        ), 
                    tags$li(
                        "Modeling: Evaluates various models on ", 
                        "predicting electoral outcomes"
                        )
                    ),
                
                # Add line breaks to extend the page.
                br(),
                br(),
                br()
                )
            ),
        
        # Create the Data page.
        tabPanel(
            
            # Add a title.
            title="Data",
                 
                 # Create a side panel.
                 sidebarPanel(
                     
                     # Create a filter for the states of interest.
                     selectInput(
                         inputId = "selectedStates",
                         label = "Filter by State(s)",
                         choices = unique(countyData$State),
                         selected = unique(countyData$State),
                         multiple = TRUE,
                         selectize = TRUE
                         ),
                     # Create a filter for the counties to display by winner.
                     selectInput(
                         inputId = "selectedWinner",
                         label = "Filter by Winner(s)",
                         choices = c("Clinton", "Trump"),
                         selected = c("Clinton", "Trump"),
                         multiple = TRUE,
                         selectize = TRUE
                     ),
                     # Create a filter for the columns to display.
                     selectInput(
                         inputId = "selectedCols",
                         label = "Filter Columns",
                         choices = colnames(countyData),
                         selected = colnames(countyData),
                         multiple = TRUE,
                         selectize = TRUE
                     ),
                     
                     sidebarPanel(downloadButton("downloadData", "Download"))
                 ),
            
            mainPanel(
                
                dataTableOutput(outputId = "tab")
                
                )
            ),
        
        tabPanel(
            
            title = "Data Exploration",
            
            sidebarPanel(
                     
                     # Set the width.
                     width = 4,
                     
                     # Add a header for this sidebar area.
                     h3("Universal Parameters"),
                     
                     # Create a filter for the states of interest.
                     selectInput(
                         inputId = "selectedStatesDE",
                         label = "Filter by State(s)",
                         choices = unique(countyData$State),
                         selected = unique(countyData$State),
                         multiple = TRUE,
                         selectize = TRUE
                     ),
                     # Create a filter for the counties to display by winner.
                     selectInput(
                         inputId = "selectedWinnerDE",
                         label = "Filter by Winner(s)",
                         choices = c("Clinton", "Trump"),
                         selected = c("Clinton", "Trump"),
                         multiple = TRUE,
                         selectize = TRUE
                     ),
                     
                     # Add a header for this sidebar portion.
                     h3("Visualization Parameters"),
                     
                     # Create buttons to choose the plot type.
                     radioButtons(
                         inputId = "plotType",
                         label = "Plot Type",
                         choiceValues = c("histogram", "scatterPlot"),
                         choiceNames = c("Histogram", "Scatter Plot"),
                         selected = "histogram",
                         inline = TRUE
                         ),
                     
                     # Only show this panel if the plot type is a histogram.
                     conditionalPanel(
                         condition = "input.plotType == 'histogram'",
                         
                         # Accept user inputs to modify the histogram.
                         
                         selectInput(
                             inputId = "histVar",
                             label = "Variable",
                             choices = colnames(countyData)[3:35]
                             ),
                         numericInput(
                             inputId = "bins",
                             label = "Number of Bins",
                             value = 30,
                             min = 5,
                             max = 100,
                             step = 5
                             ),
                         checkboxInput(
                             inputId = "histLogScale", 
                             label = "Apply Natural Logarithm",
                             value = FALSE
                             )
                         ),
                     
                     # Only show this panel if the plot type is a scatter plot.
                     conditionalPanel(
                         condition = "input.plotType == 'scatterPlot'",
                         
                         # Accept user inputs to modify the scatter plot.
                         
                         selectInput(
                             inputId = "varX",
                             label = "X Variable",
                             choices = colnames(countyData)[3:35]
                             ),
                         checkboxInput(
                             inputId = "varXLogScale",
                             label = "Apply Natural Logarithm to X",
                             value = FALSE
                             ),
                         selectInput(
                             inputId = "varY",
                             label = "Y Variable",
                             choices = colnames(countyData)[3:35],
                             selected = "PercentTrump"
                             ),
                         checkboxInput(
                             inputId = "varYLogScale",
                             label = "Apply Natural Logarithm to Y",
                             value = FALSE
                             ),
                         radioButtons(
                             inputId = "addRegression",
                             label = "Add Smoothed Regression Line",
                             choiceValues = c(TRUE, FALSE),
                             choiceNames = c("Yes", "No"),
                             selected = FALSE,
                             inline = TRUE
                             )
                         ),
                     
                     # Add a header for this sidebar portion.
                     h3("Summary Table Parameters"),
                     
                     # Choose whether to report numeric summaries of discrete.
                     radioButtons(
                         inputId = "summaryType",
                         label = "Summary Type",
                         choiceValues = c("numeric", "countiesWon"),
                         choiceNames = c("Numeric", "Counties Won"),
                         selected = "",
                         inline = TRUE
                     ),
                     
                     # Only show this panel if the summary type is numeric.
                     conditionalPanel(
                         condition = "input.summaryType == 'numeric'",
                         selectInput(
                             inputId = "numericVars",
                             label = "Variable(s) to Summarize",
                             choices = colnames(countyData)[3:35],
                             selected = colnames(countyData)[3:35],
                             multiple = TRUE,
                             selectize = TRUE
                             )
                         ),
                     # Only show this panel if the summary type is countiesWon.
                     conditionalPanel(
                         condition = "input.summaryType == 'countiesWon'",
                         numericInput(
                             inputId = "popDensRounding",
                             label = "Number of Digits for Pop. Density",
                             value = 2,
                             min = 0,
                             max = 10,
                             step = 1
                         )
                     )
                     ),
                 mainPanel(
                     conditionalPanel(
                         condition = "input.plotType == 'histogram'",
                         plotlyOutput("histogram")
                         ),
                     conditionalPanel(
                         condition = "input.plotType == 'scatterPlot'",
                         plotlyOutput("scatter")
                         ),
                     conditionalPanel(
                         condition = "input.summaryType == 'numeric'",
                         dataTableOutput("numericSummaryTable")
                         ),
                     conditionalPanel(
                         condition = "input.summaryType == 'countiesWon'",
                         dataTableOutput("countiesWonTable")
                         )
                     )
                 ),
        # Create the Data page.
        navbarMenu(
            
            # Add a title.
            title="Modeling",
            
            # Add text for the Modeling Info tab.
            tabPanel(
                title = "Modeling Info",
                mainPanel(fluidPage(
                    
                    br(),
                    
                    h4("Goals of the Modeling Secion"),
                    "The goal of the modeling section is to classify counties ",
                    "as voting for Trump or Clinton in the 2016 election ",
                    "based solely on their demographic characteristics. ",
                    "We will use 3 types of models: logistic regression, k-NN ",
                    "and a random forest.",
                    br(),
                    br(),
                    h4("Logistic Regression"),
                    "Logistic regression is a classification model that ",
                    "models the log-odds as a linear function of the variables. ",
                    "It assumes the form: ",
                    uiOutput("logRegEx"),
                    "Its linear form allows for interpretation, as the signs ",
                    "tell us if increasing values of a variable makes an ",
                    "outcome more or less likely.",
                    br(),
                    br(),
                    h4("k-NN"),
                    "k-NN, or k-Nearest Neighbors is a simple algorithm. We ",
                    "simply find the", tags$b("k"), "closest points to a new ",
                    "data point and take a majority vote to determine the ",
                    "class (or average the response for regression). ",
                    "Closeness is usually determined by Euclidean distance.",
                    br(),
                    br(),
                    "Because k-NN relies on distance metrics, it is prone to",
                    "the curse of dimensionality; points get farther apart ",
                    "and closeness loses its meaning in high dimensions. ",
                    "Thus, you need to keep the numbers of variables low ",
                    "or use dimensionality reduction on the data set for best ",
                    "performance. It is also not interpretable, because there ",
                    "are no parameters.",
                    br(),
                    br(),
                    h4("Random Forests"),
                    "Random forests create bootstrap samples of the training ",
                    "data and grow classification or regression trees on each ",
                    "sample. At each split, the trees are restricted to a ",
                    "subset of the features. For classification, the trees ",
                    "take a majority vote on the class of the new data. For ",
                    "regression, their predictions are averaged.",
                    br(),
                    br(),
                    "Only considering a subset of features at each split ",
                    "prevents a handful of features from dominating the early ",
                    "splits in each tree and makes each tree more independent ",
                    "(hopefully). By aggregating the predictions of the ",
                    "independent trees, we reduce the variance of the ",
                    "predictions. Random forests are typically good out-of-the",
                    "-box predictive models, but unfortunately lose the ",
                    "interpretability that stand-alone trees have.",
                    br(),
                    br(),
                    br()
                    ))
                ),
            
            tabPanel(
                
                # Add a title for the sub tab.
                title = "Model Fitting",
                
                # Allow the user to set a random seed between -1000 and 1000.
                sidebarPanel(
                    h3("Train-Test Split"),
                    numericInput(
                        inputId = "randSeed",
                        label = "Set a Random Seed",
                        value = 1,
                        min = -1000,
                        max = 1000,
                        step = 1
                    ),
                
                    # Allow the user to select the proportion of data to use for
                    # a testing set.
                    numericInput(
                        inputId = "propTesting",
                        label = "Proportion of Data to use for Test Set",
                        value = 0.2,
                        min = 0.1,
                        max = 0.5,
                        step = 0.05
                    ),
                    
                    
                    h3("Cross-Validation"),
                    # Set the number of folds.
                    div(
                        numericInput(
                            inputId = "numFolds",
                            label = "Number of Folds",
                            value = 3,
                            min = 3,
                            max = 5,
                            step = 1
                            ),
                        style="display:inline-block"
                    ),
                    
                    # Create a section for the logistic regression parameters.
                    h3("Logistic Regression Parameters"),
                    selectInput(
                        inputId = "logRegVars",
                        label = "Variables to Include:",
                        choices = colnames(countyData)[3:33],
                        selected = colnames(countyData)[3:33],
                        multiple = TRUE,
                        selectize = TRUE
                    ),
                    
                    # Create a section for the SVM parameters.
                    h3("k-NN Parameters"),
                    selectInput(
                        inputId = "knnVars",
                        label = "Variables to Include:",
                        choices = colnames(countyData)[3:33],
                        selected = colnames(countyData)[3:33],
                        multiple = TRUE,
                        selectize = TRUE
                    ),
                    
                    # Add side-by-side inputs to take in the k parameter.
                    h4(tags$b("k:")),
                    div(
                        uiOutput("minKInput"),  
                        style="display:inline-block"
                    ),
                    div(
                        uiOutput("maxKInput"),  
                        style="display:inline-block"
                    ),
                    div(
                        numericInput(
                            inputId = "numKs",
                            label = "# of Values", 
                            min = 1, 
                            max = 5, 
                            value = 3,
                            step = 1
                        ),  
                        style="display:inline-block"
                    ),
                    
                    # Create a section for the random forest parameters.
                    h3("Random Forest Parameters"),
                    # Let the user select which variables to use.
                    selectInput(
                        inputId = "randForVars",
                        label = "Variables to Include:",
                        choices = colnames(countyData)[3:33],
                        selected = colnames(countyData)[3:33],
                        multiple = TRUE,
                        selectize = TRUE
                    ),
                    # Let the user select the number of variables to consider
                    # at each split.
                    selectizeInput(
                        inputId = "randForMtry", 
                        label = "Select up to 5 values for mtry:", 
                        choices = 1:length(colnames(countyData)[3:33]),
                        multiple = TRUE,
                        selected = c(2, 6, 10),
                        options = list(maxItems = 5)
                        ),
                    # Add a button for fitting models.
                    actionButton(
                        inputId = "trainStart",
                        label = "Fit Models"
                        )
                    ),
                mainPanel(
                    # Show the test-set accuracies.
                    h3("Test Set Accuracies to 3 decimal places"),
                    dataTableOutput("accTableOutput"),
                    br(),
                    # Show the coefficients of the Logistic Regression Model.
                    h3("Summary of Logistic Regression Model"),
                    dataTableOutput("logRegSummary"),
                    br(),
                    h3("Random Forest Feature Importances"),
                    plotOutput("rfVarImpPlot")
                    )
                ),
            tabPanel(
                title = "Prediction",
                sidebarPanel(),
                mainPanel()
                )
            )
        )
    )
)



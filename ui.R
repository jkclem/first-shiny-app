###
#
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
                         selectize = FALSE,
                         size=5
                         ),
                     # Create a filter for the counties to display by winner.
                     selectInput(
                         inputId = "selectedWinner",
                         label = "Filter by Winner(s)",
                         choices = c("Clinton", "Trump"),
                         selected = c("Clinton", "Trump"),
                         multiple = TRUE,
                         selectize = FALSE,
                         size=2
                     ),
                     # Create a filter for the columns to display.
                     selectInput(
                         inputId = "selectedCols",
                         label = "Filter Columns",
                         choices = colnames(countyData),
                         selected = colnames(countyData),
                         multiple = TRUE,
                         selectize = FALSE,
                         size=8
                     )
                 ),
            
            mainPanel(
                dataTableOutput(outputId = "tab")
                )
            ),
        
        tabPanel(title = "Data Exploration",
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
                         selectize = FALSE,
                         size=5
                     ),
                     # Create a filter for the counties to display by winner.
                     selectInput(
                         inputId = "selectedWinnerDE",
                         label = "Filter by Winner(s)",
                         choices = c("Clinton", "Trump"),
                         selected = c("Clinton", "Trump"),
                         multiple = TRUE,
                         selectize = FALSE,
                         size=2
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
                             selectize = FALSE
                             )
                         )
                     ),
                 mainPanel(
                     conditionalPanel(
                         condition = "input.plotType == 'histogram'",
                         plotOutput("histogram")
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
                 )
        )
    )
)



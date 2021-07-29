###
#
###

library(shiny)
library(shinyWidgets)
library(shinythemes)
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

# Define UI.
shinyUI(navbarPage(
    
    title = "2016 County-Level Demographics and Presidential Electoral Results",
    
    theme = shinytheme("darkly"),
    
    tabsetPanel(
        tabPanel(title="About",
                 mainPanel(
                     fluidRow()
                     )
                ),
    tabPanel(title="Data",
             mainPanel(
                 dataTableOutput(outputId = "tab")
                 )
             ),
    tabPanel(title = "Data Exploration",
                 sidebarPanel(
                     radioButtons(
                         inputId = "plotType",
                         label = "Plot Type",
                         choiceValues = c("histogram", "scatterPlot"),
                         choiceNames = c("Histogram", "Scatter Plot"),
                         selected = "histogram",
                         inline = TRUE,
                     ),
                     # Only show this panel if the plot type is a histogram
                     conditionalPanel(
                         condition = "input.plotType == 'histogram'",
                         selectInput(
                             inputId = "histVar",
                             label = "Variable",
                             choices = colnames(countyData)[3:35]),
                         numericInput(inputId = "bins",
                                      label = "Number of Bins",
                                      value = 30,
                                      min = 5,
                                      max = 100,
                                      step = 5),
                         checkboxInput(inputId = "histLogScale", 
                                       label = "Apply Natural Logarithm", 
                                       value = FALSE)
                 
             )),
             mainPanel(
                 plotOutput("plot")
             )
             )
    )
    )
)



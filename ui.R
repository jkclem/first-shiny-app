

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

# Define UI.
shinyUI(navbarPage(
    
    titlePanel("Tabsets"),
    
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
             sidebarLayout(
                 sidebarPanel(
                     selectInput(
                         inputId = "histVar",
                         label = "Histogram Variables",
                         choices = colnames(countyData)[3:35]
                     ),
                     checkboxInput(inputId = "histLogScale", 
                                   label = "Log Scale", 
                                   value = FALSE
                     )
                 
             ),
             mainPanel(
                 plotOutput("myHistogram")
             )
             )
    )
    )
))



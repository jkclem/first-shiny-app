

library(shiny)

# Define UI.
shinyUI(navbarPage(
    
    titlePanel("Tabsets"),
    tabsetPanel(
        tabPanel(title="About",
            sidebarLayout(
            sidebarPanel(
                width = 2,
                checkboxGroupButtons(inputId = "show", 
                                     label = "Show column:", 
                                     choices = c("Column 1", "Column 2"))
            ),
            mainPanel(
                fluidRow(
                    column(6,
                           uiOutput("col1")),
                    column(6,
                           uiOutput("col2"))
                )))), 
        tabPanel("Data"), 
        tabPanel("Data Exploration"),
        tabPanel("Modeling")
                )
            )
        )
    )
)


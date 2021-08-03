# first-shiny-app

## Overview

This app explores demographic data from the 2015 American Community Survey and
2016 presidential election outcomes at county level.

## Required Packages

I used the following packages for this app:

- `shiny`: app framework
- `shinyWidgets`: additional functionality for Shiny
- `shinythemes`: prettying up the app
- `DT`: additional functionality for Shiny
- `readr`: reading in data
- `tidyverse`: data manipulation and visualization
- `plotly`: additional visualization functionality
- `imager`: working with images
- `caret`: machine learning

To install them all, run this code chunk:

```
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("shinythemes")
install.packages("DT")
install.packages("readr")
install.packages("tidyverse")
install.packages("plotly")
install.packages("imager")
install.packages("caret")
```

## Use the code below to run the app

```
shiny::runGitHub("jkclem/first-shiny-app", ref="main")
```

# libraries
library(shiny)
library(plotly)
library(quantmod)
library(fredr)
library(tidyverse)

# Source files
source("R/global.R")
source("R/ui.R")
source("R/server.R")

# Run the application
shinyApp(ui = ui, server = server)

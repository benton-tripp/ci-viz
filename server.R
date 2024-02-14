# Load libraries
suppressWarnings(
  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(dplyr)
    library(data.table)
    library(ggplot2)
    library(DT)
  })
)

# Set seed
set.seed(19)

# Source modules & utils
purrr::walk(list.files("modules", full.names=T), source)
purrr::walk(list.files("utils", full.names=T), source)

# Define server
server <- function(input, output, session) {
  
  # Define server environment variable
  server.env <- environment()
  
  # Load modules
  # <module.function>(input, output, session, server.env)
  apply.module(input, output, session, server.env)
  
  # Load initial plots/stats/data
  shinyjs::click("apply")
}
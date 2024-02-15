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
  
  # Starting `datasets` value
  datasets <- 1
  
  # Default parameters
  assign("mu", isolate(input$mu), envir=server.env)
  assign("s", isolate(input$sigma), envir=server.env)
  assign("cl", isolate(input$confLevel), envir=server.env)
  assign("n", isolate(input$sampleSize), envir=server.env)
  
  # Initiate samples
  samples <- purrr::map(1:100, ~{
    setnames(data.table(numeric(0)), "V1", paste0("n", .x))
  }) %>% do.call("cbind", .)
  
  # Initiate other variables 
  apply.from.ds <- F
  
  # Load modules
  # <module.function>(input, output, session, server.env)
  apply.module(input, output, session, server.env)
  datasets.module(input, output, session, server.env)
  plots.module(input, output, session, server.env)
  tables.module(input, output, session, server.env)
  
  # Load initial plots/stats/data
  shinyjs::click("apply")
}
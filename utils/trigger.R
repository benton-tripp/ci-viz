trigger.input <- function(input.name) {
  runjs(paste0('Shiny.setInputValue("',input.name, '", Math.random());'))
}
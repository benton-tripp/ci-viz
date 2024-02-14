apply.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  apply.env <- environment()
  
  # On apply button click
  observeEvent(input$apply, {
    js$loading()
    
    
    
    js$finishedLoading()
  }, ignoreInit=T, ignoreNULL=T)
}
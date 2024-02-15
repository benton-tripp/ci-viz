tables.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  tables.env <- environment()
  
  observeEvent(input$renderTables, {
    datasets <- get("datasets", server.env)
    samples <- get("samples", server.env)
    
    print(nrow(samples))
    js$finishedLoading()
  }, ignoreInit=T, ignoreNULL=T)
}
plots.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  plots.env <- environment()
  
  observeEvent(input$renderPlots, {
    datasets <- get("datasets", server.env)
    samples <- get("samples", server.env)
    
    # (Re)Generate Tables
    trigger.input("renderTables")
  }, ignoreInit=T, ignoreNULL=T)
}
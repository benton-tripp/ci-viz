apply.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  apply.env <- environment()
  
  # On apply button click
  observeEvent(input$apply, {
    js$loading()
    
    datasets <- get("datasets", server.env)
    samples <- get("samples", server.env)
    apply.from.ds <- get("apply.from.ds", server.env)

    if (!apply.from.ds) {
      mu <- isolate(input$mu)
      s <- isolate(input$sigma)
      cl <- isolate(input$confLevel)
      n <- isolate(input$sampleSize)
      # Update latest parameter selections
      assign("mu", mu, envir=server.env)
      assign("s", s, envir=server.env)
      assign("cl", cl, envir=server.env)
      assign("n", n, envir=server.env)
    } else {
      mu <- get("mu", server.env)
      s <- get("s", server.env)
      cl <- get("cl", server.env)
      n <- get("n", server.env)
      
      # Update samples
      new.samples <- datasets - nrow(samples)
      samples <- rbindlist(l=list(
        samples,
        purrr::map_df(1:new.samples, function(.x) {
          .ds <- paste0("n", 1:1e2) %>%
            set_names() %>%
            purrr::map2(., 1:1e2, ~list(rnorm(.y, mu, s))) %>%
            as.data.table()
        }) 
      ))
      # Reset `apply.from.ds`
      assign("apply.from.ds", F, server.env)
    }
    
    assign("samples", samples, server.env)
    
    # (Re)Generate Plots & Tables
    trigger.input("renderPlots")
  }, ignoreInit=T, ignoreNULL=T)
  
}
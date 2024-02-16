datasets.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  datasets.env <- environment()
  
  observeEvent(input$generate, {
    datasets <- get("datasets", server.env)
    new.ds <- isolate({
      case_when(
        input$generateDataCount=="1 New Dataset"~1,
        input$generateDataCount=="10 New Datasets"~10,
        input$generateDataCount=="100 New Datasets"~1e2,
        input$generateDataCount=="1000 New Datasets"~1e3,
        T~0
      )
    })
    datasets <- min(1e4, datasets + new.ds)
    assign("apply.from.ds", T, server.env)
    assign("datasets", datasets, envir=server.env)
    shinyjs::click("apply")
  }, ignoreInit=T, ignoreNULL=T)
  
  
  observeEvent(input$resetData, {
    samples <- purrr::map(1:100, ~{
      setnames(data.table(numeric(0)), "V1", paste0("n", .x))
    }) %>% do.call("cbind", .)
    
    assign("datasets", 1, envir=server.env)
    assign("samples", samples, envir=server.env)
    assign("apply.from.ds", T, server.env)
    shinyjs::click("apply")
  }, ignoreInit=T, ignoreNULL=T)
  
}
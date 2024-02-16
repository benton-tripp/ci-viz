tables.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  tables.env <- environment()
  
  observeEvent(input$renderTables, {
    datasets <- get("datasets", server.env)
    samples <- get("samples", server.env)
    dt <- get("dt", server.env)
    mu <- get("mu", server.env)
    s <- get("s", server.env)
    cl <- get("cl", server.env)
    n <- get("n", server.env)
    
    # Attribute 	      Statistic
    # ---------------------------
    # # of Samples 	    <value>
    # # Covering Mean 	<value>
    # Coverage % 	      <value>
    
    att.tbl <- dt[, .(
      `# of Samples`=.N, 
      `# Covering Mean`=sum(lg.icm), 
      `Coverage %`=paste0(round(100*sum(lg.icm)/.N, 3),"%")
    )] %>%
      data.table::transpose(keep.names="Attribute") %>%
      setnames("V1", "Statistic")
    
    output$att_table <- renderDT({
      view.datatable(att.tbl, pageLength=3, paging=F, info=F)
    })
    
    trigger.input("triggerSampleSummaries")
  }, ignoreInit=T, ignoreNULL=T)
  
}
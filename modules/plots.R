plots.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  plots.env <- environment()
  
  observeEvent(input$renderPlots, {
    datasets <- get("datasets", server.env)
    samples <- get("samples", server.env)
    mu <- get("mu", server.env)
    s <- get("s", server.env)
    cl <- get("cl", server.env)
    n <- get("n", server.env)
    
    
    # Create `dt` as the data, where it contains:
    # 'lower.ci' and 'upper.ci' columns for the C.I.'s,
    # 'mu' for the mean of each sample, and 'id' for 
    # the sample identifier
    dt <- samples[[paste0("n", n)]] %>%
      purrr::map2_df(.x=., .y=1:nrow(samples), ~{
        
        ci <- stats::t.test(.x, mu=mu, 
                            alternative="two.sided", 
                            conf.level=cl/100)
        lower.ci=ci$conf.int[1]
        upper.ci=ci$conf.int[2]
        icm <- lower.ci <= mu & mu <= upper.ci
        data.table(id=.y, yhat=mean, sd=s, 
                   lower.ci=lower.ci,
                   upper.ci=upper.ci,
                   interval.contains.mean=factor(icm, levels=c("FALSE", "TRUE")))
      })
    
    # C.I. Plot
    ci.plt <- ggplot(dt, aes(x=id, ymin=lower.ci, ymax=upper.ci,
                             color=interval.contains.mean)) +
      geom_hline(yintercept=mu, linewidth=1.2) +
      geom_errorbar(aes(y=mu), width = 0.1) +
      scale_color_manual(values = c("FALSE"="red", "TRUE"="black")) + 
      theme_minimal() +
      guides(fill="none", color="none")

    output$ci_plot <- renderPlot(ci.plt)
    # (Re)Generate Tables
    trigger.input("renderTables")
  }, ignoreInit=T, ignoreNULL=T)
}
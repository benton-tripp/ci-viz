sample.summary.module <- function(input, output, session, server.env) {
  # Initiate environment
  ns <- session$ns
  ss.env <- environment()
  
  observeEvent(c(input$triggerSampleSummaries, input$sampleDataset), {
    
    samples <- get("samples", server.env)
    n <- get("n", server.env)
    
    if (!is.null(input$sampleDataset)) {
  
      samp <- input$sampleDataset
      .x <- samples[samp][[paste0("n", n)]][[1]]
      sample.dt <- data.table(
        x=.x
      )
      
      # Histogram of the distribution of sample means
      # Show vertical lines to indicate where larger or smaller values 
      # yields an interval that doesn't capture the mean
      sample.dist <- ggplot(data=sample.dt, aes(x=x, y=after_stat(density))) +
        geom_histogram(fill="#edf0f7", color="black", 
                       bins = min(floor(n/2), 20)) + 
        theme_bw() +
        theme(
          axis.title.x=element_text(size=rel(1.3)),  
          axis.title.y=element_text(size=rel(1.3)), 
          axis.text.x=element_text(size=13), 
          axis.text.y=element_text(size=13) 
        ) +
        labs(y="Density", x="Sample", 
             title=paste0("Distribution of Sample #", samp))
      
      output$sample_hist <- renderPlot(sample.dist)
      
      # Selected Sample Summary Table
      
      # Name 	  Value
      # -------------
      # Min 	  <value>
      # Q1 	    <value>
      # Median 	<value>
      # Q3 	    <value>
      # Max 	  <value>
      # Mean 	  <value>
      # SD 	    <value>
      # 
      # Calculated Interval: (<lower>, <upper>)
      q <- quantile(.x) 
      s.tbl <- data.table(
        Measure=c("Min", "Q1", "Median", "Q3", "Max", "Mean", "SD"),
        Value=sapply(c(q, mean(.x), sd(.x)), round, 3)
      ) %>%
        setnames(old="Value", new=paste("Sample #", samp))
      output$s_table <- renderDT({
        view.datatable(s.tbl, pageLength=7, paging=F, info=F)
      })
    }
    
    print(paste0("Total Samples: ", nrow(samples), 
                 "; Selected Sample: ", samp))
    js$finishedLoading()
    
  }, ignoreInit=T, ignoreNULL=T)
}
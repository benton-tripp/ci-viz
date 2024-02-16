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
        data.table(id=.y, yhat=mean(.x), sd=sd(.x), 
                   lower.ci=lower.ci,
                   upper.ci=upper.ci,
                   lg.icm=icm,
                   interval.contains.mean=factor(icm, 
                                                 levels=c("FALSE", "TRUE")))
      })
    
    # C.I. Plot
    ci.plt <- ggplot(dt, aes(x=id, ymin=lower.ci, ymax=upper.ci,
                             color=interval.contains.mean)) +
      geom_errorbar(aes(y=mu), width=0.1) +
      geom_hline(yintercept=mu, linewidth=1.2, color="darkblue") +
      scale_color_manual(values=c("FALSE"="red", "TRUE"="black")) + 
      guides(fill="none", color="none") +
      theme_bw() +
      labs(x="Sample #", y=paste0("Sample ", cl, "% C.I.")) +
      theme(
        axis.title.x=element_text(size=rel(1.5)),  
        axis.title.y=element_text(size=rel(1.5)), 
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15) 
      ) +
      scale_x_continuous(limits=ci.plt.break.lims(dt),
                         breaks=ci.plt.breaks(dt$id),
                         labels=function(b) {
                           ifelse(b==0 | b==nrow(dt) + 1, 
                                  "", as.character(as.integer(b)))
                         })
    
    output$ci_plot <- renderPlot(ci.plt)
    
    # Sorted C.I. Plot
    dt$diff.means <- mu - dt$yhat
    dt[, composite.rank := frank(diff.means * (ifelse(lg.icm, 1, 1e3)))]
    
    sorted.plt <- ggplot(dt, aes(x=composite.rank, 
                                 ymin=lower.ci, ymax=upper.ci,
                                 color=interval.contains.mean)) +
      geom_errorbar(aes(y=mu), width=0.1) +
      geom_point(aes(y=yhat)) +
      geom_hline(yintercept=mu, linewidth=1.2, color="darkblue") +
      scale_color_manual(values=c("FALSE"="red", "TRUE"="black")) + 
      guides(fill="none", color="none") +
      theme_bw() +
      labs(x=paste("Samples - Ranked by Coverage of True Mean, and",
                   "\nDifference of Sample Mean and True Mean"), 
           y=paste0("Sample ", cl, "% C.I.")) +
      theme(
        axis.title.x=element_text(size=rel(1.5)),  
        axis.title.y=element_text(size=rel(1.5)), 
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15) 
      ) +
      scale_x_continuous(limits=ci.plt.break.lims(dt),
                         breaks=ci.plt.breaks(dt$id),
                         labels=function(b) {
                           ifelse(b==0 | b==nrow(dt) + 1, 
                                  "", as.character(as.integer(b)))
                         })
    output$sorted_plot <- renderPlot(sorted.plt)
    
    # Cumulative Mean Coverage Plot
    dt$cumulative.mean.coverage <- cumsum(dt$lg.icm) / seq_along(dt$lg.icm)
    
    if (nrow(dt) > 1) {
      cdist.plt <- ggplot(dt, aes(x=id, y=cumulative.mean.coverage)) + 
        geom_line(linewidth=1.1) 
    } else {
      cdist.plt <- ggplot(dt, aes(x=id, y=cumulative.mean.coverage)) + 
        geom_point(size=2)
    }
    cdist.plt <- cdist.plt + 
      geom_hline(yintercept=cl/100, linetype="dashed", color="blue",
                 linewidth=1.1) +
      # scale_y_continuous(trans=scales::pseudo_log_trans(exp(1)),
      #                    limits=c(0,1)) +
      theme_bw() +
      # annotate("text", label=paste("µ =", mu), x=length(lg.icm)+0.5, y=0.99, size=6) +
      labs(x="Sample #", y="Cumulative C.I. Coverage\nof the Mean") +
      theme(
        axis.title.x=element_text(size=rel(1.5)),  
        axis.title.y=element_text(size=rel(1.5)), 
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15) 
      ) +
      scale_x_continuous(limits=ci.plt.break.lims(dt),
                         breaks=ci.plt.breaks(dt$id),
                         labels=function(b) {
                           ifelse(b==0 | b==nrow(dt) + 1, 
                                  "", as.character(as.integer(b)))
                         })
    
    output$cdist_plot <- renderPlot(cdist.plt)
    
    # Running C.I. Coverage Plot
    rci.plt <- ggplot(data=dt, aes(x=id, color=interval.contains.mean)) 
    if (nrow(dt) > 1) {
      rci.plt <- rci.plt +
        geom_ribbon(aes(ymin=lower.ci, ymax=upper.ci), 
                    fill="#bbbbbb", alpha=0.2, color="#aaaaaa") +
        geom_hline(yintercept=mu, linewidth=1.1, color="darkblue") +
        geom_line(aes(y=yhat), color="black", linewidth=0.35)
    } else {
      rci.plt <- rci.plt +
        geom_hline(yintercept=mu, linewidth=1.1, color="darkblue")
    }
    rci.plt <- rci.plt +
      geom_point(aes(y=yhat, alpha=interval.contains.mean), size=1.75) +
      geom_segment(aes(x=id, xend=id, y=lower.ci, yend=upper.ci,
                       alpha=interval.contains.mean, 
                       color=interval.contains.mean), linewidth=0.75) +
      #geom_point(aes(y=lower.ci, alpha=interval.contains.mean), size=2) +
      #geom_point(aes(y=upper.ci, alpha=interval.contains.mean), size=2) +
      theme_bw() +
      scale_color_manual(values=c("FALSE"="red", "TRUE"="black")) +
      scale_alpha_manual(values=c("FALSE"=1, "TRUE"=0)) +
      guides(fill="none", color="none", alpha="none") +
      labs(x="Sample #", 
           y=paste0("Sample Mean ", cl, 
                    "% C.I.\nRunning Coverage")) +
      theme(
        axis.title.x=element_text(size=rel(1.5)),  
        axis.title.y=element_text(size=rel(1.5)), 
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15) 
      ) +
      scale_x_continuous(limits=ci.plt.break.lims(dt),
                         breaks=ci.plt.breaks(dt$id),
                         labels=function(b) {
                           ifelse(b==0 | b==nrow(dt) + 1, 
                                  "", as.character(as.integer(b)))
                         })
    if (nrow(dt) == 1 & dt$interval.contains.mean[[1]] != "FALSE") {
      rci.plt <- rci.plt +
        geom_segment(aes(x=id, xend=id, y=lower.ci, yend=upper.ci), 
                     linewidth=0.75, color="#999999") +
        geom_point(aes(y=yhat), color="black", size=1.75) 
    }
    output$rci_plot <- renderPlot(rci.plt)
    
    # Histogram of the distribution of sample means
    # Show vertical lines to indicate where larger or smaller values 
    # yields an interval that doesn't capture the mean
    sm.dist <- ggplot(data=dt, aes(x=yhat, y=after_stat(density))) +
      geom_histogram(fill="#edf0f7", color="black",
                     bins=max(1, min(floor(nrow(dt)/1.3), 30)))
    if (nrow(dt) >= 20) {
      sm.dist <- sm.dist + geom_density(color="darkred", 
                                        linetype="dashed", 
                                        linewidth=1.2)
    }
    sm.dist <- sm.dist + 
      theme_bw() +
      theme(
        axis.title.x=element_text(size=rel(1.5)),  
        axis.title.y=element_text(size=rel(1.5)), 
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15) 
      ) +
      labs(y="Density", x="Sample Means")
    
    
    output$sm_dist <- renderPlot(sm.dist)
    
    # QQ-Plot
    theoretical.quantiles <- qnorm(p=ppoints(1e2), mean=mu, sd=s/sqrt(n))
    empirical.quantiles <- qnorm(p=ppoints(1e2), mean=mean(dt$yhat), 
                                 sd=sd(dt$yhat))
    
    # Create a data frame for Q-Q lot
    qq.data <- data.table(
      Theoretical=theoretical.quantiles,
      Empirical=empirical.quantiles
    )
    if (nrow(dt) > 1) {
      # Create the Q-Q plot using ggplot
      qq.plt <- ggplot(qq.data, aes(x=Theoretical, y=Empirical)) +
        geom_point() 
      
    } else {
      qq.plt <- ggplot(qq.data, aes(x=Theoretical, y=Theoretical)) +
        geom_point(alpha=0)
    }
    qq.plt <- qq.plt + 
        geom_abline(slope=1, intercept=0, color="red") +
        labs(x="Theoretical Quantiles", y="Empirical Quantiles") +
        annotate("text", x=min(qq.data$Theoretical), y=Inf, 
                 label="Normal QQ-Plot\nfor Sample Means", 
                 hjust=-0.1, vjust=1.1, size=5) +
        theme_bw() +
        theme(plot.margin=margin(0, 0, 0, 0, "mm"), 
              plot.title=element_blank())
    output$qq_plot <- renderPlot(qq.plt)
    
    # Assign `dt` to server environment
    assign("dt", value=dt, envir=server.env)
    
    # (Re)Generate Tables
    trigger.input("renderTables")
  }, ignoreInit=T, ignoreNULL=T)
}
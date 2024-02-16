ci.plt.break.lims <- function(dt) {
  if (nrow(dt)==1) c(0, nrow(dt) + 1) else NULL
}

ci.plt.breaks <- function(x) {
  if (length(x) == 1) return(0:2)
  
  .max <- ifelse(length(x) <= 10, 10, floor(log10(length(x))) + 2)
  # Determine breaks: Calculate number based on range size
  x.min <- min(x)
  x.max <- max(x)
  n.breaks <- min(.max, x.max - x.min + 1) 
  # Generate breaks
  breaks <- seq(from=x.min, to=x.max, length.out=n.breaks)
  breaks <- round(breaks) # Ensure breaks are integers
  # Remove duplicated breaks after rounding
  breaks <- unique(breaks)
  return(breaks)
}


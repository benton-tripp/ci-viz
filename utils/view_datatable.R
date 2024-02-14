view.datatable <- function(dt, escape=F, rownames=F, selection="none",
                           filter="none", colnames=names(dt), searching=F,
                           paging=T, scrollX=F, scrollY=F, orderMulti=F,
                           info=T, dom='tip', ordering=F, pageLength=10,
                           lengthChange=F, fontSize="15px", ...) {
  options <- list(
    searching=searching, 
    paging=paging, 
    scrollX=scrollX, 
    scrollY=scrollY, 
    orderMulti=orderMulti, 
    info=info,
    dom=dom, 
    ordering=ordering, 
    pageLength=pageLength, 
    lengthChange=lengthChange,
    ...
  )
  datatable(
    dt, 
    escape=escape, 
    rownames=rownames, 
    selection=selection, 
    filter=filter,
    colnames=colnames,
    options=options
  ) %>%
    DT::formatStyle(1:ncol(dt), "white-space"="nowrap") %>%
    DT::formatStyle(1:ncol(dt), fontSize=fontSize) %>%
    DT::formatStyle(1:ncol(dt), "cursor"="default")
}

# SAMPLE UI/STYLING
# 
# div(
#   class="view-data",
#   style="width:300px;",
#   DTOutput("my_data")
# )
# 



# Load libraries
suppressWarnings(
  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(DT)
  })
)

# Define UI
ui <- fluidPage(
  id="uiPage",
  includeScript("www/scripts.js"),
  includeCSS("www/styles.css"),
  tags$head(
    useShinyjs(),  
    extendShinyjs(text=readr::read_file('www/scripts.js'),
                  functions = c("loading", "finishedLoading")),
    tags$title("C.I. Visualizations"),
    tags$link(rel="shortcut icon", href="favicon.ico"),
    tags$head(
      tags$link(
        rel = "stylesheet", 
        href = "https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.css", 
        integrity = "sha384-MlJdn/WNKDGXveldHDdyRP1R4CTHr3FeuDNfhsLPYrq2t0UBkUdK2jyTnXPEK1NQ",
        crossorigin = "anonymous"
      ),
      tags$script(
        defer = "", 
        src = "https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/katex.min.js", 
        integrity = "sha384-VQ8d8WVFw0yHhCk5E8I86oOhv48xLpnDZx5T9GogA/Y84DcCKWXDmSDfn13bzFZY",
        crossorigin = "anonymous"
      ),
      tags$link(
        rel="stylesheet",
        href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css",
        integrity="sha512-DTOQO9RWCH3ppGqcWaEA1BIZOC6xxalwEsw9c2QQeAIftl+Vegovlnee1c9QX4TctnWMn13TZye+giMm8e2LwA==",
        crossorigin="anonymous",
        referrerpolicy="no-referrer"
      )
    ),
  ),
  h2("Confidence Interval Visualizations"),
  div(
    id="layoutSection",
    class="wrapper shiny-row",
    div(
      id="sidebarSection",
      div(
        id="sidebarOptions",
        h4("Normal Distribution Parameters"),
        tags$script(
          src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML"
        ),
        # Mu
        div(
          class="shiny-row",
          tags$span(
            style="font-size:28px; font-weight: normal; position:relative;
                   font-family:MathJax_Math; font-style:italic; top:-6px;", 
            "μ"),
          tags$span(style="font-size:20px; margin-right:6px; margin-left:2px;", ":"),
          numericInput("mu", label=NULL, value=0)
        ),
        
        # Sigma
        div(
          class="shiny-row",
          tags$span(
            style="font-size:28px; font-weight: normal; position:relative;
                   font-family:MathJax_Math; font-style:italic; top:-6px;", 
            "σ"),
          tags$span(style="font-size:20px; margin-right:6px; margin-left:2px;", ":"),
          numericInput("sigma", label=NULL, value=1)
        ),
        # Confidence Level
        
        # Sample Size

        actionButton(inputId="apply", label="Apply Parameters", width="100%"),
        hr(),
        h4("Generate Data"),
        selectInput(inputId="generateDataCount", label=NULL, 
                    choices=c("1 New Dataset",
                              "10 New Datasets",
                              "100 New Datasets",
                              "1000 New Datasets"),
                    selected="1 New Dataset")
      ),
      tags$button(
        id="generate",
        type="button",
        class="btn btn-default action-button shiny-bound-input",
        style="margin-top:5px; background:#fff;",
        div(
          style="shiny-row",
          tags$i(
            style="margin-right:0; font-size:18px; color:#003000;",
            class="fa fa-plus",
            `aria-hidden`="true"
          ),
          tags$span(
            style="position: relative; top: -1px; color:#004000;",
            "Generate Dataset(s)"
          )
        )
      ),
      tags$button(
        id="resetData",
        type="button",
        class="btn btn-default action-button shiny-bound-input",
        style="margin-top:10px; background:#777;",
        div(
          style="shiny-row",
          tags$i(
            style="margin-right:5px; font-size:20px; color:#fff",
            class="fa fa-refresh",
            `aria-hidden`="true"
          ),
          tags$span(
            style="position: relative; top: -2px; color:#fff;",
            "Reset to 1"
          )
        )
      )
    ),
    div(
      id="mainSection"
    )
  )
)
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
        div(
          shinyWidgets::numericInputIcon(
            "confLevel", 
            "Confidence Interval %:",
            value = 95,
            min=80,
            max=99.99,
            step=0.01,
            icon=list(NULL, icon("percent", verify_fa=F)),
            help_text="Range limited to 80-99.99%")
        ),
        # Sample Size
        numericInput("sampleSize", "Sample Size:", value=10, min=2, max=1e2),
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
      hr(),
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
      id="mainSection",
      div(
        class="shiny-row",
        div(
          style="margin-right:1.5vh",
          div(
            style="height:40px;",
            h4("Sample Summary", style="margin:0; padding:10px;")
          ),
          div(
            class="bordered-cell",
            div(
              id="sampleSummarySection",
              div(
                style="padding:8px; width: 100%;",
                div(
                  class="shiny-row",
                  style="padding:8px; border:1px solid #333;
                         background-color:#efefef;",
                  sliderInput(inputId="sampleDataset", 
                              label="Sample Dataset #", value=1, 
                              min=1, max=1, step=1, 
                              animate=animationOptions(interval=2e3, loop=T)),
                  span(style="width:50px"),
                  radioButtons("sampleDisplay", label="Display Type",
                               choices=c("Plot", "Table"), selected="Plot")
                )
              ),
              conditionalPanel(
                "input.sampleDisplay==='Plot'",
                div(
                  style="padding-right:20px;",
                  plotOutput("sample_hist", height="350px", width="99%")
                )
              ),
              conditionalPanel(
                "input.sampleDisplay==='Table'",
                style="padding: 1vh; padding-left:3vh;",
                div(
                  id="sampleTable",
                  DTOutput("s_table")
                )
              )
            )
          )
        ),
        shiny::tabsetPanel(
          id="tabs", 
          tabPanel(
            title="Distribution of Sample Means",
            div(
              class="plot-tab",
              div(
                class="plot-area-2 shiny-row",
                div(
                  class="plot-area-2-sub-1",
                  plotOutput("sm_dist", width="100%", height="100%")
                ),
                div(
                  class="plot-area-2-sub-2 shiny-col",
                  div(
                    class="bordered-cell",
                    id="attributesTableArea",
                    # Attributes/Statistics Table
                    h4("Attributes/Statistics Summary",
                       style="margin:0; padding:1px;"),
                    div(
                      id="attributesTable",
                      DTOutput("att_table")
                    )
                  ),
                  div(
                    # QQ-Plot
                    style="margin-top:10px;",
                    plotOutput("qq_plot", height="360px", width="390px")
                  )
                )
              )
            )
          ),
          tabPanel(
            # Confidence Interval Coverage Plots
            title="C.I. Coverage Plots",
            div(
              class="plot-tab",
              div(
                class="plot-area-1",
                div(
                  class="plot-area-1-sub",
                  plotOutput("ci_plot", width="100%", height="100%")
                ),
                div(
                  style="border-bottom: 1px solid #333333;"
                ),
                div(
                  class="plot-area-1-sub",
                  plotOutput("sorted_plot", width="100%", height="100%")
                )
              )
            )
          ),
          tabPanel(
            title="Cumulative & Running C.I. Coverage Plots",
            div(
              class="plot-tab",
              div(
                class="plot-area-1",
                div(
                  class="plot-area-1-sub",
                  plotOutput("cdist_plot", width="100%", height="100%")
                ),
                div(
                  style="border-bottom: 1px solid #333333;"
                ),
                div(
                  class="plot-area-1-sub",
                  plotOutput("rci_plot", width="100%", height="100%")
                )
              )
            )
          )
        )
      )
    )
  )
)
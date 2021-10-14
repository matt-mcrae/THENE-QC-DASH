library(shiny)
library(DBI)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)
library(plotly)

source("LIMS.R")
source("SPC_PLOT.R")
source("FLOSS.R")


# Define UI ----
ui <-
  navbarPage(
    title = div(strong("Alkathene Quality Dashboard"),
                style = "color:darkblue; font-size:40px"),
    windowTitle = "Alkathene QC Dashboard",
    collapsible = T,
    header = wellPanel(
      fluidRow(
        column(4, div(style = "font-size:14px; height:80px; color:dimgrey",
                      sliderInput("days",
                                  label = "Select Number of Days to View:",
                                  min = 7, max = 28, value = 7),
        )), 
        column(5, div(style = "font-size:14px; height:80px",
                      p(strong(span("GREEN", style = "color:springgreen"),
                               "= Process under control (no action required)")), 
                      p(strong(span("YELLOW", style = "color:goldenrod"), 
                               "= Warning - process outside control limits (take action to correct)")),
                      p(strong(span("RED", style = "color:coral"), 
                               "= Process out of control (potentially off-spec product)"))
        )
        ),
        column(3, div(style = "font-size:14px; margin-top: 32px; color:dimgrey",
                      checkboxInput("spc",
                                    label = strong("Enable SPC Overlay?"),
                                    value = F)),
               div(span(strong("Latest result:"), style = "color:dimgrey"), 
                   em(textOutput('last', inline = T)))
        )
      ),
      tags$style( #remove whitespaces etc with css tags
        type = "text/css",
        ".navbar {margin-bottom: 0px}",
        ".well {padding-bottom: 9px; margin-bottom: 0px;padding-top: 9px; margin-top: 0px;}",
        "input[type=checkbox] {transform: scale(1.5); margin-left: -24px;}",
        ".checkbox {margin-top: 0px; margin-bottom: 0px;}"
      )
    ),
    tabPanel(
      title = strong("Overview", style = "font-size:24px; color:dimgrey"),
      style = "margin-top: 10px",
      fluidRow(
        column(3, fluidRow(
          div(strong("Summary (Past 24hrs)"),
              style = "color:darkblue; font-size:30px",
              align = "center")
        ),
        fluidRow(div(
          strong(tableOutput("traffic"),
                 style = "font-size:20px")
        ))),
        column(8, fluidRow(
          div(strong("Floss Contamination"),
              style = "color:darkblue; font-size:30px",
              align = "center")
        ),
        fluidRow(plotOutput("floss", height = "450px")),
        offset = 1)
      )
    ),
    tabPanel(
      title = strong("RV2", style = "color:springgreen; font-size:24px"),
      fluidRow(column(6, plotlyOutput('RV2D')), 
               column(6, plotlyOutput('RV2S'))),
      uiOutput("RV2")
    ),
    tabPanel(
      title = strong("RV3", style = "color:lightskyblue; font-size:24px"),
      fluidRow(column(6, plotlyOutput('RV3D')), 
               column(6, plotlyOutput('RV3S'))),
      uiOutput("RV3")
    ),
    tabPanel(
      title = strong("RV4", style = "color:hotpink; font-size:24px"),
      fluidRow(column(6, plotlyOutput('RV4D')), 
               column(6, plotlyOutput('RV4S'))),
      uiOutput("RV4")
    ),
    tabPanel(
      title = strong("Help", style = "font-size:24px; color:dimgrey"),
      style = "margin-top: 10px",
      fluidRow(column(6, 
                      div(strong("How to Use the Dashboard"),
                          style = "color:darkblue; font-size:30px",
                          align = "center"),
                      div(style = "font-size: 20px",
                      p("The 'Overview' tab gives a summary of overall product quality.
          For more detail, select the tab for one of the Reactor Vessels (RV) and use the
          slider to adjust how much data to view (up to 4 weeks)."
                      ),
                      p("The Quality Control Limits (QC) are indicated by", span("green", style = "color:springgreen"), 
                      "zones and Specification Limits (Quality Assurance, QA) are indicated by", 
                      span("yellow", style = "color:goldenrod"), "zones: any results falling outside
          both the yellow and green zones are", span("'off-spec.'", style = "color:coral")),
                      p("The Summary table uses a 'traffic light' system to indicate the plant status at a glance.
                        The lights switch to yellow or red if any results have been outside the QC or QA limits, respectively."),
                      p("In the Floss Contamination charts, the area is proportional to how much floss is present
                        in bulk container samples unloaded each day: for example, if the chart is entirely green
                        on a given day, then there is a low amount of floss overall."),
                      p("For each grade on the Control Charts, the specification target is indicated by a solid",
          span("blue", style = "color:dodgerblue"), "line, and different grades are faintly outlined."),
                      p("Hover over data points in the control charts to see individual result values. 
                      Click and drag over an area to zoom in. To return to the default zoom level, double-click 
                      inside the control chart."),
                      p("An additional 'SPC' layer can be enabled to view more detailed information
                        about the spread (standard deviation) and centre (average) of the results.")
                      )),
               column(6,                       
                      div(strong("What is SPC?"),
                          style = "color:darkblue; font-size:30px",
                          align = "center"),
                      div(style = "font-size: 20px",
                      p("Statistical Process Control (SPC) can be used to monitor process data and help detect when
                        there has been a change in the process, which could cause out-of-specification product."),
                      p("When the SPC layer is enabled, some additional information is shown on the control charts:"),
                      p("A solid", span("green", style = "color:forestgreen"), "line indicates the process mean (average 
                        of the results for each grade)."),
                      p("Dashed", span("red", style = "color:red"), "lines show the variation in the results (standard 
                      deviation or sigma, σ). These lines are drawn at ±1σ, 2σ and 3σ."),
                      p("Using these statistics, four rules can be used as a guide to detect when the process is out of control:"),
                      p(span(strong("RULE 1:", style = "color:red")), "Any single data point falls outside the 3σ-limit from the 
                        process mean"),
                      p(span(strong("RULE 2:", style = "color:red")), "Two out of three consecutive points fall beyond the 
                        2σ-limit (on the same side of the process mean)"),
                      p(span(strong("RULE 3:", style = "color:red")), "Four out of five consecutive points fall beyond the 
                        1σ-limit (on the same side of the process mean)"),
                      p(span(strong("RULE 4:", style = "color:red")), "Nine consecutive points fall on the same side of the 
                        process mean"),
                      p("If any of the above rules are violated, data points are highlighted red with a number indicating 
                        which rule was violated.")
                      )))
    )
  )

# Define server logic ----
server <- function(input, output) {
  
  DATA <-  reactiveVal(LIMS())
  
  options(warn = -1)

  
  output$RV2D <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Density", input$days, input$spc)})
  output$RV2S <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Swell Ratio", input$days, input$spc)})
  output$RV2A <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Ash", input$days, input$spc)})
  output$RV2G <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Granules per Gram", input$days, input$spc)})
  output$RV2C <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Good Granules", input$days, input$spc)})
  
  output$RV2 <- renderUI({
    if(is.null(SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Ash", input$days, input$spc))){fluidRow(
        column(6, plotlyOutput("RV2C")),
        column(6, plotlyOutput("RV2G"))
      )
    } else {fluidRow(
        column(4, plotlyOutput("RV2A")),
        column(4, plotlyOutput("RV2C")),
        column(4, plotlyOutput("RV2G"))
      )
    }
  })
  
  output$RV3D <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Density", input$days, input$spc)})
  output$RV3S <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Swell Ratio", input$days, input$spc)})
  output$RV3A <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Ash", input$days, input$spc)})
  output$RV3G <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Granules per Gram", input$days, input$spc)})
  output$RV3C <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Good Granules", input$days, input$spc)})
  
  output$RV3 <- renderUI({
    if(is.null(SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Ash", input$days, input$spc))){fluidRow(
      column(6, plotlyOutput("RV3C")),
      column(6, plotlyOutput("RV3G"))
    )
    } else {fluidRow(
      column(4, plotlyOutput("RV3A")),
      column(4, plotlyOutput("RV3C")),
      column(4, plotlyOutput("RV3G"))
    )
    }
  })
  
  output$RV4D <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Density", input$days, input$spc)})
  output$RV4S <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Swell Ratio", input$days, input$spc)})
  output$RV4A <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Ash", input$days, input$spc)})
  output$RV4G <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Granules per Gram", input$days, input$spc)})
  output$RV4C <- renderPlotly({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Good Granules", input$days, input$spc)})
  
  output$RV4 <- renderUI({
    if(is.null(SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Ash", input$days, input$spc))){fluidRow(
      column(6, plotlyOutput("RV4C")),
      column(6, plotlyOutput("RV4G"))
    )
    } else {fluidRow(
      column(4, plotlyOutput("RV4A")),
      column(4, plotlyOutput("RV4C")),
      column(4, plotlyOutput("RV4G"))
    )
    }
  })
  
  
  output$traffic <- renderTable({DATA()$TRAF},
                                na = "",
                                rownames = T,
                                align = "lccc",
                                spacing = "m",
                                wideth = "100%",
                                sanitize.text.function = function(x) x)


  output$floss <- renderPlot(FLOSS(DATA()$FLOS, input$days))
  
  
  output$last <- renderText(strftime(last(DATA()$RSLT$SMPL_DT_TM), 
                                     "%a %b %e, %R", 
                                     tz = "GMT"))
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
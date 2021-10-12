library(shiny)
library(DBI)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)

source("LIMS.R")
source("SPC_PLOT.R")
source("FLOSS.R")

# Define UI ----
ui <-
  navbarPage(
    title = div(strong("Alkathene Quality Dashboard"),
                style = "font-size:32px"),
    windowTitle = "Alkathene QC Dashboard",
    collapsible = T,
    header = wellPanel(
      fluidRow(
        column(3, sliderInput("days",
                              label = div("Number of Days to View:",
                                          style = "font-size:26px"),
                              min = 7, max = 28, value = 7
        ),
        offset = 1
        ),
        column(
          4,
          p(
            "The 'Overview' tab gives a summary of overall product quality.
          For more detail, select the tab for one of the reactors and use the
          slider to adjust how much data to view (up to 4 weeks).",
            style = "font-size:12px"
          ),
          p(
            "The control limits (QC) are indicated by green zones and specification
          (QA) limits are indicated by yellow zones: any results falling outside
          the yellow and green zones are 'off-spec'.",
            style = "font-size:12px"
          ),
          p(
            "For each grade, the average of the results is indicated by a solid
          blue line, with red dashed lines placed at ± 1, 2 and 3 standard
          deviations (σ) from the average.",
            style = "font-size:12px"
          )
        ),
        column(
          4,
          p(
            "Four rules can be used as a guide to detect when the process is out
          of control:",
            style = "font-size:12px"
          ),
          p(
            span(strong("RULE 1:", style = "color:red")),
            "Any single data point falls outside the 3σ-limit from the process
          mean",
            style = "font-size:12px"
          ),
          p(
            span(strong("RULE 2:", style = "color:red")),
            "Two out of three consecutive points fall beyond the 2σ-limit (on the
          same side of the process mean)",
            style = "font-size:12px"
          ),
          p(
            span(strong("RULE 3:", style = "color:red")),
            "Four out of five consecutive points fall beyond the 1σ-limit (on the
          same side of the process mean)",
            style = "font-size:12px"
          ),
          p(
            span(strong("RULE 4:", style = "color:red")),
            "Nine consecutive points fall on the same side of the process mean",
            style = "font-size:12px"
          )
        )
      ),
      tags$style( #remove whitespaces
        type = "text/css",
        ".navbar {margin-bottom: 0px}",
        ".well {padding-bottom: 0px; margin-bottom: 0px}",
      )
    ),
    tabPanel(
      title = strong("Overview", style = "font-size:24px"),
      fluidRow(
        column(3, fluidRow(
          div("Summary (Past 24hrs)",
              style = "color:#0099FF; font-size:30px",
              align = "center")
        ),
        fluidRow(div(
          strong(tableOutput("traffic"),
                 style = "font-size:20px")
        ))),
        column(8, fluidRow(
          div("Floss Severity per Batch",
              style = "color:#0099FF; font-size:30px",
              align = "center")
        ),
        fluidRow(plotOutput("floss", height = "450px")),
        offset = 1)
      )
    ),
    tabPanel(
      title = strong("RV2", style = "color:springgreen; font-size:24px"),
      fluidRow(column(6, plotOutput('RV2D', height = "375px")), 
               column(6, plotOutput('RV2S', height = "375px"))),
      uiOutput("RV2")
    ),
    tabPanel(
      title = strong("RV3", style = "color:lightskyblue; font-size:24px"),
      fluidRow(column(6, plotOutput('RV3D', height = "375px")), 
               column(6, plotOutput('RV3S', height = "375px"))),
      uiOutput("RV3")
    ),
    tabPanel(
      title = strong("RV4", style = "color:hotpink; font-size:24px"),
      fluidRow(column(6, plotOutput('RV4D', height = "375px")), 
               column(6, plotOutput('RV4S', height = "375px"))),
      uiOutput("RV4")
    )
  )

# Define server logic ----
server <- function(input, output) {
  
  DATA <-  reactiveVal(LIMS())

  
  output$RV2D <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Density", input$days)})
  output$RV2S <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Swell Ratio", input$days)})
  output$RV2A <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Ash", input$days)})
  output$RV2G <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Granules per Gram", input$days)})
  output$RV2C <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Good Granules", input$days)})
  
  output$RV2 <- renderUI({
    if(is.null(SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV2", "Ash", input$days))){fluidRow(
        column(6, plotOutput("RV2C", height = "375px")),
        column(6, plotOutput("RV2G", height = "375px"))
      )
    } else {fluidRow(
        column(4, plotOutput("RV2A", height = "375px")),
        column(4, plotOutput("RV2C", height = "375px")),
        column(4, plotOutput("RV2G", height = "375px"))
      )
    }
  })
  
  output$RV3D <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Density", input$days)})
  output$RV3S <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Swell Ratio", input$days)})
  output$RV3A <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Ash", input$days)})
  output$RV3G <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Granules per Gram", input$days)})
  output$RV3C <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Good Granules", input$days)})
  
  output$RV3 <- renderUI({
    if(is.null(SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV3", "Ash", input$days))){fluidRow(
      column(6, plotOutput("RV3C", height = "375px")),
      column(6, plotOutput("RV3G", height = "375px"))
    )
    } else {fluidRow(
      column(4, plotOutput("RV3A", height = "375px")),
      column(4, plotOutput("RV3C", height = "375px")),
      column(4, plotOutput("RV3G", height = "375px"))
    )
    }
  })
  
  output$RV4D <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Density", input$days)})
  output$RV4S <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Swell Ratio", input$days)})
  output$RV4A <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Ash", input$days)})
  output$RV4G <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Granules per Gram", input$days)})
  output$RV4C <- renderPlot({SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Good Granules", input$days)})
  
  output$RV4 <- renderUI({
    if(is.null(SPC_PLOT(DATA()$RSLT, DATA()$SPEC, "RV4", "Ash", input$days))){fluidRow(
      column(6, plotOutput("RV4C", height = "375px")),
      column(6, plotOutput("RV4G", height = "375px"))
    )
    } else {fluidRow(
      column(4, plotOutput("RV4A", height = "375px")),
      column(4, plotOutput("RV4C", height = "375px")),
      column(4, plotOutput("RV4G", height = "375px"))
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
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
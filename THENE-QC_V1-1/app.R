library(shiny)
library(DBI)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)

source('SPC.R')

# Define UI ----
ui <-
  navbarPage(
    title = div(strong("Alkathene Quality Dashboard"), 
                style = "color:black; font-size:32px"),
    windowTitle = "Alkathene QC Dashboard",
    collapsible = T,
    header = fluidRow(
      column(
        3,
        sliderInput(
          "days",
          label = "Number of Days to View:",
          min = 7,
          max = 28,
          value = 7
        ),
        offset = 1
      ),
      column(
        4,
        p(
          "The Control Charts below are used to detect variations in the 
          Alkathene process. For each grade, the process mean (average) is 
          indicated by a green line, with red lines placed at ± 1, 2 and 3 
          standard deviations (??) from the process mean. The position of data 
          points within these regions can be used to predict potentially out of 
          control processes based on a set of four rules:",
          style = "font-size:12px"
        ),
        em(
          "NOTE: negative values indicate a rule violation has occured below 
          the process mean.",
          style = "font-size:12px"
        )
      ),
      column(
        4,
        p(
          span(strong("RULE 1:", style = "color:red")),
          "Any single data point falls outside the 3??-limit from the process 
          mean",
          style = "font-size:12px"
        ),
        p(
          span(strong("RULE 2:", style = "color:red")),
          "Two out of three consecutive points fall beyond the 2??-limit (on the 
          same side of the process mean)",
          style = "font-size:12px"
        ),
        p(
          span(strong("RULE 3:", style = "color:red")),
          "Four out of five consecutive points fall beyond the 1??-limit (on the 
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
    tabPanel(
      title = strong("RV2", style = "color:green; font-size:24px"),
      fluidRow(column(6, plotOutput('RV2D')), column(6, plotOutput('RV2S'))),
      fluidRow(column(6, plotOutput('RV2C')), column(6, plotOutput('RV2G')))
    ),
    tabPanel(
      title = strong("RV3", style = "color:blue; font-size:24px"),
      fluidRow(column(6, plotOutput('RV3D')), column(6, plotOutput('RV3S'))),
      fluidRow(
        column(4, plotOutput('RV3A')),
        column(4, plotOutput('RV3C')),
        column(4, plotOutput('RV3G'))
      )
    ),
    tabPanel(
      title = strong("RV4", style = "color:hotpink; font-size:24px"),
      fluidRow(column(6, plotOutput('RV4D')), column(6, plotOutput('RV4S'))),
      fluidRow(
        column(4, plotOutput('RV4A')),
        column(4, plotOutput('RV4C')),
        column(4, plotOutput('RV4G'))
      )
    ),
    tabPanel("Specifications",
             fluidRow(column(
               12, img(src = "specs.png")
             )))
  )

# Define server logic ----
server <- function(input, output) {
  spc <- reactive({
    SPC(input$days)
  })
  
  output$RV2D <- renderPlot({
    spc()$RV2D$PLOT
  })
  output$RV2S <- renderPlot({
    spc()$RV2S$PLOT
  })
  output$RV2C <- renderPlot({
    spc()$RV2C$PLOT
  })
  output$RV2G <- renderPlot({
    spc()$RV2G$PLOT
  })
  
  output$RV3D <- renderPlot({
    spc()$RV3D$PLOT
  })
  output$RV3S <- renderPlot({
    spc()$RV3S$PLOT
  })
  output$RV3A <- renderPlot({
    spc()$RV3A$PLOT
  })
  output$RV3C <- renderPlot({
    spc()$RV3C$PLOT
  })
  output$RV3G <- renderPlot({
    spc()$RV3G$PLOT
  })
  
  output$RV4D <- renderPlot({
    spc()$RV4D$PLOT
  })
  output$RV4S <- renderPlot({
    spc()$RV4S$PLOT
  })
  output$RV4A <- renderPlot({
    spc()$RV4A$PLOT
  })
  output$RV4C <- renderPlot({
    spc()$RV4C$PLOT
  })
  output$RV4G <- renderPlot({
    spc()$RV4G$PLOT
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
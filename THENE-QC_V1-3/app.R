library(shiny)
library(DBI)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)

source("LIMS.R")
source("SPC.R")

# Define UI ----
ui <-
  navbarPage(
    title = div(strong("Alkathene Quality Dashboard"), 
                style = "font-size:32px"),
    windowTitle = "Alkathene QC Dashboard",
    collapsible = T,
    header = wellPanel(fluidRow(
      column(
        3,
        sliderInput(
          "days",
          label = div("Number of Days to View:",
                      style = "font-size:19px"),
          min = 7,
          max = 28,
          value = 7
        ),
        offset = 1
      ),
      column(
        4,
        p(
          "The 'Overview' tab gives a summary of overall product quality.
          For more detail, select the tab for one of the reactors and use the 
          slider to adjust how much data to view (up to 4 weeks).",
          style = "font-size:12px"),
        p(
          "The control limits (QC) are indicated by green zones and specification 
          (QA) limits are indicated by yellow zones: any results falling outside
          the yellow and green zones are 'off-spec'.",
          style = "font-size:12px"),
        p(
          "For each grade, the average of the results is indicated by a solid 
          blue line, with red dashed lines placed at ± 1, 2 and 3 standard 
          deviations (σ) from the average.",
          style = "font-size:12px")
      ),
      column(
        4,
        p("Four rules can be used as a guide to detect when the process is out 
          of control:",
          style = "font-size:12px"),
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
      ), tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}",
                    ".well {padding-bottom: 0px}") #remove whitespaces
      ),
    tabPanel(
      title = strong("Overview", style = "font-size:24px"),
      fluidRow(column(3, fluidRow(div("Summary (Past 24hrs)",
                                     style = "color:#0099FF; font-size:30px", 
                                     align = "center")),
                      fluidRow(div(strong(tableOutput("traffic"),
                                 style = "font-size:20px")))
                      ),
               column(8, fluidRow(div("Floss Severity per Batch",
                                     style = "color:#0099FF; font-size:30px", 
                                     align = "center")),
                      fluidRow(plotOutput('floss', height = "450px")),
                      offset = 1
                      )
               )
    ),
    tabPanel(
      title = strong("RV2", style = "color:springgreen; font-size:24px"),
      fluidRow(column(6, plotOutput('RV2D')), column(6, plotOutput('RV2S'))),
      fluidRow(
        column(4, plotOutput('RV2A')),
        column(4, plotOutput('RV2C')),
        column(4, plotOutput('RV2G'))
      )
    ),
    tabPanel(
      title = strong("RV3", style = "color:lightskyblue; font-size:24px"),
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
  )
  )

# Define server logic ----
server <- function(input, output) {
  
  DATA <- LIMS()
  
  spc <- reactive({SPC(input$days)})
  
  output$RV2D <- renderPlot({spc()$RV2D$PLOT})
  output$RV2S <- renderPlot({spc()$RV2S$PLOT})
  output$RV2A <- renderPlot({spc()$RV2A$PLOT})
  output$RV2C <- renderPlot({spc()$RV2C$PLOT})
  output$RV2G <- renderPlot({spc()$RV2G$PLOT})
  
  output$RV3D <- renderPlot({spc()$RV3D$PLOT})
  output$RV3S <- renderPlot({spc()$RV3S$PLOT})
  output$RV3A <- renderPlot({spc()$RV3A$PLOT})
  output$RV3C <- renderPlot({spc()$RV3C$PLOT})
  output$RV3G <- renderPlot({spc()$RV3G$PLOT})
  
  output$RV4D <- renderPlot({spc()$RV4D$PLOT})
  output$RV4S <- renderPlot({spc()$RV4S$PLOT})
  output$RV4A <- renderPlot({spc()$RV4A$PLOT})
  output$RV4C <- renderPlot({spc()$RV4C$PLOT})
  output$RV4G <- renderPlot({spc()$RV4G$PLOT})
  
  output$traffic <- renderTable({DATA$TRAF}, 
                                na = "",
                                rownames = T,
                                align = "lccc", 
                                spacing = "m",
                                wideth = "100%",
                                sanitize.text.function = function(x) x)
  
  
  FLOSS <- function(ndays){
    
    st <- today() - ndays
    
    if(ndays<=7){
      datea <- 45
      datel <- "%a"
        datev <- 0.5} else if(ndays<=21){
          datea <- 45
          datel <- "%d-%b"
          datev <- 1} else{
            datea <- 90
            datel <- "%d-%b"
            datev <- 0.5}
    
    floss.plot <- DATA$FLOS %>% filter(DATE >= st) %>% 
      ggplot(aes(DATE,value,fill=category)) + 
      theme_bw() +
      geom_area() +
      geom_line(position = "stack", colour = "dimgrey") +      
      scale_x_datetime(date_breaks = "1 day", date_labels = datel) + 
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +  
      scale_fill_manual(values = c("red","orange","yellow","green")) +      
      theme(legend.position = "top",
            legend.text = element_text(face = "bold", size = 12),
            axis.text.x = element_text(angle = datea, vjust = datev, hjust=1, 
                                       face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 12),
            strip.text.x = element_text(size = 14, face = "bold")) + 
      labs(x=NULL, y=NULL, fill=NULL) + 
      facet_wrap(~EQ_NAME)
    
    return(suppressMessages(print(floss.plot)))
    
  }
  
  output$floss <- renderPlot(FLOSS(input$days))
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
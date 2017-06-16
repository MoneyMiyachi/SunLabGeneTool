library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(xtable)
library(markdown)
library(corrplot)
library(rdrop2)
library(shinyBS)

data <- read.csv("Almost_Complete_db.csv", stringsAsFactors = F)
data$X <- NULL

fluidPage(

  titlePanel("Sun Lab Gene Correlation Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload Files',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      textAreaInput("caption", "Input", "Data"),
      actionButton("Load", label = "Analyze"),
      actionButton("add", label = "Add to Database"),
      br(),
      tags$hr(),
      bsCollapse(id = "collapseExample",
                 bsCollapsePanel("Input Options",
                                 checkboxInput('header', 'Header', TRUE),
                                 radioButtons('sep', 'Separator',
                                              c(Comma=',',
                                                Semicolon=';',
                                                Tab='\t'),
                                              ','),
                                 radioButtons('quote', 'Quote',
                                              c(None='',
                                                'Double Quote'='"',
                                                'Single Quote'="'"),
                                              '"'), style = "info")
      )
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Add Gene/Experiment",
                 h4("Users Uploaded Table"),
                 #tableOutput("contents")
                 DT::dataTableOutput("contents"),
                 verbatimTextOutput("value")
        ),
        tabPanel("Pearson Correlation Rankings",
                 h4("Highest Correlated Genes"),
                 DT::dataTableOutput("top_10")
        ),
        tabPanel("Correlation Plots",
                 h4("Pearsons Correlation Coefficient Plot"),
                 plotOutput('trial')),
        tabPanel("Working Database",
                 h4("Current Working Database"),
                 fluidRow(dataTableOutput('idk'))
        )
      )
    )
  ),
  bsCollapse(id = "main_collapse",
             bsCollapsePanel("Advanced Correlation Options",   fluidRow(
               column(3,
                      h4("Correlation Explorer"),
                      sliderInput('corrthresh', 'Correlation Threshold', 
                                  min=1, max=nrow(data), value=min(1000, nrow(data)), 
                                  step=500, round=0),
                      br(),
                      checkboxInput('downfold', 'Downfold'),
                      checkboxInput('upfold', 'Upfold')
               ),
               column(4, h4("Single Gene Comparison"), offset = 1,
                      selectInput('gene1', 'Gene 1', names(data)),
                      selectInput('gene2', 'Gene 2', names(data), names(data)[[2]]),
                      selectInput('thresh', 'Treshhold', c('None', names(data)))
               ),
               column(4,h4("Supplemental Information"), 
                      includeMarkdown("supplemental.md")
               )
             ), style = "info")
  )
)
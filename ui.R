library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(xtable)
library(markdown)
library(corrplot)

data <- read.csv("Almost_Complete_db.csv", stringsAsFactors = F)
data$X <- NULL

fluidPage(theme = "bootstrap.css",
  titlePanel("Sun Lab Gene Correlation Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload Files',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
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
                   '"')
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Add Gene/Experiment",
                 h4("Users Uploaded Table"),
                 #tableOutput("contents")
                 DT::dataTableOutput("contents")
        ),
        tabPanel("Working Database",
                 h4("Current Working Database"),
                fluidRow(dataTableOutput('foo'))
        ),
        tabPanel("Pearson Correlation Coefficient",
                 h4("Pearsons Correlation Coefficient Plot"),
        plotOutput('x'))
      )
    )
  ),
  fluidRow(
    column(3,
           h4("Correlation Explorer"),
           sliderInput('corrthresh', 'Correlation Threshold', 
                       min=1, max=nrow(data), value=min(1000, nrow(data)), 
                       step=500, round=0),
           br(),
           checkboxInput('downfold', 'Downfold'),
           checkboxInput('upfold', 'Upfold')
    ),
    column(4, h4("Choose Gene"), offset = 1,
           selectInput('gene1', 'Gene 1', names(data)),
           selectInput('gene2', 'Gene 2', names(data), names(data)[[2]]),
           selectInput('color', 'Color', c('None', names(data)))
    ),
    column(4,h4("Supplemental Information"), 
           includeMarkdown("supplemental.md")
    )
  )
)
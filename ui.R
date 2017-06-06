library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(xtable)


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
                 tableOutput("contents")
        ),
        tabPanel("Working Database",
                 h4("Current Working Database"),
                fluidRow(dataTableOutput('foo'))
        ),
        tabPanel("Pearson Correlation Coefficient",
                 h4("Pearsons Correlation Coefficient Plot")),
        plotOutput('x')
      )
    )
  ),
  fluidRow(
    column(3,
           h4("Diamonds Explorer"),
           sliderInput('sampleSize', 'Sample Size', 
                       min=1, max=nrow(data), value=min(1000, nrow(data)), 
                       step=500, round=0),
           br(),
           checkboxInput('jitter', 'Jitter'),
           checkboxInput('smooth', 'Smooth')
    ),
    column(4, offset = 1,
           selectInput('x', 'X', names(data)),
           selectInput('y', 'Y', names(data), names(data)[[2]]),
           selectInput('color', 'Color', c('None', names(data)))
    ),
    column(4,
           selectInput('facet_row', 'Facet Row', c(None='.', names(data))),
           selectInput('facet_col', 'Facet Column', c(None='.', names(data)))
    )
  )
)
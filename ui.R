library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(xtable)
library(markdown)
library(corrplot)
library(rdrop2)
library(shinyBS)
library(rmarkdown)



data <- read.csv("Almost_Complete_db.csv", stringsAsFactors = F)
data$X <- NULL

fluidPage(
  tags$head(tags$script(src="script.js")),
  ## link the CSS file
  tags$head(tags$link(rel="stylesheet", 
                      type="text/css",
                      href="style.css")),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  titlePanel("Sun Lab Gene Correlation Tool"),
  bsCollapse(id = "collapseExample",
             bsCollapsePanel("Directions",
                             includeMarkdown("./markdown/Direction.md"), style = "info")
  ),
  #Sidebar stuff
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload Files',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      actionButton("fLoad", label = "Load Input File", class="btn-info"),
      actionButton("fCorr", label = "Correlation Analysis", class="btn-primary"),
      actionButton("fAdd", label = "Add Gene", class="btn-danger"),
      tags$hr(),
      br(),
      textAreaInput("caption", "Text Input", "", placeholder = 
"Gene, BRCA1
CALML5, 0.234
CASP14, 0.568
KRT9, 0.238
CALML5, 0.816
ARG1, 0.439
KRT1, 0.711"),
      br(),
      actionButton("Load", label = "Load Text Input", class="btn-info"),
      actionButton("Corr", label = "Correlation Analysis", class="btn-primary"),
      actionButton("Add", label = "Add Gene", class="btn-danger"),
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
      ),
    tags$hr(),
    actionButton("Example", label = "Example Text Input", class="btn-success")
    ),
    
    
    #Main Panel Tab Panel Joint Action that display all the Correlation Analysis
    mainPanel(
      tabsetPanel(
        tabPanel("Add Gene/Experiment",
                 h4("Users Uploaded Table"),
                 #tableOutput("contents")
                 DT::dataTableOutput("contents"),
                 DT::dataTableOutput("other")
        ),
        tabPanel("Pearson Correlation Rankings",
                 h4("Highest Pearson Correlated Genes"),
                 DT::dataTableOutput("top_10"),
                 DT::dataTableOutput("txttop_10")
        ),
        tabPanel("Spearman Correlation Rankings",
                 h4("Highest Spearman Correlated Genes"),
                 DT::dataTableOutput("spear_10")
        ),
        tabPanel("Correlation Plots",
                 h4("Pearsons Correlation Coefficient Plot"),
                 conditionalPanel(condition="input.file1 != NULL",
                 plotOutput('trial')),
                 conditionalPanel(condition="input.corr != 0",
                 plotOutput('txtpearson'))
        ),
        tabPanel("Working Database",
                 h4("Current Working Database"),
                 fluidRow(dataTableOutput('shit'))
        )
      )
    )
  ),
  #Extra Stuff that is completley arbritrary and irrelevant right now 
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
                      actionButton("Compare", label = "Compare Genes", class="btn-success")
               ),
               column(4,h4("Supplemental Information"), 
                      includeMarkdown("markdown/supplemental.md")
               )
             ), style = "info")
  )
)
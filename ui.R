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
corner_element = HTML(paste0('<a href=',shQuote(paste0("https://www.kenmiyachi.com/")), '>', 'Foo', '</a>'))

new <- mtcars



fluidPage(
  tags$head(tags$script(src="script.js")),
  ## link the CSS file
  tags$head(tags$link(rel="stylesheet", 
                      type="text/css",
                      href="style.css")),
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Open+Sans:400,700|Space+Mono:400,700i');
                    
                    h2 {
                    font-family: 'Space Mono', bold;
                    font-weight: 800;
                    line-height: 1.9;
                    color: black;
                    margin: auto;
                    text-align: center;
                    width: 50%;
                    font-size: 29px;
                    }

                    #suninfo {
                    font-family: 'Space Mono', bold;
                    font-weight: 800;
                    line-height: 1.9;
                    color: black;
              
                    }

                    ol {
                    type: 1;
                    }
                    
                    "))
    ),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  imageOutput("preImage", height = 60),
  titlePanel("Human Genome Keratinocyte Perturbation Database (HGKP-Db)"),
  tags$head(tags$style(
    type="text/css",
    "#image preImage {max-width: 100%; width: 100%; height: auto}"
  )),
  fluidRow(id = "suninfo", column(width = 4, helpText(   a("Journal: HGKP-Db Tool", href="https://www.bryansunlab.com/", target="_blank">"https://www.bryansunlab.com/"))),
           column(width = 4, offset = 4, helpText(   a("Sun Laboratory: University of California, San Diego", href="https://www.bryansunlab.com/", target="_blank">"https://www.bryansunlab.com/")))),
  bsCollapse(id = "collapseExample",
             bsCollapsePanel("Directions",
                             includeMarkdown("./markdown/Direction.md"), style = "success")
  ),
  #Sidebar stuff
  sidebarLayout(
    sidebarPanel(
      #fluidRow(column(width = 4, checkboxInput("file", "File Upload")),column(width = 4, checkboxInput("input", "Text Input"))),
      radioButtons("i_type", "Input Type:",
                   c("File Upload" = "fup",
                     "Text Input" = "txt")),
      conditionalPanel(condition = "input.i_type == 'fup'",
      fileInput('inputFile', 'Upload Files',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      actionButton("fLoad", label = "Load Input File", class="btn-warning"),
      actionButton("fCorr", label = "Correlation Analysis", class="btn-primary"),
      downloadButton('downloadData', "Example File", class="btn-secondary")),
      conditionalPanel(condition = "input.i_type == 'txt'",
      textAreaInput("caption", "Text Input", "", placeholder = 
"Gene, BRCA1
CALML5, 0.234
CASP14, 0.568
KRT9, 0.238
CALML5, 0.816
ARG1, 0.439
KRT1, 0.711"),
      br(),
      actionButton("Load", label = "Load Text Input", class="btn-warning"),
      actionButton("Corr", label = "Correlation Analysis", class="btn-primary"),
      actionButton("Example", label = "Example Input", class="btn-secondary")
      ),
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
    tags$hr()
  ),
    
    
    #Main Panel Tab Panel Joint Action that display all the Correlation Analysis
    mainPanel(
      tabsetPanel(
        tabPanel("Add Gene/Experiment",
                 h4("Users Uploaded Table"),
                 #tableOutput("contents")
                 DT::dataTableOutput("fileUpload"),
                 DT::dataTableOutput("txtUpload")
        ),
        tabPanel("Pearson Correlation Rankings",
                 h4("Highest Pearson Correlated Genes"),
                 DT::dataTableOutput("topPearson"),
                 #h4("Highest Spearman Corrleated Genes"),
                 #DT::dataTableOutput("topSpearman"),
                 DT::dataTableOutput("txttop_10")
        ),
        tabPanel("Correlation Plots",
                 h4("Pearsons Correlation Coefficient Plot"),
                 conditionalPanel(condition="input.file1 != NULL",
                 plotOutput('trial')),
                 conditionalPanel(condition="input.corr != 0",
                 plotOutput('pearsonVis'))
        ),
        tabPanel("Working Database",
                 h4("Current Working Database"),
                 fluidRow(dataTableOutput('fullDb'))
        ),
        
        tabPanel("Data Information",
                 h4("Publication Data Information"),
                 DT::dataTableOutput("table1")
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
                                  min=0, max=1, value=0.3, 
                                  step=0.01, round=0.001),
                      br(),
                      checkboxInput('downfold', 'Downfold'),
                      checkboxInput('upfold', 'Upfold'),
                      checkboxInput('absolute','Absolute Value')
               ),
               column(4, h4("Single Gene Comparison"), offset = 1,
                      selectInput('gene1', 'Gene 1', names(data)),
                      selectInput('gene2', 'Gene 2', names(data), names(data)[[2]]),
                      actionButton("Compare", label = "Compare Genes", class="btn-success")
               ),
               column(4,h4("Correlation Information"),
                      tags$ul(
                        tags$li(tags$a(href="www.rstudio.com", "Pearson Correlation Coefficient")),
                        tags$li(tags$a(href="www.rstudio.com", "Spearman Correlation")),
                        tags$li(tags$a(href="www.rstudio.com", "Kendall Correlation")),
                        tags$li(tags$a(href="www.rstudio.com", "Vector/Matrix Calculus")),
                        tags$li(tags$a(href="www.rstudio.com", "Normalization"))
                      ),
                      h4("Sun Laboratory Information"),
                      tags$ul(
                        tags$li(tags$a(href="www.rstudio.com", "PSMD8 Publication")),
                        tags$li(tags$a(href="www.rstudio.com", "Publications")),
                        tags$li(tags$a(href="www.rstudio.com", "Contact"))
                      )
               )
             ), style = "info")
  ),
fluidRow(id = "suninfo", column(width = 4, helpText("Developed by: ",   a("Ken Miyachi", href="https://www.kenmiyachi.com/", target="_blank">"https://www.kenmiyachi.com/"))),
         column(width = 4, offset = 4, "{kjmiyachi, neimamoshiri, bryansunlab} @ gmail.com"))
)
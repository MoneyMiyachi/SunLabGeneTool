library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(xtable)
library(markdown)
library(corrplot)
library(shinyBS)
library(rmarkdown)
library(shinycssloaders)


corner_element = HTML(paste0('<a href=',shQuote(paste0("https://www.kenmiyachi.com/")), '>', 'Foo', '</a>'))


fluidPage(
  tags$head(tags$script(src="script.js")),
  ## link the CSS file
  tags$head(tags$link(rel="stylesheet", 
                      type="text/css",
                      href="style.css")),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden !important; }",
             ".shiny-output-error:before { visibility: hidden !important; }"
  ),
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Open+Sans:400,700|Lara:400,700i');
                    
                    h2 {
                    font-family: 'Lara', bold;
                    font-weight: 800;
                    line-height: 1.9;
                    color: rgb(24,43,100);
                    margin: auto;
                    text-align: center;
                    width: 90%;
                    font-size: 35px;
                    }

                    #suninfo {
                    font-family: 'Open Sans', bold;
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
  fluidRow(id = 'suninfo', column(width = 2, imageOutput("jacobsImage", height = 60)), column(width=1,offset=9, imageOutput("medImage", height = 60))),
  titlePanel("Human Keratinocyte Genome Perturbation Database (HKGP-Db)"),
  tags$head(tags$style(
    type="text/css",
    "#image jacobsImage {max-width: 100%; width: 100%; height: auto;}"
  )),
  fluidRow(id = "suninfo", column(width = 4, helpText(   a("Journal: HGKP-Db Tool", href="https://www.bryansunlab.com/", target="_blank">"https://www.bryansunlab.com/"))),
           column(width = 3, offset = 5, helpText(   a("Bryan Sun Laboratory: UCSD Dermatology", href="https://www.bryansunlab.com/", target="_blank">"https://www.bryansunlab.com/")))),
  bsCollapse(id = "collapseExample",
             bsCollapsePanel("Directions",
                             includeMarkdown("./Direction.md"), style = "success")
  ),
  #Sidebar stuff
  sidebarLayout(
    sidebarPanel(
      #fluidRow(column(width = 4, checkboxInput("file", "File Upload")),column(width = 4, checkboxInput("input", "Text Input"))),
      fluidRow(column(width = 4, radioButtons("i_type", "Input Type:",
                                              c("File Upload" = "fup",
                                                "Text Input" = "txt"))))
      ,
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
      actionButton("clear", label = "Clear Input/Output", class="btn-danger"),
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
                 conditionalPanel(condition="input.i_type == 'fup'",
                 DT::dataTableOutput("topPearson") %>% withSpinner(color="#0dc5c1")),
                 #h4("Highest Spearman Corrleated Genes"),
                 #DT::dataTableOutput(""),
                 conditionalPanel(condition="input.i_type == 'txt'",
                 DT::dataTableOutput("txttopPearson") %>% withSpinner(color="#0dc5c1"))
        ),
        tabPanel("Pearson Correlation Visualizations",
                 h4("Pearsons Correlation Coefficient Plot"),
                 conditionalPanel(condition="input.i_type == 'fup'",
                 #plotOutput('barPearson') %>% withSpinner(color="#0dc5c1"),
                 plotOutput('barPearson') %>% withSpinner(color="#0dc5c1")),
                 conditionalPanel(condition="input.i_type == 'txt'",
                 plotOutput('txtvis') %>% withSpinner(color="#0dc5c1"))
        ),
        tabPanel("Working Database",
                 h4("Current Working Database"),
                 fluidRow(dataTableOutput('fullDb') %>% withSpinner(color="#0dc5c1"))
        )
      )
    )
  ),
  #Extra Stuff that is completley arbritrary and irrelevant right now 
  bsCollapse(id = "main_collapse",
             bsCollapsePanel("Supporting Documentation",   fluidRow(
               column(4,
                      h4("Sun Lab Information"),
                      tags$ul(
                        tags$li(tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/29183730", "PSMD8 Publication")),
                        tags$li(tags$a(href="https://www.bryansunlab.com/publications", "Sun Lab Publications")),
                        tags$li(tags$a(href="https://www.bryansunlab.com/contact", "Contact"))
                      )
               ),
               column(4, h4("Correlation Information"),
                      tags$ul(
                        tags$li(tags$a(href="http://onlinestatbook.com/2/describing_bivariate_data/pearson.html", "Pearson Correlation Coefficient")),
                        tags$li(tags$a(href="https://en.wikipedia.org/wiki/Matrix_calculus", "Vector/Matrix Calculus")),
                        tags$li(tags$a(href="https://academic.oup.com/bioinformatics/article/28/20/2584/203544#2091068", "Normalization Techniques"))
                      )
               )
             ), style = "info")
  ),
fluidRow(id = "suninfo", column(width = 4, helpText("Developed by: ",   a("Ken Miyachi", href="https://www.kenmiyachi.com/", target="_blank">"https://www.kenmiyachi.com/"))),
         column(width = 4, offset = 4, "{kjmiyachi, neimamoshiri, bryansunlab} @ gmail.com"))
)
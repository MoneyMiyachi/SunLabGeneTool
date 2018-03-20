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
                    font-family: 'Open Sans', bold;
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
  titlePanel("Human Keratinocyte Gene Perturbation Database (HKGP-Db)"),
  tags$head(tags$style(
    type="text/css",
    "#image jacobsImage {max-width: 100%; width: 100%; height: auto;}"
  )),
  fluidRow(id = "suninfo", column(width = 4, helpText(   a("Journal: HGKP-Db Tool", href="https://www.bryansunlab.com/", target="_blank">"https://www.bryansunlab.com/"))),
           column(width = 3, offset = 5, helpText(   a("Bryan Sun Laboratory: UCSD Dermatology", href="https://www.bryansunlab.com/", target="_blank">"https://www.bryansunlab.com/")))),
  bsCollapse(id = "collapseExample",
             bsCollapsePanel("Directions",
                             includeMarkdown("/home/ckmiyachi/zscore/Direction.md"), style = "success")
  ),
  #Sidebar stuff
  sidebarLayout(
    sidebarPanel(
      #fluidRow(column(width = 4, checkboxInput("file", "File Upload")),column(width = 4, checkboxInput("input", "Text Input"))),
      fluidRow(column(width = 1, "1."), column(width = 11, radioButtons("i_type", "Input Type:",
                                              c("File Upload" = "fup",
                                                "Text Input" = "txt"), inline=T)))
      ,
      fluidRow(column(width = 1, "2."), column(width = 11, conditionalPanel(condition = "input.i_type == 'fup'",
      fileInput('inputFile', 'Upload Files',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))),conditionalPanel(condition = "input.i_type == 'txt'", textAreaInput("caption", "Text Input", "", placeholder = 
                                                                                                         "Gene, BRCA1
CALML5, 0.234
CASP14, 0.568
KRT9, 0.238
CALML5, 0.816
ARG1, 0.439
KRT1, 0.711"), br()))),
      fluidRow(column(width = 1, "3. "), column(width = 11,conditionalPanel(condition = "input.i_type == 'fup'",
      actionButton("fLoad", label = "Load Input File", class="btn-warning"),
      actionButton("fCorr", label = "Correlation Analysis", class="btn-primary")),
      conditionalPanel(condition = "input.i_type == 'txt'",
      actionButton("Load", label = "Load Text Input", class="btn-warning"),
      actionButton("Corr", label = "Correlation Analysis", class="btn-primary")
      ))),
      tags$hr(),
      fluidRow(column(width = 1, "4. "),
               column(width = 11,conditionalPanel(condition = "input.i_type == 'fup'",
                                                  downloadButton("downloadData", label = "Example File", class="btn-secondary"),
                                                  actionButton("clear", label = "Clear Input/Output", class="btn-danger")),
                      conditionalPanel(condition = "input.i_type == 'txt'",
                                       actionButton("Example", label = "Example Input", class="btn-secondary"),
                                       actionButton("clear", label = "Clear Input/Output", class="btn-danger")
                      ))
      ),
      #actionButton("dbAdd", label = "Add Experiment to HKGP-DB", class="btn-success"),
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
        tabPanel("Spearman Correlation Rankings",
                 h4("Highest Spearman Correlated Genes"),
                 conditionalPanel(condition="input.i_type == 'fup'",
                 DT::dataTableOutput("topPearson") %>% withSpinner(color="#0dc5c1")),
                 #h4("Highest Spearman Corrleated Genes"),
                 #DT::dataTableOutput("topSpearman"),
                 conditionalPanel(condition="input.i_type == 'txt'",
                 DT::dataTableOutput("txttopPearson") %>% withSpinner(color="#0dc5c1"))
        ),
        tabPanel("Correlation Bar Plot",
                 h4("Spearman Correlation Bar Plot"),
                 conditionalPanel(condition="input.i_type == 'fup'",
                                  #plotOutput('barPearson') %>% withSpinner(color="#0dc5c1"),
                                  plotOutput('barPearson') %>% withSpinner(color="#0dc5c1")),
                 conditionalPanel(condition="input.i_type == 'txt'",
                                  plotOutput('txtbarPearson') %>% withSpinner(color="#0dc5c1"))
        ),
        tabPanel("Working Database",
                 #fluidRow(column(width = 5,h4("Current Working Database")),column(width=5, offset = 2,
                #  actionButton("downloadWorking", label = "Download Full Database", class="btn-secondary"))),
                h4("Current Working Database"),
                #actionButton("downloadWorking", label = "Download Full Database", class="btn-secondary"),
                fluidRow(dataTableOutput('easyWorking') %>% withSpinner(color="#0dc5c1")),
                downloadButton("downloadWorking", label = "Download Full Database", class="btn-secondary"),
                tags$hr(),
                bsModal("KLF4_", "Data Table", "KLF4", size = "large",
                        dataTableOutput("KLF4_Table"), downloadButton("downloadKLF4", "Download")),
                bsModal("ZNF750_", "Data Table", "ZNF750", size = "large",
                        dataTableOutput("ZNF750_Table"), downloadButton("downloadZNF750", "Download")),
                bsModal("ACTL_", "Data Table", "ACTL", size = "large",
                        dataTableOutput("ACTL_Table"), downloadButton("downloadACTL6a", "Download")),
                bsModal("GRHL3_", "Data Table", "GRHL3", size = "large",
                        dataTableOutput("GRHL3_Table"), downloadButton("downloadGRHL3", "Download")),
                bsModal("MLL2_", "Data Table", "MLL2", size = "large",
                        dataTableOutput("MLL2_Table"), downloadButton("downloadMLL2", "Download")),
                bsModal("SNAI2_Over_", "Data Table", "SNAI2_Over", size = "large",
                        dataTableOutput("SNAI2_Over_Table"), downloadButton("downloadSNAI2_Over", "Download")),
                bsModal("SNAI2_Knockdown_", "Data Table", "SNAI2_Knockdown", size = "large",
                        dataTableOutput("SNAI2_Knockdown_Table"), downloadButton("downloadSNAI2_Down", "Download")),
                bsModal("DMNT1_", "Data Table", "DMNT1", size = "large",
                        dataTableOutput("DMNT1_Table"), downloadButton("downloadDMNT1", "Download")),
                bsModal("CBX4_Over_", "Data Table", "CBX4_Over", size = "large",
                        dataTableOutput("CBX4_Over_Table"), downloadButton("downloadCBX4_Over", "Download")),
                bsModal("CBX4_Knockdown_", "Data Table", "CBX4_Knockdown", size = "large",
                        dataTableOutput("CBX4_Knockdown_Table"), downloadButton("downloadCBX4_Down", "Download")),
                bsModal("MAFB1_", "Data Table", "MAFB1", size = "large",
                        dataTableOutput("MAFB1_Table"), downloadButton("downloadMAFB1", "Download")),
                bsModal("MPZL3_", "Data Table", "MPZL3", size = "large",
                        dataTableOutput("MPZL3_Table"), downloadButton("downloadMPZL3", "Download")),
                bsModal("FDXR_", "Data Table", "FDXR", size = "large",
                        dataTableOutput("FDXR_Table"), downloadButton("downloadFDXR", "Download")),
                bsModal("TINCR_", "Data Table", "TINCR", size = "large",
                        dataTableOutput("TINCR_Table"), downloadButton("downloadTINCR", "Download")),
                bsModal("CALML5_", "Data Table", "CALML5", size = "large",
                        dataTableOutput("CALML5_Table"), downloadButton("downloadCALML5", "Download")),
                bsModal("SFN_", "Data Table", "SFN", size = "large",
                        dataTableOutput("SFN_Table"), downloadButton("downloadSFN", "Download")),
                bsModal("ATF2_", "Data Table", "ATF2", size = "large",
                        dataTableOutput("ATF2_Table"), downloadButton("downloadATF2", "Download")),
                bsModal("ATF3_", "Data Table", "ATF3", size = "large",
                        dataTableOutput("ATF3_Table"), downloadButton("downloadATF3", "Download")),
                bsModal("ATF4_", "Data Table", "ATF4", size = "large",
                        dataTableOutput("ATF4_Table"), downloadButton("downloadATF4", "Download")),
                bsModal("ATF5_", "Data Table", "ATF5", size = "large",
                        dataTableOutput("ATF5_Table"), downloadButton("downloadATF5", "Download")),
                bsModal("ATF7_", "Data Table", "ATF7", size = "large",
                        dataTableOutput("ATF7_Table"), downloadButton("downloadATF7", "Download")),
                bsModal("BRIP1_", "Data Table", "BRIP1", size = "large",
                        dataTableOutput("BRIP1_Table"), downloadButton("downloadBRIP1", "Download")),
                bsModal("NOTCH3_", "Data Table", "NOTCH3", size = "large",
                        dataTableOutput("NOTCH3_Table"), downloadButton("downloadNOTCH3", "Download")),
                bsModal("CEBPG_", "Data Table", "CEBPG", size = "large",
                        dataTableOutput("CEBPG_Table"), downloadButton("downloadCEBPG", "Download")),
                bsModal("CREB5_", "Data Table", "CREB5", size = "large",
                        dataTableOutput("CREB5_Table"), downloadButton("downloadCREB5", "Download")),
                bsModal("E2F1_", "Data Table", "E2F1", size = "large",
                        dataTableOutput("E2F1_Table"), downloadButton("downloadE2F1", "Download")),
                bsModal("ETS1_", "Data Table", "ETS1", size = "large",
                        dataTableOutput("ETS1_Table"), downloadButton("downloadETS1", "Download")),
                bsModal("FOS_", "Data Table", "FOS", size = "large",
                        dataTableOutput("FOS_Table"), downloadButton("downloadFOS", "Download")),
                bsModal("FOSB_", "Data Table", "FOSB", size = "large",
                        dataTableOutput("FOSB_Table"), downloadButton("downloadFOSB", "Download")),
                bsModal("FOSL1_", "Data Table", "FOSL1", size = "large",
                        dataTableOutput("FOSL1_Table"), downloadButton("downloadFOSL1", "Download")),
                bsModal("FOSL2_", "Data Table", "FOSL2", size = "large",
                        dataTableOutput("FOSL2_Table"), downloadButton("downloadFOSL2", "Download")),
                bsModal("FOXD1_", "Data Table", "FOXD1", size = "large",
                        dataTableOutput("FOXD1_Table"), downloadButton("downloadFOXD1", "Download")),
                bsModal("GABP1_", "Data Table", "GABP1", size = "large",
                        dataTableOutput("GABP1_Table"), downloadButton("downloadGABP1", "Download")),
                bsModal("FOXN1_", "Data Table", "FOXN1", size = "large",
                        dataTableOutput("FOXN1_Table"), downloadButton("downloadFOXN1", "Download")),
                bsModal("FOXN2_", "Data Table", "FOXN2", size = "large",
                        dataTableOutput("FOXN2_Table"), downloadButton("downloadFOXN2", "Download")),
                bsModal("FOXP1_", "Data Table", "FOXP1", size = "large",
                        dataTableOutput("FOXP1_Table"), downloadButton("downloadFOXP1", "Download")),
                bsModal("GRHL1_", "Data Table", "GRHL1", size = "large",
                        dataTableOutput("GRHL1_Table"), downloadButton("downloadGRHL1", "Download")),
                bsModal("GRHL2_", "Data Table", "GRHL2", size = "large",
                        dataTableOutput("GRHL2_Table"), downloadButton("downloadGRHL2", "Download")),
                bsModal("GRHL3a_", "Data Table", "GRHL3a", size = "large",
                        dataTableOutput("GRHL3a_Table"), downloadButton("downloadGRHL3a", "Download")),
                bsModal("JUN_", "Data Table", "JUN", size = "large",
                        dataTableOutput("JUN_Table"), downloadButton("downloadJUN", "Download")),
                bsModal("JUNB_", "Data Table", "JUNB", size = "large",
                        dataTableOutput("JUNB_Table"), downloadButton("downloadJUNB", "Download")),
                bsModal("JUND_", "Data Table", "JUND", size = "large",
                        dataTableOutput("JUND_Table"), downloadButton("downloadJUND", "Download")),
                bsModal("KLF4a_", "Data Table", "KLF4a", size = "large",
                        dataTableOutput("KLF4a_Table"), downloadButton("downloadKLF4a", "Download")),
                bsModal("LRRFLP1_", "Data Table", "LRRFLP1", size = "large",
                        dataTableOutput("LRRFLP1_Table"), downloadButton("downloadLRRFLP1", "Download")),
                bsModal("PBX1_", "Data Table", "PBX1", size = "large",
                        dataTableOutput("PBX1_Table"), downloadButton("downloadPBX1", "Download")),
                bsModal("OVOL1_", "Data Table", "OVOL1", size = "large",
                        dataTableOutput("OVOL1_Table"), downloadButton("downloadOVOL1", "Download")),
                bsModal("OVOL2_", "Data Table", "OVOL2", size = "large",
                        dataTableOutput("OVOL2_Table"), downloadButton("downloadOVOL2", "Download")),
                bsModal("NR3C1_", "Data Table", "NR3C1", size = "large",
                        dataTableOutput("NR3C1_Table"), downloadButton("downloadNR3C1", "Download")),
                bsModal("RUNX1_", "Data Table", "RUNX1", size = "large",
                        dataTableOutput("RUNX1_Table"), downloadButton("downloadRUNX1", "Download")),
                bsModal("RORA_", "Data Table", "RORA", size = "large",
                        dataTableOutput("RORA_Table"), downloadButton("downloadRORA", "Download")),
                bsModal("RELB_", "Data Table", "RELB", size = "large",
                        dataTableOutput("RELB_Table"), downloadButton("downloadRELB", "Download")),
                bsModal("RBL1_", "Data Table", "RBL1", size = "large",
                        dataTableOutput("RBL1_Table"), downloadButton("downloadRBL1", "Download")),
                bsModal("RARG_", "Data Table", "RARG", size = "large",
                        dataTableOutput("RARG_Table"), downloadButton("downloadRARG", "Download")),
                bsModal("PRDM1_", "Data Table", "PRDM1", size = "large",
                        dataTableOutput("PRDM1_Table"), downloadButton("downloadPRDM1", "Download")),
                bsModal("PBX2_", "Data Table", "PBX2", size = "large",
                        dataTableOutput("PBX2_Table"), downloadButton("downloadPBX2", "Download")),
                bsModal("STAT3_", "Data Table", "STAT3", size = "large",
                        dataTableOutput("STAT3_Table"), downloadButton("downloadSTAT3", "Download")),
                bsModal("STAT1_", "Data Table", "STAT1", size = "large",
                        dataTableOutput("STAT1_Table"), downloadButton("downloadSTAT1", "Download")),
                bsModal("SP3_", "Data Table", "SP3", size = "large",
                        dataTableOutput("SP3_Table"), downloadButton("downloadSP3", "Download")),
                bsModal("SP1_", "Data Table", "SP1", size = "large",
                        dataTableOutput("SP1_Table"), downloadButton("downloadSP1", "Download")),
                bsModal("SOX6_", "Data Table", "SOX6", size = "large",
                        dataTableOutput("SOX6_Table"), downloadButton("downloadSOX6", "Download")),
                bsModal("SOX11_", "Data Table", "SOX11", size = "large",
                        dataTableOutput("SOX11_Table"), downloadButton("downloadSOX11", "Download")),
                bsModal("SMAD4_", "Data Table", "SMAD4", size = "large",
                        dataTableOutput("SMAD4_Table"), downloadButton("downloadSMAD4", "Download")),
                bsModal("RUNX2_", "Data Table", "RUNX2", size = "large",
                        dataTableOutput("RUNX2_Table"), downloadButton("downloadRUNX2", "Download")),
                bsModal("XBP1_", "Data Table", "XBP1", size = "large",
                        dataTableOutput("XBP1_Table"), downloadButton("downloadXBP1", "Download")),
                bsModal("TP63_", "Data Table", "TP63", size = "large",
                        dataTableOutput("TP63_Table"), downloadButton("downloadTP63", "Download")),
                bsModal("TCF7L2_", "Data Table", "TCF7L2", size = "large",
                        dataTableOutput("TCF7L2_Table"), downloadButton("downloadTCF7L2", "Download")),
                bsModal("TCF4_", "Data Table", "TCF4", size = "large",
                        dataTableOutput("TCF4_Table"), downloadButton("downloadTCF4", "Download")),
                bsModal("STAT6_", "Data Table", "STAT6", size = "large",
                        dataTableOutput("STAT6_Table"), downloadButton("downloadSTAT6", "Download"))
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
                        tags$li(tags$a(href="https://statistics.laerd.com/statistical-guides/spearmans-rank-order-correlation-statistical-guide.php", "Spearman Rank-Order Correlation")),
                        tags$li(tags$a(href="https://en.wikipedia.org/wiki/Matrix_calculus", "Vector/Matrix Calculus")),
                        tags$li(tags$a(href="https://academic.oup.com/bioinformatics/article/28/20/2584/203544#2091068", "Normalization Techniques"))
                      )
               )
             ), style = "info")
  ),
fluidRow(id = "suninfo", column(width = 4, helpText("Developed by: ",   a("Ken Miyachi", href="https://www.kenmiyachi.com/", target="_blank">"https://www.kenmiyachi.com/"))),
         column(width = 4, offset = 4, "{kjmiyachi, neimamoshiri, bryansunlab} @ gmail.com"))
)
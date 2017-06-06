
## app.R ##
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(xtable)

function(input, output) {
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  }, options = list( 
    scrollY = '300px', paging = FALSE 
  ))
  
  output$foo = renderDataTable({ 
    data 
  }, options = list( 
    scrollY = '300px', paging = FALSE 
  ))
  
  output$x = renderPlot({corrplot.mixed(M, addrect=2, col=col5(20), tl.pos = "lt", diag = "u")})
  

}




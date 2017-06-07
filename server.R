setwd("~/Research/database/SunLabGeneTool")
## app.R ##
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(xtable)
library(markdown)

function(input, output) {
  
  output$contents <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  },
  options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  
  output$originalfiles <- renderDataTable(
    input$file1,
    options = list(dom = ",", searching = FALSE)
  )
  
  
  output$foo = renderDataTable({ 
    data 
  }, options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  
  output$x = renderPlot({corrplot.mixed(M, addrect=2, col=col5(20), tl.pos = "lt", diag = "u")})
  

}




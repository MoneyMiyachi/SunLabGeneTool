setwd("~/Research/database/SunLabGeneTool")
## app.R ##
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(xtable)
library(markdown)
library(rdrop2)
library(DT)
library(corrplot)
library(shinyBS)
token <- drop_auth()
saveRDS(token, "droptoken.rds")
token <- readRDS("droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
# Then pass the token to each drop_ function
drop_acc(dtoken = token)


function(input, output, session) {
  
  output$genericPlot <- renderPlot(plot(rnorm(100)))
  observeEvent(input$p1Button, ({
    updateCollapse(session, "collapseExample", open = "Panel 1")
  }))
  observeEvent(input$styleSelect, ({
    updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
  }))
  
  output$value <- renderText({ input$caption })
  
  data1 <- reactive({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    
    isolate({ 
      input$Load
      my_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
    })
    my_data
  })
  

  
  output$isolated <- renderDataTable({ data1() },options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  
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
  
  
  
  #New new rdrop stuff that I used to test
  outputDir <- "Ken Miyachi/testing"
  # Upload the file to Dropbox
  
  
  #drop_upload("/Ken Miyachi/Database/Working_DB/Almost_Complete_db.csv")
  table <- drop_read_csv("/Ken Miyachi/Database/Working_DB/Almost_Complete_db.csv")
  #new_test <- t(table)
  output$idk <- renderDataTable({ table },options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  
  
  col5 <- colorRampPalette(c("yellow","cyan","green", "blue", "purple","red"))
  col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                             "cyan", "#007FFF", "blue","#00007F")) 
  clean_db <- table
  clean_db$Gene <- NULL
  M <- cor(clean_db, use="pairwise.complete.obs")
  output$x = renderPlot({corrplot.mixed(M, addrect=2, col=col5(20), tl.pos = "lt", diag = "u")})
  

  pearson <- reactive ({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    isolate({ 
      input$Load
      new_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      merged <- merge(table,new_data, by = "Gene", all=T)
      colnames(merged)[1] <- "Gene"
      merged <- aggregate(merged[, 2:ncol(merged)], list(merged$Gene), mean)
      clean_db <- merged[, sapply(merged, is.numeric)]
      clean_db$X <- NULL
      Pearsons <- cor(clean_db, use="pairwise.complete.obs")
      
      x <- data.frame(Pearsons)
      new <- x[ncol(x),]
      new <- new[-length(new)]
      new <- new[order(new, decreasing = T)]
      new <- t(new)
      new <- head(new, n = 10)
      gene_select <- row.names(new)
      pleasework <- clean_db[,gene_select]
      newPearsons <- cor(pleasework, use="pairwise.complete.obs")
    })
    newPearsons
  })
  output$trial = renderPlot({corrplot.mixed(pearson(), addrect=2, col=col5(20), tl.pos = "lt", diag = "u")})
  
  
  
  
 
  
  
  
  hope <- reactive ({
    if(input$Load == 0){return()}
    inFile <- input$file1
    if (is.null(inFile)){return(NULL)}
    isolate({ 
      input$Load
      new_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      merged <- merge(table,new_data, by = "Gene", all=T)
      colnames(merged)[1] <- "Gene"
      merged <- aggregate(merged[, 2:ncol(merged)], list(merged$Gene), mean)
      clean_db <- merged[, sapply(merged, is.numeric)]
      clean_db$X <- NULL
      Pearsons <- cor(clean_db, use="pairwise.complete.obs")
      x <- data.frame(Pearsons)
      new <- x[ncol(x),]
      new <- new[-length(new)]
      new <- new[order(new, decreasing = T)]
      new <- t(new)
      colnames(new) <- "Pearson Correlation Coefficient"
    })
    new
  })
  
  
  
  
  
  
  output$top_10 <- renderDataTable({data.frame(head(hope(), n = 10))},options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  
  
  output$ranking = renderDataTable({hope()},options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
}




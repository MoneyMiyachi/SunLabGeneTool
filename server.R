##setwd("~/Research/database/SunLabGeneTool")
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
library(rsconnect)
library(RMySQL)
library(RODBC)


#Connects to the MySQL Server and grabs the full table needed to compute correlation statistics
drv = dbDriver("MySQL")
con = dbConnect(drv,host="localhost",dbname="HGKP_Db",user="root",pass="tomohiro1")
working = dbGetQuery(con,statement="select * from Fulltable")
pub_info = dbGetQuery(con,statement="SELECT * FROM pub_info")

new = mtcars

inputType <- function(x, type) {
  switch(type,
         fup = hist(x),
         txt = barplot(x))
}


createLink <- function(val) {
  sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
}

#This is the outer wrapper that all input and output is gotten from 
function(input, output, session) {
  
  #output$debug <- renderDataTable({new})
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    mtcars
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    },
    contentType = 'csv'
  )
  
  output$download_data <- downloadHandler(
    filename = "portals_subset.csv",
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  #Output: fileUpload - the data table this is uploaded by the user when she submits a csv/txt file
  output$fileUpload <- renderDataTable({
    inFile <- input$inputFile
    if (input$fLoad == 0)
      return(NULL)
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  },
  options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  

  #This is the Data that gets rendered to the text input when the user clicks 'Example Input'
  observeEvent(input$Example, {
    updateTextInput(session, "caption",
                    value = 
"Gene, BRCA1
CALML5, 0.234
CASP14, 0.568
KRT9, 0.238
CALML5, 0.816
ARG1, 0.439
KRT1, 0.711")
  })
    
  
  
  # This parses the inpute file and outputs the pearson correlation coefficients to the different genes that have been gathered in the database
  # Outputs the Top 10 Correlated Genes :) via the cor package in R using 'Pearson' paramater
  Pearson_cor <- reactive ({
    if(input$fCorr == 0){return()}
    inFile <- input$inputFile
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
  
  
  output$preImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "./ucsdmed.jpg"
    
    # Return a list containing the filename and alt text
    list(src = filename, width="100", 
         height="60", 
         alt = paste("Image number", input$n))
    
  }, deleteFile = FALSE)
  
  #This gives me the top 10 for the pearson correlation coefficient and it is what I'm using so
  #Fuck Yeah
  output$topPearson <- renderDataTable({data.frame(head(Pearson_cor(), n = 10))},options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  
  
  #Spearman Top 10 Isolation action
  
  
  # spearman <- reactive ({
  #   if(input$fCorr == 0){return()}
  #   inFile <- input$inputFile
  #   if (is.null(inFile)){return(NULL)}
  #   isolate({
  #     input$Load
  #     new_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
  #     merged <- merge(table,new_data, by = "Gene", all=T)
  #     colnames(merged)[1] <- "Gene"
  #     merged <- aggregate(merged[, 2:ncol(merged)], list(merged$Gene), mean)
  #     clean_db <- merged[, sapply(merged, is.numeric)]
  #     clean_db$X <- NULL
  #     Pearsons <- cor(clean_db, use="pairwise.complete.obs", method="spearman")
  #     x <- data.frame(Pearsons)
  #     new <- x[ncol(x),]
  #     new <- new[-length(new)]
  #     new <- new[order(new, decreasing = T)]
  #     new <- t(new)
  #     colnames(new) <- "Spearman Correlation"
  #   })
  #   new
  # })
  # 
  # 
  # output$topSpearman <- renderDataTable({data.frame(head(spearman(), n = 10))},options = list( 
  #   scrollY = '300px', paging = FALSE, scrollX = TRUE
  # ))
   
  
  
  #This is the pearson correlation coefficient logic which I think works as of now
  pearson <- reactive ({
    tryCatch({
      if(input$fCorr == 0){return(NULL)}
      inFile <- input$inputFile
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
    }, error=function(e) {
      return(NULL)
    })
  })
  
  #Renders the actual plot of the Input File
    if(is.null(pearson)) {output$trial = NULL}
    else {
      output$trial = renderPlot({corrplot.mixed(pearson(), addrect=2, col=col5(20), tl.pos = "lt", diag = "u")})
    }

  
  
  
  
  
  
  #Renders the Working Database that is currently being used in the system 
  ##table <- drop_read_csv("/Ken Miyachi/Database/Big Addition/bigAddition_db.csv")
  table = read.csv("./Data/bigAddition_db.csv")
  table$X <- NULL
  #new_test <- t(table)
  output$idk <- renderDataTable({ working },options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE
  ))
  
  
  
  #Outputs some caption
  output$value <- renderText({ input$caption })
  
  
  #This is the part of code that outputs the input file right when it is loaded into the page
  data1 <- reactive({
    if(input$fLoad == 0){return()}
    inFile <- input$inputFile
    if (is.null(inFile)){return(NULL)}
    
    
    #Isolates the infile on a button push?
    isolate({ 
      input$fLoad
      my_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
    })
    my_data
  })
  

  #Renders the outputted table
  output$isolated <- renderDataTable({ data1() },options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  

  
  output$originalfiles <- renderDataTable(
    input$inputFile,
    options = list(dom = ",", searching = FALSE)
  )
  
  
  
  #New new rdrop stuff that I used to test
  outputDir <- "Ken Miyachi/testing"
  # Upload the file to Dropbox
  
  
  #drop_upload("/Ken Miyachi/Database/Working_DB/Almost_Complete_db.csv")
  #Reads in the big database from dropbox 

  
  
  col5 <- colorRampPalette(c("yellow","cyan","green", "blue", "purple","red"))
  col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                             "cyan", "#007FFF", "blue","#00007F")) 
  clean_db <- table
  clean_db$Gene <- NULL
  M <- cor(clean_db, use="pairwise.complete.obs")
  output$x = renderPlot({corrplot.mixed(M, addrect=2, col=col5(20), tl.pos = "lt", diag = "u")})
  

  current <- read.csv("./Data/bigAddition_db.csv")
  output$fullDb = renderDataTable({ data.frame(working) },options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE, pageLength = 500
  ))
  
  output$pub_info = renderDataTable({
    my_table = pub_info
    my_table$ID = NULL
    return(my_table)}, options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE, pageLength = 500
  ))
  
  output$table1 <- renderDataTable({
    
    my_table <- pub_info
    my_table$ID <- NULL
    my_table$Notes <- NULL
    #colnames(my_table)[1] <- 'Gene'
    my_table$link <- createLink(my_table$DOI)
    return(my_table)
    
  },  options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE, pageLength = 25
  ), escape = FALSE)
  
  #Text upload 
  txtInput= reactive({
      if (input$Load == 0) {return(NULL)}
      if(input$caption == ""){return()}
      isolate({
        con <- textConnection(input$caption)
        x <- data.frame(paste(input$caption), stringsAsFactors = F )
        y <- read.csv(con, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      })
      y
  })
  
  #Output of Text upload
  output$txtUpload <- renderDataTable({ txtInput() },options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE, , pageLength = 500
  ))
  
  # This parses the inpute file for the top 10 pearson correlation coefficient genes
  txthope <- reactive ({
    if(input$Corr == 0){return(NULL)}
    inFile <- input$inputFile
    if(input$caption == ""){return(NULL)}
    isolate({ 
      input$Load
      con <- textConnection(input$caption)
      x <- data.frame(paste(input$caption), stringsAsFactors = F )
      y <- read.csv(con, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      merged <- merge(table,y, by = "Gene", all=T)
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
  
  output$txttop_10 <- renderDataTable({data.frame(head(txthope(), n = 10))},options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE, pageLength = 500
  ))
  
  
  #This is the pearson correlation coefficient logic which I think works as of now
  pearsonVis <- reactive ({
    if(input$Corr == 0){return()}
    inFile <- input$inputFile
    if(input$caption == ""){return()}
    isolate({ 
      con <- textConnection(input$caption)
      x <- data.frame(paste(input$caption), stringsAsFactors = F )
      y <- read.csv(con, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      merged <- merge(table,y, by = "Gene", all=T)
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
  #Renders the actual plot of the Input File
  output$txtpearson = reactive({
    renderPlot({corrplot.mixed(txtpearson(), addrect=2, col=col5(20), tl.pos = "lt", diag = "u")})
  })
  
  
  output$text1 <- renderText({ 
    paste("Gene, BRCA1
           Gene1, 0.234
           Gene2, 0.787", input$caption)
  })
  
  
  
  # kendall <- reactive ({
  #   if(input$fCorr == 0){return()}
  #   inFile <- input$inputFile
  #   if (is.null(inFile)){return(NULL)}
  #   isolate({ 
  #     input$Load
  #     new_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
  #     merged <- merge(table,new_data, by = "Gene", all=T)
  #     colnames(merged)[1] <- "Gene"
  #     merged <- aggregate(merged[, 2:ncol(merged)], list(merged$Gene), mean)
  #     clean_db <- merged[, sapply(merged, is.numeric)]
  #     clean_db$X <- NULL
  #     Pearsons <- cor(clean_db, use="pairwise.complete.obs", method="kendall")
  #     x <- data.frame(Pearsons)
  #     new <- x[ncol(x),]
  #     new <- new[-length(new)]
  #     new <- new[order(new, decreasing = T)]
  #     new <- t(new)
  #     colnames(new) <- "Pearson Correlation Coefficient"
  #   })
  #   new
  # })
  # 
  # output$kendall_10 <- renderDataTable({data.frame(head(kendall(), n = 10))},options = list( 
  #   scrollY = '300px', paging = FALSE, scrollX = TRUE
  # ))
}




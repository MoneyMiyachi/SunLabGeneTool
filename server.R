##setwd("~/Research/database/CleanUI")
## app.R ##
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(DT)
library(xtable)
library(markdown)
library(DT)
library(corrplot)
library(shinyBS)
library(rsconnect)
library(RMySQL)
library(RODBC)


#Connects to the MySQL Server and grabs the full table needed to compute correlation statistics
drv = dbDriver("MySQL")
con = dbConnect(drv,host="127.0.0.1",dbname="hkgpdb",user="root",pass="@TomarikenHir4463")
working = dbGetQuery(con,statement="select * from Fulltable")
working = data.frame(working)
pub_info = dbGetQuery(con,statement="SELECT * FROM pub_info")



#Functions to Render HTML Buttons that Link to Data 
createLink <- function(val,name ) {
  sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-success">%s</a>',val,val)
}
createnewLink <- function(val,name ) {
  sprintf('<a href="%s" target="_blank" class="btn btn-success">%s</a>',val,name)
}


#Loading Locally Stored CSV Files 
working <- read.csv("/home/ckmiyachi/clean/working.csv")
working = data.frame(working)
experiment_link <- read.csv("/home/ckmiyachi/clean/experiment_links.csv", stringsAsFactors = F)
experiment_link <- data.frame(experiment_link)
trial_file = read.csv("/home/ckmiyachi/clean/trial_file.csv")
trial_file$X = NULL
pub_info = read.csv("/home/ckmiyachi/clean/data_info.csv")
pub_info = head(pub_info, n = 22)
pub_info = data.frame(pub_info)
pub_info$Notes = NULL
pub_info$ID = NULL



inputType <- function(x, type) {
  switch(input$i_type,
         'fup' = hist(x),
         'txt' = barplot(x))
}

col5 <- colorRampPalette(c("yellow","cyan","green", "blue", "purple","red"))
whiteblack <- c("white", "black")

#This is the outer wrapper that all input and output is gotten from 
function(input, output, session) {
  

  
#----------------------------------------- UI Functions ---------------------------------#
    
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
  
  #Clears the Session
  observeEvent(input$clear, {
    session$reload()
  })
  
  #UCSD Med School Image
  output$medImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "./ucsdmed.jpg"
    
    # Return a list containing the filename and alt text
    list(src = filename, width="100", 
         height="60", 
         alt = paste("Image number", input$n))
    
  }, deleteFile = FALSE)
  
  
  #UCSD Jacobs School of Engineering Image
  output$jacobsImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "./ucsdjacobs.jpg"
    
    # Return a list containing the filename and alt text
    list(src = filename, width="120", 
         height="70", 
         alt = paste("Image number", input$n))
    
  }, deleteFile = FALSE)
  
  
#----------------------------------------- Miscellaneous ---------------------------------#
  #Output: downloadData - Example CSV File the user can download to visualize proper input
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("trial", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(trial_file, file, row.names = FALSE)
    },
    contentType = 'csv'
  )
#----------------------------------------- Renders Users Input Data ---------------------------------#
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
    
    #Text upload 
    txtInput= reactive({
      if (input$Load == 0) {return(NULL)}
      if(input$caption == ""){return()}
      isolate({
        con <- textConnection(input$caption)
        y <- read.csv(con, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      })
      y
    })
    
    #Output of Text upload
    output$txtUpload <- renderDataTable({ txtInput() },options = list( 
      scrollY = '300px', paging = FALSE, scrollX = TRUE, pageLength = 500
    ))
  
#----------------------------------------- Renders Correlation Tables ----------------------------------#

    # This parses the inpute file and outputs the pearson correlation coefficients to the different genes that have been gathered in the database
    # Outputs the Top 10 Correlated Genes :) via the cor package in R using 'Pearson' paramater
    Pearson_cor <- reactive ({
      if(input$fCorr == 0){return()}
      inFile <- input$inputFile
      if (is.null(inFile)){return(NULL)}
      isolate({ 
        input$Load
        new_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
        merged <- merge(working,new_data, by = "Gene", all=T)
        colnames(merged)[1] <- "Gene"
        merged <- aggregate(merged[, 2:ncol(merged)], list(merged$Gene), mean)
        clean_db <- merged[, sapply(merged, is.numeric)]
        id = rownames(clean_db)
        clean_db$X <- NULL
        Pearsons <- cor(clean_db, use="pairwise.complete.obs")
        x <- data.frame(Pearsons)
        new <- x[ncol(x),]
        new <- new[-length(new)]
        new <- new[order(new, decreasing = T)]
        new <- t(new)
        new <- data.frame(new)
        colnames(new) <- "Pearson"
        #new$Link <- createLink(new$Pearson)
      })
      new
    })
    
    #Top 10 Rankings for Pearson Correlation Coefficient of Input Experiment
    output$topPearson <- renderDataTable({ 
      x = data.frame(head(Pearson_cor(), n = 10))
      genes <- createLink(row.names(x))
      y = data.frame(genes,x$Pearson)
      colnames(y) <- c('Genes','Pearson Correlation')
      return(y)
    },options = list( 
      scrollY = '300px', paging = FALSE, scrollX = TRUE
    ), escape = FALSE)
    
    #Top 10 Rankings for Pearson Correlation Coefficient of Input Experiment
    output$barPearson <- renderPlot({ 
      x = data.frame(head(Pearson_cor(), n = 10))
      genes <- row.names(x)
      #exp_filter <- experiment_link[experiment_link$Experiment == genes,]
      x$Gene <- row.names(x)
      x$Correlation_Ranking = x$Pearson
      y = data.frame(x$Gene,x$Correlation_Ranking)
      colnames(y) = c("Gene", "P")
      n = y$Gene
      n = rev(n)
      rank <- y$P
      rank <- sort(rank, decreasing = F)
      par(mar=c(4,10,4,2))
      return(barplot(rank, main = "Pearson Correlation Bar Plot", xlab = "Pearson Correlation Coefficient",
                     horiz = TRUE, las=2, names.arg=n, col="#0dc5c1"))
    })
    
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
        merged <- merge(working,y, by = "Gene", all=T)
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
    
    output$txttopPearson <- renderDataTable({ 
      x = data.frame(head(txthope(), n = 10))
      x$Gene <- createLink(row.names(x))
      x$Correlation_Ranking = x$Pearson
      y = data.frame(x$Gene,x$Correlation_Ranking)
      
      colnames(y) = c("Gene", "Pearson Correlation Coefficient")
      return(y)
    },options = list( 
      scrollY = '300px', paging = FALSE, scrollX = TRUE
    ), escape = FALSE)
    
#--------------------------------------- Renders Correlation Visualizations ----------------------------#

    pearson <- reactive ({
      tryCatch({
        if(input$fCorr == 0){return(NULL)}
        inFile <- input$inputFile
        if (is.null(inFile)){return(NULL)}
        isolate({ 
          input$Load
          new_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
          merged <- merge(working,new_data, by = "Gene", all=T)
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
          gene_select <- append(colnames(new),row.names(new))
          newP <- subset(x,select=gene_select)
          newP <- newP[gene_select,]
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
      par(mar=c(7,2,25,2))
      output$trial = renderPlot({corrplot.mixed(pearson(), addrect=2, tl.pos = "lt", diag = "u", 
                                                lower.col = "black", number.cex = .7, upper.col = col5(20), mar=c(0,10,0,0))})
    }
    
    
    #This is the pearson correlation coefficient logic which I think works as of now
    pearsonVis <- reactive ({
      if(input$Corr == 0){return()}
      inFile <- input$inputFile
      #if(input$caption == ""){return()}
      isolate({ 
        con <- textConnection(input$caption)
        x <- data.frame(paste(input$caption), stringsAsFactors = F )
        y <- read.csv(con, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
        merged <- merge(working,y, by = "Gene", all=T)
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
    if(is.null(pearsonVis)) {output$txtvis = NULL}
    else {
      output$txtvis = renderPlot({corrplot.mixed(pearsonVis(), addrect=2, tl.pos = "lt", diag = "u", lower.col = "black", number.cex = .7, upper.col = col5(20))})
    }
    
    
#---------------------------------------- Renders Current Database ------------------------------------#
    output$fullDb = renderDataTable({ data.frame(working) },options = list( 
      scrollY = '350px', paging = TRUE, scrollX = TRUE, pageLength = 500
    ))
    

    
    
#----------------------------------------   END OF WORKING CODE ------------------------#
    
    

    
#---------------------------------------- Trial Functions ------------------------#
    
  finalPearson <- reactive ({
    if(input$fCorr == 0){return()}
    inFile <- input$inputFile
    if (is.null(inFile)){return(NULL)}
    isolate({ 
      input$Load
      new_data <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      merged <- merge(working,new_data, by = "Gene", all=T)
      colnames(merged)[1] <- "Gene"
      merged <- aggregate(merged[, 2:ncol(merged)], list(merged$Gene), mean)
      clean_db <- merged[, sapply(merged, is.numeric)]
      id = rownames(clean_db)
      clean_db$X <- NULL
      Pearsons <- cor(clean_db, use="pairwise.complete.obs")
      x <- data.frame(Pearsons)
      new <- x
      #new$Link <- createLink(new$Pearson)
    })
    new
  })
  
  output$newtopPearson <- renderDataTable({
    x <- data.frame(finalPearson())
    new <- x[ncol(x),]
    new <- new[-length(new)]
    new <- new[order(new, decreasing = T)]
    new <- t(new)
    new <- data.frame(new)
    colnames(new) <- "Pearson"
    newx = data.frame(head(new, n = 10))
    genes <- row.names(newx)
    exp_filter <- experiment_link[experiment_link$Experiment == genes,]
    newx$Gene <- createnewLink(exp_filter$New_Link,genes)
    newx$Correlation_Ranking = newx$Pearson
    y = data.frame(newx$Gene,newx$Correlation_Ranking)
    
    colnames(y) = c("Gene", "Pearson Correlation Coefficient")
    return(y)
  },options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ), escape = FALSE)
  

  output$ptrial <- renderDataTable({ 
    x = data.frame(fuckPearson())
    genes <- row.names(x)
    #exp_filter <- experiment_link[experiment_link$Experiment == genes,]
    #x$Gene <- createnewLink(row.names(x))
    #x$Correlation_Ranking = x$Pearson
    #y = data.frame(x$Gene,x$Correlation_Ranking)
    
    #colnames(y) = c("Gene", "Pearson Correlation Coefficient")
    return(x)
  },options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ), escape = FALSE)
  
  output$toplink <- renderDataTable({ 
    x = data.frame(head(experiment_link, n = 10))
    x$new <- createnewLink(x$New_Link,x$Experiment)
    #x$Correlation_Ranking = x$Pearson
    #y = data.frame(x$Gene,x$Correlation_Ranking)
    
    #colnames(y) = c("Gene", "Pearson Correlation Coefficient")
    return(x)
  },options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ), escape = FALSE)

  #----------------------------------- Unused Functions ** Maybe Helpful Later ------------------------#
  
  #Publication Data Information (This might be unneccesary as well)
  output$data_info = renderDataTable({ data.frame(pub_info) }, options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE
  ))

}




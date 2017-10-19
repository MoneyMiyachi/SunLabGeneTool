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





token <- drop_auth()
saveRDS(token, "./droptoken.rds")
token <- readRDS("./droptoken.rds")
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



#This is the outer wrapper that all input and output is gotten from 
function(input, output, session) {
  
  
  #This is what uploads the table when you click Load Input File
  output$contents <- renderDataTable({
    inFile <- input$file1
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
  
  #drop_create(path = "/Ken Miyachi/foobar")
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
  
  observeEvent(input$fAdd,{
    inFile <- input$file1
    if (!is.null(inFile)) {
      x <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
      write.csv(x,"trial_file.csv")
      drop_upload("trial_file.csv", dest = "/Ken Miyachi/foobar")
    }}
    #drop_upload("shit.csv", dest = "/Ken Miyachi/foobar")}
  )
  
  
  
  observeEvent(input$Add,{
    inFile <- input$file1
    if(!input$caption == "") {
      con <- textConnection(input$caption)
      x <- data.frame(paste(input$caption), stringsAsFactors = F )
      y <- read.csv(con, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      write.csv(y,"second_trial_file.csv")
      drop_upload("second_trial_file.csv", dest = "/Ken Miyachi/foobar")
    }}
    #drop_upload("shit.csv", dest = "/Ken Miyachi/foobar")}
  )
  
  
  
  reactive ({ 
  if (input$fAdd == 1) {
    if (!is.null(inFile)) {
      x <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                    quote=input$quote)
      write.csv(x,"trial_file.csv")
      #drop_upload(what = "trial_file.csv", filename = "/Ken Miyachi/foobar")
    }
  }
  })
  
  
  fuck <- reactive ({
    if (input$fAdd == 0) return(NULL)
    if (is.null(inFile))
      return(NULL)
    x <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                  quote=input$quote)
    write.csv(x,"trial_file.csv")
    #drop_upload("trial_file.csv", dest = "/Ken Miyachi/foobar")
    x
  })
    
  
  
  # This parses the inpute file for the top 10 pearson correlation coefficient genes
  hope <- reactive ({
    if(input$fCorr == 0){return()}
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
  
  
  
  
  
  #This gives me the top 10 for the pearson correlation coefficient and it is what I'm using so
  #Fuck Yeah
  output$top_10 <- renderDataTable({data.frame(head(hope(), n = 10))},options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  
  # #This might be the actual pearson correlation coefficient I'm using as well
  # output$ranking = renderDataTable({hope()},options = list( 
  #   scrollY = '300px', paging = FALSE, scrollX = TRUE
  # ))
  
  #Spearman Top 10 Isolation action
  spearman <- reactive ({
    if(input$fCorr == 0){return()}
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
      Pearsons <- cor(clean_db, use="pairwise.complete.obs", method="spearman")
      x <- data.frame(Pearsons)
      new <- x[ncol(x),]
      new <- new[-length(new)]
      new <- new[order(new, decreasing = T)]
      new <- t(new)
      colnames(new) <- "Spearman Correlation"
    })
    new
  })


  output$spear_10 <- renderDataTable({data.frame(head(spearman(), n = 10))},options = list(
        scrollY = '300px', paging = FALSE, scrollX = TRUE
      ))
   
  
  #This is the pearson correlation coefficient logic which I think works as of now
  pearson <- reactive ({
    tryCatch({
      if(input$fCorr == 0){return(NULL)}
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
  output$idk <- renderDataTable({ table },options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE
  ))
  
  
  
  #Outputs some caption
  output$value <- renderText({ input$caption })
  
  
  #This is the part of code that outputs the input file right when it is loaded into the page
  data1 <- reactive({
    if(input$fLoad == 0){return()}
    inFile <- input$file1
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
    input$file1,
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
  output$shit = renderDataTable({ data.frame(current) },options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE
  ))
  
  #I do not think these next two blocks of code do shit
  data2= reactive({
      if (input$Load == 0) {return(NULL)}
      if(input$caption == ""){return()}
      isolate({
        con <- textConnection(input$caption)
        x <- data.frame(paste(input$caption), stringsAsFactors = F )
        y <- read.csv(con, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      })
      y
  })
  
  
  output$other <- renderDataTable({ data2() },options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  
  # This parses the inpute file for the top 10 pearson correlation coefficient genes
  txthope <- reactive ({
    if(input$Corr == 0){return(NULL)}
    inFile <- input$file1
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
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ))
  
  
  #This is the pearson correlation coefficient logic which I think works as of now
  txtpearson <- reactive ({
    if(input$Corr == 0){return()}
    inFile <- input$file1
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
  #   inFile <- input$file1
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




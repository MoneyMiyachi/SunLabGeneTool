
observeEvent(input$fAdd,{
  inFile <- input$inputFile
  if (!is.null(inFile)) {
    x <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                  quote=input$quote)
    write.csv(x,"trial_file.csv")
    drop_upload("trial_file.csv", dest = "/Ken Miyachi/foobar")
  }}
  #drop_upload("shit.csv", dest = "/Ken Miyachi/foobar")}
)



observeEvent(input$Add,{
  inFile <- input$inputFile
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




working <- read.csv("./bigAddition_db.csv", stringsAsFactors = F)
Data <- subset( working, select = -c( KLF4 : SFN ))
new <- Data[complete.cases(Data),]
rownames(new) <- 1:nrow(new)

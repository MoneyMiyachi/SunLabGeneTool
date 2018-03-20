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
dbDisconnect(con)

f <- function(x) {
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] = median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}

#working = data.frame(apply(working,2,f))
#working = data.frame(scale(working))

#Link Creation Buttons
createLink <- function(e_link,val) {
  #e_link <- link[,val]
  sprintf('<a href="%s" target="_blank" class="btn btn-success">%s</a>',e_link,val)
}
createnewLink <- function(val) {
  e_link <- link[,val]
  sprintf('<a href="%s" target="_blank" id="show" class="btn btn-info">%s</a>',e_link,val)
}
createModal <- function(val) {
  e_link <- link[,val]
  sprintf('<button class="btn btn-info action-button shiny-bound-input" 
          id=%s data-toggle="modal" data-target="#%s_"type="button">%s</button>',val,val,val)
}

#Loading Locally Stored CSV Files 
#working <- read.csv("/home/ckmiyachi/clean/working.csv")
#working = data.frame(working)
experiment_link <- read.csv("/home/ckmiyachi/zscore/experiment_links.csv", stringsAsFactors = F)
experiment_link <- data.frame(experiment_link)
link <- experiment_link$New_Link
link <- t(link)
colnames(link) <- experiment_link$Experiment
trial_file = read.csv("/home/ckmiyachi/clean/trial_file.csv")
trial_file$X = NULL
pub_info = read.csv("/home/ckmiyachi/clean/data_info.csv")
pub_info = head(pub_info, n = 22)
pub_info = data.frame(pub_info)
pub_info$Notes = NULL
pub_info$ID = NULL
klf4 <- read.csv("/home/ckmiyachi/zscore/experiments/KLF4.csv")
znf750 <- read.csv("/home/ckmiyachi/zscore/experiments/ZNF750.csv")
ACTL6a <- read.csv("/home/ckmiyachi/zscore/experiments/ACTL6a.csv")
GRHL3 <- read.csv("/home/ckmiyachi/zscore/experiments/GRHL3.csv")
MLL2 <- read.csv("/home/ckmiyachi/zscore/experiments/MLL2.csv")
SNAI2_Over <- read.csv("/home/ckmiyachi/zscore/experiments/SNAI2_Over.csv")
SNAI2_Down <- read.csv("/home/ckmiyachi/zscore/experiments/SNAI2_Down.csv")
DMNT1 <- read.csv("/home/ckmiyachi/zscore/experiments/DMNT1.csv")
CBX4_Over <- read.csv("/home/ckmiyachi/zscore/experiments/CBX4_Over.csv")
CBX4_Down <- read.csv("/home/ckmiyachi/zscore/experiments/CBX4_Down.csv")
MAFB1 <- read.csv("/home/ckmiyachi/zscore/experiments/MAFB1.csv")
MPZL3 <- read.csv("/home/ckmiyachi/zscore/experiments/MPZL3.csv")
FDXR <- read.csv("/home/ckmiyachi/zscore/experiments/FDXR.csv")
TINCR <- read.csv("/home/ckmiyachi/zscore/experiments/TINCR.csv")
CALML5 <- read.csv("/home/ckmiyachi/zscore/experiments/CALML5.csv")
SFN <- read.csv("/home/ckmiyachi/zscore/experiments/SFN.csv")
ATF2 <- read.csv("/home/ckmiyachi/zscore/experiments/ATF2.csv")
ATF3 <- read.csv("/home/ckmiyachi/zscore/experiments/ATF3.csv")
ATF4 <- read.csv("/home/ckmiyachi/zscore/experiments/ATF4.csv")
ATF5 <- read.csv("/home/ckmiyachi/zscore/experiments/ATF5.csv")
ATF7 <- read.csv("/home/ckmiyachi/zscore/experiments/ATF7.csv")
BRIP1 <- read.csv("/home/ckmiyachi/zscore/experiments/BRIP1.csv")
NOTCH3 <- read.csv("/home/ckmiyachi/zscore/experiments/NOTCH3.csv")
CEBPG <- read.csv("/home/ckmiyachi/zscore/experiments/CEBPG.csv")
CREB5 <- read.csv("/home/ckmiyachi/zscore/experiments/CREB5.csv")
E2F1 <- read.csv("/home/ckmiyachi/zscore/experiments/E2F1.csv")
ETS1 <- read.csv("/home/ckmiyachi/zscore/experiments/ETS1.csv")
FOS <- read.csv("/home/ckmiyachi/zscore/experiments/FOS.csv")
FOSB <- read.csv("/home/ckmiyachi/zscore/experiments/FOSB.csv")
FOSL1 <- read.csv("/home/ckmiyachi/zscore/experiments/FOSL1.csv")
FOSL2 <- read.csv("/home/ckmiyachi/zscore/experiments/FOSL2.csv")
FOXD1 <- read.csv("/home/ckmiyachi/zscore/experiments/FOXD1.csv")
GABP1 <- read.csv("/home/ckmiyachi/zscore/experiments/GABP1.csv")
FOXN1 <- read.csv("/home/ckmiyachi/zscore/experiments/FOXN1.csv")
FOXN2 <- read.csv("/home/ckmiyachi/zscore/experiments/FOXN2.csv")
FOXP1 <- read.csv("/home/ckmiyachi/zscore/experiments/FOXP1.csv")
GRHL1 <- read.csv("/home/ckmiyachi/zscore/experiments/GRHL1.csv")
GRHL2 <- read.csv("/home/ckmiyachi/zscore/experiments/GRHL2.csv")
GRHL3a <- read.csv("/home/ckmiyachi/zscore/experiments/GRHL3a.csv")
JUN <- read.csv("/home/ckmiyachi/zscore/experiments/JUN.csv")
JUNB <- read.csv("/home/ckmiyachi/zscore/experiments/JUNB.csv")
JUND <- read.csv("/home/ckmiyachi/zscore/experiments/JUND.csv")
KLF4a <- read.csv("/home/ckmiyachi/zscore/experiments/KLF4a.csv")
LRRFLP1 <- read.csv("/home/ckmiyachi/zscore/experiments/LRRFLP1.csv")
PBX1 <- read.csv("/home/ckmiyachi/zscore/experiments/PBX1.csv")
OVOL1 <- read.csv("/home/ckmiyachi/zscore/experiments/OVOL1.csv")
OVOL2 <- read.csv("/home/ckmiyachi/zscore/experiments/OVOL2.csv")
NR3C1 <- read.csv("/home/ckmiyachi/zscore/experiments/NR3C1.csv")
RUNX1 <- read.csv("/home/ckmiyachi/zscore/experiments/RUNX1.csv")
RORA <- read.csv("/home/ckmiyachi/zscore/experiments/RORA.csv")
RELB <- read.csv("/home/ckmiyachi/zscore/experiments/RELB.csv")
RBL1 <- read.csv("/home/ckmiyachi/zscore/experiments/RBL1.csv")
RARG <- read.csv("/home/ckmiyachi/zscore/experiments/RARG.csv")
PRDM1 <- read.csv("/home/ckmiyachi/zscore/experiments/PRDM1.csv")
PBX2 <- read.csv("/home/ckmiyachi/zscore/experiments/PBX2.csv")
STAT3 <- read.csv("/home/ckmiyachi/zscore/experiments/STAT3.csv")
STAT1 <- read.csv("/home/ckmiyachi/zscore/experiments/STAT1.csv")
SP3 <- read.csv("/home/ckmiyachi/zscore/experiments/SP3.csv")
SP1 <- read.csv("/home/ckmiyachi/zscore/experiments/SP1.csv")
SOX6 <- read.csv("/home/ckmiyachi/zscore/experiments/SOX6.csv")
SOX11 <- read.csv("/home/ckmiyachi/zscore/experiments/SOX11.csv")
SMAD4 <- read.csv("/home/ckmiyachi/zscore/experiments/SMAD4.csv")
RUNX2 <- read.csv("/home/ckmiyachi/zscore/experiments/RUNX2.csv")
XBP1 <- read.csv("/home/ckmiyachi/zscore/experiments/XBP1.csv")
TP63 <- read.csv("/home/ckmiyachi/zscore/experiments/TP63.csv")
TCF7L2 <- read.csv("/home/ckmiyachi/zscore/experiments/TCF7L2.csv")
TCF4 <- read.csv("/home/ckmiyachi/zscore/experiments/TCF4.csv")
STAT6 <- read.csv("/home/ckmiyachi/zscore/experiments/STAT6.csv")

#Colors
col5 <- colorRampPalette(c("yellow","cyan","green", "blue", "purple","red"))
whiteblack <- c("white", "black")

#This is the outer wrapper that all input and output is gotten from 
function(input, output, session) {
  
  #----------------------------------------- Individual Tables ---------------------------------#
  
  output$KLF4_Table <- renderDataTable({
    x = data.frame(working$Gene, working$KLF4)
    colnames(x) <- c("Gene", "KLF4")
    x = x[!(x$KLF4 == "NA"),]
    return(x)
  }, options = list(pageLength=10))

  output$ZNF750_Table <- renderDataTable({
    x = data.frame(working$Gene, working$ZNF750)
    colnames(x) <- c("Gene", "ZNF750")
    x = x[!(x$ZNF750 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$ACTL_Table <- renderDataTable({
    x = data.frame(working$Gene, working$ACTL)
    colnames(x) <- c("Gene", "ACTL")
    x = x[!(x$ACTL == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$GRHL3_Table <- renderDataTable({
    x = data.frame(working$Gene, working$GRHL3)
    colnames(x) <- c("Gene", "GRHL3")
    x = x[!(x$GRHL3 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$MLL2_Table <- renderDataTable({
    x = data.frame(working$Gene, working$MLL2)
    colnames(x) <- c("Gene", "MLL2")
    x = x[!(x$MLL2 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$SNAI2_Over_Table <- renderDataTable({
    x = data.frame(working$Gene, working$SNAI2_Over)
    colnames(x) <- c("Gene", "SNAI2_Over")
    x = x[!(x$SNAI2_Over == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$SNAI2_Knockdown_Table <- renderDataTable({
    x = data.frame(working$Gene, working$SNAI2_Knockdown)
    colnames(x) <- c("Gene", "SNAI2_Knockdown")
    x = x[!(x$SNAI2_Knockdown == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$DMNT1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$DMNT1)
    colnames(x) <- c("Gene", "DMNT1")
    x = x[!(x$DMNT1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$CBX4_Over_Table <- renderDataTable({
    x = data.frame(working$Gene, working$CBX4_Over)
    colnames(x) <- c("Gene", "CBX4_Over")
    x = x[!(x$CBX4_Over == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$CBX4_Knockdown_Table <- renderDataTable({
    x = data.frame(working$Gene, working$CBX4_Knockdown)
    colnames(x) <- c("Gene", "CBX4_Knockdown")
    x = x[!(x$CBX4_Knockdown == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$MAFB1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$MAFB1)
    colnames(x) <- c("Gene", "MAFB1")
    x = x[!(x$MAFB1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$MPZL3_Table <- renderDataTable({
    x = data.frame(working$Gene, working$MPZL3)
    colnames(x) <- c("Gene", "MPZL3")
    x = x[!(x$MPZL3 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$FDXR_Table <- renderDataTable({
    x = data.frame(working$Gene, working$FDXR)
    colnames(x) <- c("Gene", "FDXR")
    x = x[!(x$FDXR == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$TINCR_Table <- renderDataTable({
    x = data.frame(working$Gene, working$TINCR)
    colnames(x) <- c("Gene", "TINCR")
    x = x[!(x$TINCR == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$CALML5_Table <- renderDataTable({
    x = data.frame(working$Gene, working$CALML5)
    colnames(x) <- c("Gene", "CALML5")
    x = x[!(x$CALML5 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$SFN_Table <- renderDataTable({
    x = data.frame(working$Gene, working$SFN)
    colnames(x) <- c("Gene", "SFN")
    x = x[!(x$SFN == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$ATF2_Table <- renderDataTable({
    x = data.frame(working$Gene, working$ATF2)
    colnames(x) <- c("Gene", "ATF2")
    x = x[!(x$ATF2 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$ATF3_Table <- renderDataTable({
    x = data.frame(working$Gene, working$ATF3)
    colnames(x) <- c("Gene", "ATF3")
    x = x[!(x$ATF3 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$ATF4_Table <- renderDataTable({
    x = data.frame(working$Gene, working$ATF4)
    colnames(x) <- c("Gene", "ATF4")
    x = x[!(x$ATF4 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$ATF5_Table <- renderDataTable({
    x = data.frame(working$Gene, working$ATF5)
    colnames(x) <- c("Gene", "ATF5")
    x = x[!(x$ATF5 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$ATF7_Table <- renderDataTable({
    x = data.frame(working$Gene, working$ATF7)
    colnames(x) <- c("Gene", "ATF7")
    x = x[!(x$ATF7 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$BRIP1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$BRIP1)
    colnames(x) <- c("Gene", "BRIP1")
    x = x[!(x$BRIP1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$NOTCH3_Table <- renderDataTable({
    x = data.frame(working$Gene, working$NOTCH3)
    colnames(x) <- c("Gene", "NOTCH3")
    x = x[!(x$NOTCH3 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$DMNT1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$DMNT1)
    colnames(x) <- c("Gene", "DMNT1")
    x = x[!(x$DMNT1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$CEBPG_Table <- renderDataTable({
    x = data.frame(working$Gene, working$CEBPG)
    colnames(x) <- c("Gene", "CEBPG")
    x = x[!(x$CEBPG == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$CREB5_Table <- renderDataTable({
    x = data.frame(working$Gene, working$CREB5)
    colnames(x) <- c("Gene", "CREB5")
    x = x[!(x$CREB5 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$DMNT1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$DMNT1)
    colnames(x) <- c("Gene", "DMNT1")
    x = x[!(x$DMNT1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$E2F1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$E2F1)
    colnames(x) <- c("Gene", "E2F1")
    x = x[!(x$E2F1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$ETS1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$ETS1)
    colnames(x) <- c("Gene", "ETS1")
    x = x[!(x$ETS1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$FOS_Table <- renderDataTable({
    x = data.frame(working$Gene, working$FOS)
    colnames(x) <- c("Gene", "FOS")
    x = x[!(x$FOS == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$FOSB_Table <- renderDataTable({
    x = data.frame(working$Gene, working$FOSB)
    colnames(x) <- c("Gene", "FOSB")
    x = x[!(x$FOSB == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$FOSL1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$FOSL1)
    colnames(x) <- c("Gene", "FOSL1")
    x = x[!(x$FOSL1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$FOSL2_Table <- renderDataTable({
    x = data.frame(working$Gene, working$FOSL2)
    colnames(x) <- c("Gene", "FOSL2")
    x = x[!(x$FOSL2 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$FOXD1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$FOXD1)
    colnames(x) <- c("Gene", "FOXD1")
    x = x[!(x$FOXD1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$GABP1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$GABP1)
    colnames(x) <- c("Gene", "GABP1")
    x = x[!(x$GABP1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$FOXN1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$FOXN1)
    colnames(x) <- c("Gene", "FOXN1")
    x = x[!(x$FOXN1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$FOXN2_Table <- renderDataTable({
    x = data.frame(working$Gene, working$FOXN2)
    colnames(x) <- c("Gene", "FOXN2")
    x = x[!(x$FOXN2 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$FOXP1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$FOXP1)
    colnames(x) <- c("Gene", "FOXP1")
    x = x[!(x$FOXP1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$GRHL1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$GRHL1)
    colnames(x) <- c("Gene", "GRHL1")
    x = x[!(x$GRHL1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$GRHL2_Table <- renderDataTable({
    x = data.frame(working$Gene, working$GRHL2)
    colnames(x) <- c("Gene", "GRHL2")
    x = x[!(x$GRHL2 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$GRHL3a_Table <- renderDataTable({
    x = data.frame(working$Gene, working$GRHL3a)
    colnames(x) <- c("Gene", "GRHL3a")
    x = x[!(x$GRHL3a == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$JUNB_Table <- renderDataTable({
    x = data.frame(working$Gene, working$JUNB)
    colnames(x) <- c("Gene", "JUNB")
    x = x[!(x$JUNB == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$JUND_Table <- renderDataTable({
    x = data.frame(working$Gene, working$JUND)
    colnames(x) <- c("Gene", "JUND")
    x = x[!(x$JUND == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$KLF4a_Table <- renderDataTable({
    x = data.frame(working$Gene, working$KLF4a)
    colnames(x) <- c("Gene", "KLF4a")
    x = x[!(x$KLF4a == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$LRRFLP1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$LRRFLP1)
    colnames(x) <- c("Gene", "LRRFLP1")
    x = x[!(x$LRRFLP1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$PBX1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$PBX1)
    colnames(x) <- c("Gene", "PBX1")
    x = x[!(x$PBX1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$OVOL1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$OVOL1)
    colnames(x) <- c("Gene", "OVOL1")
    x = x[!(x$OVOL1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$OVOL2_Table <- renderDataTable({
    x = data.frame(working$Gene, working$OVOL2)
    colnames(x) <- c("Gene", "OVOL2")
    x = x[!(x$OVOL2 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$NR3C1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$NR3C1)
    colnames(x) <- c("Gene", "NR3C1")
    x = x[!(x$NR3C1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$RUNX1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$RUNX1)
    colnames(x) <- c("Gene", "RUNX1")
    x = x[!(x$RUNX1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$RORA_Table <- renderDataTable({
    x = data.frame(working$Gene, working$RORA)
    colnames(x) <- c("Gene", "RORA")
    x = x[!(x$RORA == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$RELB_Table <- renderDataTable({
    x = data.frame(working$Gene, working$RELB)
    colnames(x) <- c("Gene", "RELB")
    x = x[!(x$RELB == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$RBL1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$RBL1)
    colnames(x) <- c("Gene", "RBL1")
    x = x[!(x$RBL1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$RARG_Table <- renderDataTable({
    x = data.frame(working$Gene, working$RARG)
    colnames(x) <- c("Gene", "RARG")
    x = x[!(x$RARG == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$PRDM1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$PRDM1)
    colnames(x) <- c("Gene", "PRDM1")
    x = x[!(x$PRDM1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$PBX2_Table <- renderDataTable({
    x = data.frame(working$Gene, working$PBX2)
    colnames(x) <- c("Gene", "PBX2")
    x = x[!(x$PBX2 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$STAT3_Table <- renderDataTable({
    x = data.frame(working$Gene, working$STAT3)
    colnames(x) <- c("Gene", "STAT3")
    x = x[!(x$STAT3 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$STAT1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$STAT1)
    colnames(x) <- c("Gene", "STAT1")
    x = x[!(x$STAT1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$SP3_Table <- renderDataTable({
    x = data.frame(working$Gene, working$SP3)
    colnames(x) <- c("Gene", "SP3")
    x = x[!(x$SP3 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$SP1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$SP1)
    colnames(x) <- c("Gene", "SP1")
    x = x[!(x$SP1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$SOX6_Table <- renderDataTable({
    x = data.frame(working$Gene, working$SOX6)
    colnames(x) <- c("Gene", "SOX6")
    x = x[!(x$SOX6 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$SOX11_Table <- renderDataTable({
    x = data.frame(working$Gene, working$SOX11)
    colnames(x) <- c("Gene", "SOX11")
    x = x[!(x$SOX11 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$SMAD4_Table <- renderDataTable({
    x = data.frame(working$Gene, working$SMAD4)
    colnames(x) <- c("Gene", "SMAD4")
    x = x[!(x$SMAD4 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$RUNX2_Table <- renderDataTable({
    x = data.frame(working$Gene, working$RUNX2)
    colnames(x) <- c("Gene", "RUNX2")
    x = x[!(x$RUNX2 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$XBP1_Table <- renderDataTable({
    x = data.frame(working$Gene, working$XBP1)
    colnames(x) <- c("Gene", "XBP1")
    x = x[!(x$XBP1 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$TP63_Table <- renderDataTable({
    x = data.frame(working$Gene, working$TP63)
    colnames(x) <- c("Gene", "TP63")
    x = x[!(x$TP63 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$TCF7L2_Table <- renderDataTable({
    x = data.frame(working$Gene, working$TCF7L2)
    colnames(x) <- c("Gene", "TCF7L2")
    x = x[!(x$TCF7L2 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$TCF4_Table <- renderDataTable({
    x = data.frame(working$Gene, working$TCF4)
    colnames(x) <- c("Gene", "TCF4")
    x = x[!(x$TCF4 == "NA"),]
    return(x)
  }, options = list(pageLength=10))
  
  output$STAT6_Table <- renderDataTable({
    x = data.frame(working$Gene, working$STAT6)
    colnames(x) <- c("Gene", "STAT6")
    x = x[!(x$STAT6 == "NA\r"),]
    return(x)
  }, options = list(pageLength=10))
  
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
  
  save_data_flatfile <- function(data) {
    #data <- t(data)
    results_dir = "/home/ckmiyachi/newdata"
    file_name <- paste0(paste(get_time_human(), digest(data, 
                                                      algo = "md5"), sep = "-"), ".csv")
    file_name <- "Confusion.csv"
    write.csv(x = data, file = file.path(results_dir, file_name), 
              row.names = FALSE, quote = TRUE)
  }
  
  observeEvent(input$dbAdd, {
    inFile <- input$inputFile
    if (input$fLoad == 0)
      return(NULL)
    if (is.null(inFile))
      return(NULL)
    x = read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
    saveData(x)
    save_data_flatfile(x)
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
  
  output$downloadKLF4 <- downloadHandler(
    filename = function() {
      paste("KLF4", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(klf4, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadZNF750 <- downloadHandler(
    filename = function() {
      paste("ZNF750", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(znf750, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadACTL6a <- downloadHandler(
    filename = function() {
      paste("ACTL6a", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ACTL6a, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadGRHL3 <- downloadHandler(
    filename = function() {
      paste("GRHL3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(GRHL3, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadMLL2 <- downloadHandler(
    filename = function() {
      paste("MLL2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(MLL2, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSNAI2_Over <- downloadHandler(
    filename = function() {
      paste("SNAI2_Over", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SNAI2_Over, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSNAI2_Down <- downloadHandler(
    filename = function() {
      paste("SNAI2_Down", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SNAI2_Down, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadDMNT1 <- downloadHandler(
    filename = function() {
      paste("DMNT1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(DMNT1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadCBX4_Over <- downloadHandler(
    filename = function() {
      paste("CBX4_Over", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(CBX4_Over, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadCBX4_Down <- downloadHandler(
    filename = function() {
      paste("CBX4_Down", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(CBX4_Down, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadMAFB1 <- downloadHandler(
    filename = function() {
      paste("MAFB1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(MAFB1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadMPZL3 <- downloadHandler(
    filename = function() {
      paste("MPZL3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(MPZL3, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadFDXR <- downloadHandler(
    filename = function() {
      paste("FDXR", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FDXR, file, row.names = F)
    },
    contentType = 'csv'
  )
  
  output$downloadTINCR <- downloadHandler(
    filename = function() {
      paste("TINCR", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(TINCR, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadCALML5 <- downloadHandler(
    filename = function() {
      paste("CALML5", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(CALML5, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSFN <- downloadHandler(
    filename = function() {
      paste("SFN", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SFN, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadATF2 <- downloadHandler(
    filename = function() {
      paste("ATF2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ATF2, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadATF3 <- downloadHandler(
    filename = function() {
      paste("ATF3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ATF3, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadATF4 <- downloadHandler(
    filename = function() {
      paste("ATF4", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ATF4, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadATF5 <- downloadHandler(
    filename = function() {
      paste("ATF5", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ATF5, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadATF7 <- downloadHandler(
    filename = function() {
      paste("ATF7", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ATF7, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadBRIP1 <- downloadHandler(
    filename = function() {
      paste("BRIP1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(BRIP1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadNOTCH3 <- downloadHandler(
    filename = function() {
      paste("NOTCH3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(NOTCH3, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadCEBPG <- downloadHandler(
    filename = function() {
      paste("CEBPG", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(CEBPG, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadCREB5 <- downloadHandler(
    filename = function() {
      paste("CREB5", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(CREB5, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadE2F1 <- downloadHandler(
    filename = function() {
      paste("E2F1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(E2F1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadETS1 <- downloadHandler(
    filename = function() {
      paste("ETS1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ETS1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadFOS <- downloadHandler(
    filename = function() {
      paste("FOS", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FOS, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadFOSB <- downloadHandler(
    filename = function() {
      paste("FOSB", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FOSB, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadFOSL1 <- downloadHandler(
    filename = function() {
      paste("FOSL1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FOSL1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadFOSL2 <- downloadHandler(
    filename = function() {
      paste("FOSL2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FOSL2, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadFOXD1 <- downloadHandler(
    filename = function() {
      paste("FOXD1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FOXD1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadGABP1 <- downloadHandler(
    filename = function() {
      paste("GABP1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(GABP1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadFOXN1 <- downloadHandler(
    filename = function() {
      paste("FOXN1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FOXN1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadFOXN2 <- downloadHandler(
    filename = function() {
      paste("FOXN2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FOXN2, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadFOXP1 <- downloadHandler(
    filename = function() {
      paste("FOXP1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(FOXP1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadGRHL1 <- downloadHandler(
    filename = function() {
      paste("GRHL1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(GRHL1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadGRHL2 <- downloadHandler(
    filename = function() {
      paste("GRHL2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(GRHL2, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadGRHL3a <- downloadHandler(
    filename = function() {
      paste("GRHL3a", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(GRHL3a, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadJUN <- downloadHandler(
    filename = function() {
      paste("JUN", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(JUN, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadJUNB <- downloadHandler(
    filename = function() {
      paste("JUNB", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(JUNB, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadJUND <- downloadHandler(
    filename = function() {
      paste("JUND", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(JUND, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadKLF4a <- downloadHandler(
    filename = function() {
      paste("KLF4a", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(KLF4a, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadLRRFLP1 <- downloadHandler(
    filename = function() {
      paste("LRRFLP1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(LRRFLP1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadPBX1 <- downloadHandler(
    filename = function() {
      paste("PBX1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(PBX1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadOVOL1 <- downloadHandler(
    filename = function() {
      paste("OVOL1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(OVOL1, file, row.names = F)
    },
    contentType = 'csv'
  )
  
  output$downloadOVOL2 <- downloadHandler(
    filename = function() {
      paste("OVOL2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(OVOL2, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadNR3C1 <- downloadHandler(
    filename = function() {
      paste("NR3C1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(NR3C1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadRUNX1 <- downloadHandler(
    filename = function() {
      paste("RUNX1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RUNX1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadRORA <- downloadHandler(
    filename = function() {
      paste("RORA", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RORA, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadRELB <- downloadHandler(
    filename = function() {
      paste("RELB", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RELB, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadRBL1 <- downloadHandler(
    filename = function() {
      paste("RBL1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RBL1, file, row.names = F)
    },
    contentType = 'csv'
  )
  
  output$downloadRARG <- downloadHandler(
    filename = function() {
      paste("RARG", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RARG, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadPRDM1 <- downloadHandler(
    filename = function() {
      paste("PRDM1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(PRDM1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadPBX2 <- downloadHandler(
    filename = function() {
      paste("PBX2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(PBX2, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSTAT3 <- downloadHandler(
    filename = function() {
      paste("STAT3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(STAT3, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSTAT1 <- downloadHandler(
    filename = function() {
      paste("STAT1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(STAT1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSP3 <- downloadHandler(
    filename = function() {
      paste("SP3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SP3, file, row.names = F)
    },
    contentType = 'csv'
  )
  
  output$downloadSP1 <- downloadHandler(
    filename = function() {
      paste("SP1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SP1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSOX6 <- downloadHandler(
    filename = function() {
      paste("SOX6", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SOX6, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSOX11 <- downloadHandler(
    filename = function() {
      paste("SOX11", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SOX11, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSMAD4 <- downloadHandler(
    filename = function() {
      paste("SMAD4", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(SMAD4, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadRUNX2 <- downloadHandler(
    filename = function() {
      paste("RUNX2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RUNX2, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadXBP1 <- downloadHandler(
    filename = function() {
      paste("XBP1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(XBP1, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadTP63 <- downloadHandler(
    filename = function() {
      paste("TP63", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(TP63, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadTCF7L2 <- downloadHandler(
    filename = function() {
      paste("TCF7L2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(TCF7L2, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadTCF4 <- downloadHandler(
    filename = function() {
      paste("TCF4", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(TCF4, file, row.names = F)
    },
    contentType = 'csv'
  )
  output$downloadSTAT6 <- downloadHandler(
    filename = function() {
      paste("STAT6", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(STAT6, file, row.names = F)
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
      colnames(new_data) <- c("Gene", "Tria")
      merged <- merge(working,new_data, by = "Gene", all=T)
      #colnames(merged)[1] <- "Gene"
      genes <- data.frame(merged$Gene)
      #merged <- merged[, sapply(merged[,2:ncol(merged)], as.numeric)]
      cnumeric = colnames(merged[2:ncol(merged)])
      merged <- suppressWarnings(data.frame(sapply(merged, as.numeric)))
      merged$Gene <- genes$merged.Gene
      #colnames(merged)[1] <- "Gene"
      merged <- aggregate(merged[, 2:ncol(merged)], list(merged$Gene), FUN = mean, na.rm = TRUE, na.action = na.pass)
      clean_db <- merged[, sapply(merged, is.numeric)]
      #id = rownames(clean_db)
      #clean_db$X <- NULL
      clean_db = data.frame(apply(clean_db,2,f))
      clean_db = data.frame(scale(clean_db))
      Pearsons <- cor(clean_db, use="complete.obs", method="spearman")
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
    genes = row.names(x)
    #temp2 <- link[intersect(genes, colnames(link))]
    #elinks <- subset(link,select = genes )
    #idk <- t(elinks)
    #colnames(idk)[1] <- "Link"
    #idk <- data.frame(idk)
    genelink <- createLink(genes)
    y = data.frame(genelink,x$Pearson)
    colnames(y) <- c('Genes','Spearman Correlation')
    return(y)
  },options = list( 
    scrollY = '300px', paging = FALSE, scrollX = TRUE
  ), escape = FALSE)
  
  #Top 10 Rankings for Pearson Correlation Coefficient of Input Experiment
  output$barPearson <- renderPlot({ 
    x = data.frame(head(Pearson_cor(), n = 10))
    genes <- row.names(x)
    #exp_filter <- experiment_link[experiment_link$Experiment == genes,]
    genes <- row.names(x)
    x$Correlation_Ranking = x$Pearson
    #y = data.frame(x$Gene,x$Correlation_Ranking)
    y = data.frame(genes,x$Pearson)
    colnames(y) = c("Gene", "P")
    n = y$Gene
    n = rev(n)
    rank <- y$P
    rank <- rank[order(rank, decreasing = F)]
    #rank <- sort(abs(rank), decreasing = F)
    par(mar=c(4,10,4,2))
    return(barplot(rank, main = "Spearman Correlation Bar Plot", xlab = "Spearman Correlation Coefficient",
                   horiz = TRUE, las=2, names.arg=n, col="#0dc5c1",xlim = c(0,1)))
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
      #colnames(merged)[1] <- "Gene"
      genes <- data.frame(merged$Gene)
      #merged <- merged[, sapply(merged[,2:ncol(merged)], as.numeric)]
      cnumeric = colnames(merged[2:ncol(merged)])
      merged <- suppressWarnings(data.frame(sapply(merged, as.numeric)))
      merged$Gene <- genes$merged.Gene
      #colnames(merged)[1] <- "Gene"
      merged <- aggregate(merged[, 2:ncol(merged)], list(merged$Gene), FUN = mean, na.rm = TRUE, na.action = na.pass)
      clean_db <- merged[, sapply(merged, is.numeric)]
      #id = rownames(clean_db)
      #clean_db$X <- NULL
      clean_db = data.frame(apply(clean_db,2,f))
      clean_db = data.frame(scale(clean_db))
      Pearsons <- cor(clean_db, use="complete.obs", method="spearman")
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
  
  output$txtbarPearson <- renderPlot({ 
    x = data.frame(head(txthope(), n = 10))
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
    return(barplot(rank, main = "Spearman Correlation Bar Plot", xlab = "Spearman Correlation Coefficient",
                   horiz = TRUE, las=2, names.arg=n, col="#0dc5c1", xlim = c(0,1)))
  })
  
  #--------------------------------------- Renders Correlation Visualizations ----------------------------#  
  
  #This is the pearson correlation coefficient logic which I think works as of now
  #This might be unneccesary. We will see! 

  
  #This is the pearson correlation coefficient logic which I think works as of now
  pearsonVis <- reactive ({
    if(input$Corr == 0){return()}
    inFile <- input$inputFile
    #if(input$caption == ""){return()}
    isolate({ 
      con <- textConnection(input$caption)
      x <- data.frame(paste(input$caption), stringsAsFactors = F )
      y <- read.csv(con, header = input$header,sep = input$sep, quote = input$quote,stringsAsFactors =FALSE)
      colnames(y) <- c("Gene", "Trial")
      merged <- merge(working,y, by = "Gene", all=T)
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
      new <- head(new, n = 10)
      gene_select <- row.names(new)
      pleasework <- clean_db[,gene_select]
      newPearsons <- cor(pleasework, use="pairwise.complete.obs", method="spearman")
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

  
  #Publication Data Information (This might be unneccesary as well)
  output$data_info = renderDataTable({ data.frame(pub_info) }, options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE
  ))
  
  
  #---------------------------------------- Trial Stuff ------------------------------------#
  
  output$table1 <- renderDataTable({
    
    my_table <- pub_info
    #my_table$ID <- NULL
    #colnames(my_table)[1] <- 'Gene'
    #my_table$link <- createLink(my_table$DOI)
    return(my_table)
    
  },  options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE, pageLength = 25
  ), escape = FALSE)
  
  
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
      Pearsons <- cor(clean_db, use="pairwise.complete.obs", method ="spearman")
      
      x <- data.frame(Pearsons)
      new <- x[ncol(x),]
      new <- new[-length(new)]
      new <- new[order(new, decreasing = T)]
      new <- t(new)
      new <- head(new, n = 10)
      gene_select <- row.names(new)
      pleasework <- clean_db[,gene_select]
      newPearsons <- cor(pleasework, use="pairwise.complete.obs", method="spearman")
    })
    newPearsons
    
  })
  #Renders the actual plot of the Input File
  output$txtpearson = reactive({
    renderPlot({corrplot.mixed(pearsonVis(), addrect=2, tl.pos = "lt", diag = "u",lower.col = "black", number.cex = .7, upper.col = col5(20))})
  })
  
  if(is.null(pearsonVis)) {output$txtvis = NULL}
  else {
    output$txtvis = renderPlot({corrplot.mixed(pearsonVis(), addrect=2, tl.pos = "lt", diag = "u", lower.col = "black", number.cex = .7, upper.col = col5(20))})
  }
  
  
  output$text1 <- renderText({ 
    paste("Gene, BRCA1
           Gene1, 0.234
           Gene2, 0.787", input$caption)
  })
  
  output$easyWorking <- renderDataTable({
    
    #my_table <- experiment_link
    my_table = data.frame(experiment_link$Experiment, experiment_link$New_Link, experiment_link$New_Link)
    colnames(my_table) = c("Experimental_Gene", "Raw_Data", "Standardized_Data")
    my_table$Raw_Data <- createLink(my_table$Raw_Data, my_table$Experimental_Gene)
    my_table$Standardized_Data <- createModal(my_table$Experimental_Gene)
    #my_table$ID <- NULL
    #colnames(my_table)[1] <- 'Gene'
    #my_table$link <- createLink(my_table$DOI)
    return(my_table)
    
  },  options = list( 
    scrollY = '350px', paging = TRUE, scrollX = TRUE, pageLength = 70), 
  escape = FALSE)
  
}




setwd("/home/ckmiyachi/zscore/experiments/")
working <- read.csv("working.csv", stringsAsFactors = F)

x = data.frame(working$Gene, working$KLF4)
colnames(x) <- c("Gene", "KLF4")
x <- x[complete.cases(x),]
write.csv(x, "KLF4.csv")


x = data.frame(working$Gene, working$ZNF750)
colnames(x) <- c("Gene", "ZNF750")
x <- x[complete.cases(x),]
write.csv(x, "ZNF750.csv", row.names = F)

x = data.frame(working$Gene, working$ACTL)
colnames(x) <- c("Gene", "ACTL6a")
x <- x[complete.cases(x),]
write.csv(x, "ACTL6a.csv", row.names = F)


x = data.frame(working$Gene, working$GRHL3)
colnames(x) <- c("Gene", "GRHL3")
x <- x[complete.cases(x),]
write.csv(x, "GRHL3.csv", row.names = F)

x = data.frame(working$Gene, working$MLL2)
colnames(x) <- c("Gene", "MLL2")
x <- x[complete.cases(x),]
write.csv(x, "MLL2.csv", row.names = F)

x = data.frame(working$Gene, working$SNAI2_Over)
colnames(x) <- c("Gene", "SNAI2_Over")
x <- x[complete.cases(x),]
write.csv(x, "SNAI2_Over.csv", row.names = F)

x = data.frame(working$Gene, working$SNAI2_Knockdown)
colnames(x) <- c("Gene", "SNAI2_Down")
x <- x[complete.cases(x),]
write.csv(x, "SNAI2_Down.csv", row.names = F)

x = data.frame(working$Gene, working$DMNT1)
colnames(x) <- c("Gene", "DMNT1")
x <- x[complete.cases(x),]
write.csv(x, "DMNT1.csv", row.names = F)

x = data.frame(working$Gene, working$CBX4_Over)
colnames(x) <- c("Gene", "CBX4_Over")
x <- x[complete.cases(x),]
write.csv(x, "CBX4_Over.csv", row.names = F)

x = data.frame(working$Gene, working$CBX4_Knockdown)
colnames(x) <- c("Gene", "CBX4_Down")
x <- x[complete.cases(x),]
write.csv(x, "CBX4_Down.csv", row.names = F)

x = data.frame(working$Gene, working$MAFB1)
colnames(x) <- c("Gene", "MAFB1")
x <- x[complete.cases(x),]
write.csv(x, "MAFB1.csv", row.names = F)

x = data.frame(working$Gene, working$MPZL3)
colnames(x) <- c("Gene", "MPZL3")
x <- x[complete.cases(x),]
write.csv(x, "MPZL3.csv", row.names = F)

x = data.frame(working$Gene, working$FDXR)
colnames(x) <- c("Gene", "FDXR")
x <- x[complete.cases(x),]
write.csv(x, "FDXR.csv", row.names = F)

x = data.frame(working$Gene, working$TINCR)
colnames(x) <- c("Gene", "TINCR")
x <- x[complete.cases(x),]
write.csv(x, "TINCR.csv", row.names = F)

x = data.frame(working$Gene, working$CALML5)
colnames(x) <- c("Gene", "CALML5")
x <- x[complete.cases(x),]
write.csv(x, "CALML5.csv", row.names = F)

x = data.frame(working$Gene, working$SFN)
colnames(x) <- c("Gene", "SFN")
x <- x[complete.cases(x),]
write.csv(x, "SFN.csv", row.names = F)

x = data.frame(working$Gene, working$ATF2)
colnames(x) <- c("Gene", "ATF2")
x <- x[complete.cases(x),]
write.csv(x, "ATF2.csv", row.names = F)

x = data.frame(working$Gene, working$ATF3)
colnames(x) <- c("Gene", "ATF3")
x <- x[complete.cases(x),]
write.csv(x, "ATF3.csv", row.names = F)

x = data.frame(working$Gene, working$ATF4)
colnames(x) <- c("Gene", "ATF4")
x <- x[complete.cases(x),]
write.csv(x, "ATF4.csv", row.names = F)

x = data.frame(working$Gene, working$ATF5)
colnames(x) <- c("Gene", "ATF5")
x <- x[complete.cases(x),]
write.csv(x, "ATF5.csv", row.names = F)

x = data.frame(working$Gene, working$ATF7)
colnames(x) <- c("Gene", "ATF7")
x <- x[complete.cases(x),]
write.csv(x, "ATF7.csv", row.names = F)

x = data.frame(working$Gene, working$BRIP1)
colnames(x) <- c("Gene", "BRIP1")
x <- x[complete.cases(x),]
write.csv(x, "BRIP1.csv", row.names = F)

x = data.frame(working$Gene, working$NOTCH3)
colnames(x) <- c("Gene", "NOTCH3")
x <- x[complete.cases(x),]
write.csv(x, "NOTCH3.csv", row.names = F)

x = data.frame(working$Gene, working$CEBPG)
colnames(x) <- c("Gene", "CEBPG")
x <- x[complete.cases(x),]
write.csv(x, "CEBPG.csv", row.names = F)

x = data.frame(working$Gene, working$CREB5)
colnames(x) <- c("Gene", "CREB5")
x <- x[complete.cases(x),]
write.csv(x, "CREB5.csv", row.names = F)

x = data.frame(working$Gene, working$E2F1)
colnames(x) <- c("Gene", "E2F1")
x <- x[complete.cases(x),]
write.csv(x, "E2F1.csv", row.names = F)

x = data.frame(working$Gene, working$ETS1)
colnames(x) <- c("Gene", "ETS1")
x <- x[complete.cases(x),]
write.csv(x, "ETS1.csv", row.names = F)

x = data.frame(working$Gene, working$FOS)
colnames(x) <- c("Gene", "FOS")
x <- x[complete.cases(x),]
write.csv(x, "FOS.csv", row.names = F)

x = data.frame(working$Gene, working$FOSB)
colnames(x) <- c("Gene", "FOSB")
x <- x[complete.cases(x),]
write.csv(x, "FOSB.csv", row.names = F)

x = data.frame(working$Gene, working$FOSL1)
colnames(x) <- c("Gene", "FOSL1")
x <- x[complete.cases(x),]
write.csv(x, "FOSL1.csv", row.names = F)

x = data.frame(working$Gene, working$FOSL2)
colnames(x) <- c("Gene", "FOSL2")
x <- x[complete.cases(x),]
write.csv(x, "FOSL2.csv", row.names = F)

x = data.frame(working$Gene, working$FOXD1)
colnames(x) <- c("Gene", "FOXD1")
x <- x[complete.cases(x),]
write.csv(x, "FOXD1.csv", row.names = F)

x = data.frame(working$Gene, working$GABP1)
colnames(x) <- c("Gene", "GABP1")
x <- x[complete.cases(x),]
write.csv(x, "GABP1.csv", row.names = F)

x = data.frame(working$Gene, working$FOXN1)
colnames(x) <- c("Gene", "FOXN1")
x <- x[complete.cases(x),]
write.csv(x, "FOXN1.csv", row.names = F)

x = data.frame(working$Gene, working$FOXN2)
colnames(x) <- c("Gene", "FOXN2")
x <- x[complete.cases(x),]
write.csv(x, "FOXN2.csv", row.names = F)

x = data.frame(working$Gene, working$FOXP1)
colnames(x) <- c("Gene", "FOXP1")
x <- x[complete.cases(x),]
write.csv(x, "FOXP1.csv", row.names = F)

x = data.frame(working$Gene, working$GRHL1)
colnames(x) <- c("Gene", "GRHL1")
x <- x[complete.cases(x),]
write.csv(x, "GRHL1.csv", row.names = F)

x = data.frame(working$Gene, working$GRHL2)
colnames(x) <- c("Gene", "GRHL2")
x <- x[complete.cases(x),]
write.csv(x, "GRHL2.csv", row.names = F)

x = data.frame(working$Gene, working$GRHL3a)
colnames(x) <- c("Gene", "GRHL3a")
x <- x[complete.cases(x),]
write.csv(x, "GRHL3a.csv", row.names = F)

x = data.frame(working$Gene, working$JUN)
colnames(x) <- c("Gene", "JUN")
x <- x[complete.cases(x),]
write.csv(x, "JUN.csv", row.names = F)

x = data.frame(working$Gene, working$JUNB)
colnames(x) <- c("Gene", "JUNB")
x <- x[complete.cases(x),]
write.csv(x, "JUNB.csv", row.names = F)

x = data.frame(working$Gene, working$JUND)
colnames(x) <- c("Gene", "JUND")
x <- x[complete.cases(x),]
write.csv(x, "JUND.csv", row.names = F)

x = data.frame(working$Gene, working$KLF4a)
colnames(x) <- c("Gene", "KLF4a")
x <- x[complete.cases(x),]
write.csv(x, "KLF4a.csv", row.names = F)


x = data.frame(working$Gene, working$LRRFLP1)
colnames(x) <- c("Gene", "LRRFLP1")
x <- x[complete.cases(x),]
write.csv(x, "LRRFLP1.csv", row.names = F)

x = data.frame(working$Gene, working$PBX1)
colnames(x) <- c("Gene", "PBX1")
x <- x[complete.cases(x),]
write.csv(x, "PBX1.csv", row.names = F)

x = data.frame(working$Gene, working$OVOL1)
colnames(x) <- c("Gene", "OVOL1")
x <- x[complete.cases(x),]
write.csv(x, "OVOL1.csv", row.names = F)

x = data.frame(working$Gene, working$OVOL2)
colnames(x) <- c("Gene", "OVOL2")
x <- x[complete.cases(x),]
write.csv(x, "OVOL2.csv", row.names = F)

x = data.frame(working$Gene, working$NR3C1)
colnames(x) <- c("Gene", "NR3C1")
x <- x[complete.cases(x),]
write.csv(x, "NR3C1.csv", row.names = F)

x = data.frame(working$Gene, working$RUNX1)
colnames(x) <- c("Gene", "RUNX1")
x <- x[complete.cases(x),]
write.csv(x, "RUNX1.csv", row.names = F)

x = data.frame(working$Gene, working$RORA)
colnames(x) <- c("Gene", "RORA")
x <- x[complete.cases(x),]
write.csv(x, "RORA.csv", row.names = F)

x = data.frame(working$Gene, working$RELB)
colnames(x) <- c("Gene", "RELB")
x <- x[complete.cases(x),]
write.csv(x, "RELB.csv", row.names = F)

x = data.frame(working$Gene, working$RBL1)
colnames(x) <- c("Gene", "RBL1")
x <- x[complete.cases(x),]
write.csv(x, "RBL1.csv", row.names = F)

x = data.frame(working$Gene, working$RARG)
colnames(x) <- c("Gene", "RARG")
x <- x[complete.cases(x),]
write.csv(x, "RARG.csv", row.names = F)

x = data.frame(working$Gene, working$PRDM1)
colnames(x) <- c("Gene", "PRDM1")
x <- x[complete.cases(x),]
write.csv(x, "PRDM1.csv", row.names = F)

x = data.frame(working$Gene, working$PBX2)
colnames(x) <- c("Gene", "PBX2")
x <- x[complete.cases(x),]
write.csv(x, "PBX2.csv", row.names = F)

x = data.frame(working$Gene, working$SOX6)
colnames(x) <- c("Gene", "SOX6")
x <- x[complete.cases(x),]
write.csv(x, "SOX6.csv", row.names = F)

x = data.frame(working$Gene, working$SOX11)
colnames(x) <- c("Gene", "SOX11")
x <- x[complete.cases(x),]
write.csv(x, "SOX11.csv", row.names = F)

x = data.frame(working$Gene, working$SMAD4)
colnames(x) <- c("Gene", "SMAD4")
x <- x[complete.cases(x),]
write.csv(x, "SMAD4.csv", row.names = F)

x = data.frame(working$Gene, working$RUNX2)
colnames(x) <- c("Gene", "RUNX2")
x <- x[complete.cases(x),]
write.csv(x, "RUNX2.csv", row.names = F)

x = data.frame(working$Gene, working$XBP1)
colnames(x) <- c("Gene", "XBP1")
x <- x[complete.cases(x),]
write.csv(x, "XBP1.csv", row.names = F)

x = data.frame(working$Gene, working$TP63)
colnames(x) <- c("Gene", "TP63")
x <- x[complete.cases(x),]
write.csv(x, "TP63.csv", row.names = F)

x = data.frame(working$Gene, working$TCF7L2)
colnames(x) <- c("Gene", "TCF7L2")
x <- x[complete.cases(x),]
write.csv(x, "TCF7L2.csv", row.names = F)

x = data.frame(working$Gene, working$TCF4)
colnames(x) <- c("Gene", "TCF4")
x <- x[complete.cases(x),]
write.csv(x, "TCF4.csv", row.names = F)

x = data.frame(working$Gene, working$STAT6)
colnames(x) <- c("Gene", "STAT6")
x <- x[complete.cases(x),]
write.csv(x, "STAT6.csv", row.names = F)

x = data.frame(working$Gene, working$STAT1)
colnames(x) <- c("Gene", "STAT1")
x <- x[complete.cases(x),]
write.csv(x, "STAT1.csv", row.names = F)

x = data.frame(working$Gene, working$SP3)
colnames(x) <- c("Gene", "SP3")
x <- x[complete.cases(x),]
write.csv(x, "SP3.csv", row.names = F)

x = data.frame(working$Gene, working$STAT3)
colnames(x) <- c("Gene", "STAT3")
x <- x[complete.cases(x),]
write.csv(x, "STAT3.csv", row.names = F)

x = data.frame(working$Gene, working$SP1)
colnames(x) <- c("Gene", "SP1")
x <- x[complete.cases(x),]
write.csv(x, "SP1.csv", row.names = F)

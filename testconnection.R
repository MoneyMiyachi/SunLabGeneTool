setwd("~/Research/Database/SunLabGeneTool")
getwd()


full <- read.csv('newFull.csv')


library(RODBC)
library(RMySQL)


dbhandle <- odbcDriverConnect('driver={SQL Server};server=127.0.0.1;database=HGKP_Db;trusted_connection=true')
res <- sqlQuery(dbhandle, 'select * from iris')



channel <- odbcConnect("test", uid="root", pwd="tomohiro1")

drv = dbDriver("MySQL")

con = dbConnect(drv,host="localhost",dbname="HGKP_Db",user="root",pass="tomohiro1")

album = dbGetQuery(con,statement="select * from iris")

print(album)




x <- colnames(album)


link_dict<- setNames(as.list(names$link), names$names)


killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}


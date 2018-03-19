library(dplyr)
library(data.table)

#Load the EPA data, quite large file so is seperated from others and
#read in with fread

EpaNonRoad <- fread("data/EpaNonRoad08.csv")

#Name the folders and create lists where the data is located

folder1 <- "data/1 Year Update/"
folderS <- "data/Static/"
folderU <- "data/Unknown Update/"

File1Year <- list.files(path = folder1, pattern = "*.csv")
FileS <- list.files(path = folderS, pattern = "*.csv")
FileU <- list.files(path = folderU, pattern = "*.csv")

names1 <- substr(File1Year,1,nchar(File1Year)-4)
namesS <- substr(FileS,1,nchar(FileS)-4)
namesU <- substr(FileU,1,nchar(FileU)-4)

#Load the data frames into R 

for (i in 1:length(File1Year)){
  assign(names1[i], 
  read.csv(paste(folder1, File1Year[i], sep=''))
)}

for (i in 1:length(FileS)){
  assign(namesS[i], 
         read.csv(paste(folderS, FileS[i], sep=''))
)}

for (i in 1:length(FileU)){
  assign(namesU[i], 
  read.csv(paste(folderU, FileU[i], sep=''))
)}

AgCensus <- read.csv("data/5 Year Update/AgCensus.csv")

Vius <- Vius2002

Ffr43 <- `FfrTable 4-3`
Ffr23 <- `FfrTable 2-3`

ScmAdj <- 0.131771767836034

rm(folder1, folderS, folderU, File1Year, FileS, 
   FileU, names1, namesS, namesU, Vius2002, `FfrTable 4-3`,`FfrTable 2-3`)

save(list = ls(all = TRUE), file= "NonHwyRaw.RData")
library(data.table)
library(dplyr)
library(readr)


path <- "C:/Projects/NYTimes/outputs/Breaked files/time"

multmerge <- function(path){
  filenames <- list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, fread), fill=TRUE)
}


data <- multmerge(path)
nrow(data)
ncol(data)
time_data <- data[,-c(1,2004)] 


write.csv(time_data, paste('C:/Projects/NYTimes/outputs/Breaked files/time/merged_data_time_4.csv', sep=''), row.names = FALSE)

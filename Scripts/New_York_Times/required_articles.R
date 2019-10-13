#Can use to get section counts for any month

require(XML)
library(hash)

dir <- 'C:/Projects/NYTimes/NYTimes/nyt_corpus/data'
out <- 'D:/NYTimes'


all_files_years<-list.files(dir)
xml_files <- list()

required_sections <- c("Arts", "Business", "Opinion" ,"New York and Region")


isValid <- function(name){
  result <- FALSE
  
  for(section in required_sections){
    if(section == name){
      result <- TRUE
      break
    }
  }
  
  result
}



for(year in 1:length(all_files_years)){
  
  currentYear <- all_files_years[year]
  all_files_months<-list.files(paste0(dir,"/",currentYear,"/Extracted"))
  
  for(month in 1:length(all_files_months)){
    currentMonth <- all_files_months[month]
    all_files_days<-list.files(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth))
    print(paste0(dir,"/",currentYear,"/",currentMonth))
    

    for(day in 1:length(all_files_days)){
      all_files<-list.files(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth,"/",all_files_days[day]))
      print(length(all_files))
      for (f in 1:length(all_files)){
        xml_files <- htmlParse(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth,"/",all_files_days[day],"/",all_files[f]), useInternalNodes=T)
        section <- (xml_files['//meta[@name="online_sections"]/@content'])[[1]][["content"]]
    
        values <- strsplit(as.character(section),"; ")  
        
        for(i in values){
          for(j in 1:length(i)){
            if(isValid(i[j])){
              file.copy(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth,"/",all_files_days[day],"/",all_files[f]), out, overwrite = TRUE)
            }
          }
        }
      }
    }
  }
}















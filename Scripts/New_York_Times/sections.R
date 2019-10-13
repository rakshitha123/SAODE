#Get section counts for all years from 1987 - 2007

require(XML)
library(hash)

dir <- 'C:/Projects/NYTimes/NYTimes/nyt_corpus/data'


all_files_years<-list.files(dir)
xml_files <- list()

section_freq <- hash()


for(year in 1:length(all_files_years)){
  
  currentYear <- all_files_years[year]
  all_files_months<-list.files(paste0(dir,"/",currentYear,"/Extracted"))
  
  for(month in 1:length(all_files_months)){
    currentMonth <- all_files_months[month]
    all_files_days<-list.files(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth))
    print(paste0(dir,"/",currentYear,"/",currentMonth))
    
    for(day in 1:length(all_files_days)){
      all_files<-list.files(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth,"/",all_files_days[day]))
      for (i in 1:length(all_files)){
        xml_files <- htmlParse(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth,"/",all_files_days[day],"/",all_files[i]), useInternalNodes=T)
        section <- (xml_files['//meta[@name="online_sections"]/@content'])[[1]][["content"]]
        date <- (xml_files['//meta[@name="publication_day_of_month"]/@content'])[[1]][["content"]]
        month <- (xml_files['//meta[@name="publication_month"]/@content'])[[1]][["content"]]
        year <- (xml_files['//meta[@name="publication_year"]/@content'])[[1]][["content"]]
        publishedDate <- paste0(year,"-",month,"-",date)
        
        values <- strsplit(as.character(section),"; ")  
        
        for(i in values){
          
          for(j in 1:length(i)){
            if(is.null(section_freq[[i[j]]])){
              section_freq[[i[j]]] <- list(frequency = 1, startDate = publishedDate, endDate = publishedDate)
            }
            else{
              section_freq[[i[j]]]$frequency <- section_freq[[i[j]]]$frequency + 1
              section_freq[[i[j]]]$endDate <- publishedDate
            }
          }
        }
      }
    }
  }
  
}


print(section_freq)

cols <- c("Section", "Frequency", "StartDate", "EndDate")
classes <- c("character", "integer", "character","character")
tbl <- read.table(text = "", colClasses = classes, col.names = cols)

count <- 1
for (item in ls(section_freq)) {
  print(section_freq[[item]])
  tbl[count,"Section"] <- item
  tbl[count,"Frequency"] <- section_freq[[item]]$frequency
  tbl[count,"StartDate"] <- section_freq[[item]]$startDate
  tbl[count,"EndDate"] <- section_freq[[item]]$endDate
  count <- count + 1
}

print(tbl)
write.csv(tbl, paste('C:/Projects/NYTimes/Outputs/online_sections.csv', sep=''), row.names = FALSE)













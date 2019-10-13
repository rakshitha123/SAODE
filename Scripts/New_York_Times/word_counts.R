#Can use to get word counts for any month

require(XML)
library(hash)
library(tm)
library(stopwords)
library(stringr)
library(pluralize)

dir <- 'C:/Projects/NYTimes/NYTimes/nyt_corpus/data'
# dir <- 'C:/Projects/NYTimes/outputs/temp'

all_files_years<-list.files(dir)
xml_files <- list()

word_counts <- hash()


format_Word <- function(word){
  word <- gsub('[0-9]+', '', word)
  
  stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
  word <- str_replace_all(word, stopwords_regex, '')
  
  word <- str_replace_all(word, "[^[:alnum:]]", '')
  word <- tolower(word)
  word <- singularize(word)
  word
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
        for (i in 1:length(all_files)){
          temp <- xmlParse(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth,"/",all_files_days[day],"/",all_files[i]))
          #print(paste0(dir,"/",currentYear,"/Extracted","/",currentMonth,"/",all_files_days[day],"/",all_files[i]))
          #temp <- xmlParse(paste0("C:/Projects/NYTimes/outputs/temp/2005/Extracted/01/01/1638683.xml"))
          temp <- xmlToList(temp)
          lead <- temp$body$body.content$block
          text <- ""
          words <- ""
          
          if(!(is.null(lead))) {
            for(p in 2:length(lead)-1){
                if(text==""){
                  text <- as.character(lead[p])
                }
                else{
                  text <- paste0(text," ",as.character(lead[p]))
                }
            }
            
            
            words <- strsplit(as.character(text)," ")
            
            for(i in words){
              for(j in 1:length(i)){
                if((!(is.null(i[j]))) && (!(is.na(i[j])))){
                  value <- format_Word(i[j])
                  if(value!=""){
                    if(is.null(word_counts[[value]])){
                      word_counts[[value]] <- 1
                    }
                    else{
                      word_counts[[value]] <- word_counts[[value]] + 1
                    }
                  }
                }
              }
            }
          }
         
        }
      }
  }
}


print(word_counts)

cols <- c("Word", "Frequency")
classes <- c("character", "integer")
tbl <- read.table(text = "", colClasses = classes, col.names = cols)

count <- 1
for (item in ls(word_counts)) {
  print(word_counts[[item]])
  tbl[count,"Word"] <- item
  tbl[count,"Frequency"] <- word_counts[[item]]
  count <- count + 1
}

print(tbl)
write.csv(tbl, paste('C:/Projects/NYTimes/Outputs/word_counts.csv', sep=''), row.names = FALSE)













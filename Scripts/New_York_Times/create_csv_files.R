#Can use to create a csv file 

require(XML)
library(hash)
library(tm)
library(stopwords)
library(stringr)
library(pluralize)

dir <- 'C:/Projects/NYTimes/Filtered Data/NYTimes'


all_files<-list.files(dir)
xml_files <- list()


top_tokens <- read.csv(file="C:/Projects/NYTimes/Outputs/top_tokens.csv", header=TRUE, sep=",")
top_tokens <- top_tokens$Word


format_word <- function(word){
  word <- gsub('[0-9]+', '', word)
  
  stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
  word <- str_replace_all(word, stopwords_regex, '')
  
  word <- str_replace_all(word, "[^[:alnum:]]", '')
  word <- tolower(word)
  word <- singularize(word)
  word
}

getLabel <-function(sections){
  label <-""
  values <- strsplit(as.character(sections),"; ")[[1]]
  categories <- c("Arts", "Business", "New York and Region", "Opinion")
  
  if(length(values)==1){
    label <- assignLabel(values[1],label)
  }
  else{
    for(cat in categories){
      if(cat %in% values){
        label <- assignLabel(cat,label)
      }
    }
  }
 
  label
}


assignLabel <- function(cat, label){
  if(cat=="Arts"){
    label <- paste0(label,"A")
  }
  else if(cat=="Business"){
    label <- paste0(label,"B")
  }
  else if(cat=="New York and Region"){
    label <- paste0(label,"R")
  }
  else if(cat=="Opinion"){
    label <- paste0(label,"O")
  }
  label
}


cols <- c("published_year", "time_")
classes <- c("character", "character")
for(token in top_tokens){
  cols <- c(cols, as.character(token))
  classes <- c(classes, "character")
}
cols <- c(cols, "target_class")
classes <- c(classes, "character")
tbl <- read.table(text = "", colClasses = classes, col.names = cols)


count <- 1
for (f in 455001:600000) {
  temp <- xmlParse(paste0(dir, "/", all_files[f]))
  temp <- xmlToList(temp)
  lead <- temp$body$body.content$block
  xml_files <- htmlParse(paste0(dir,"/",all_files[f]), useInternalNodes=T)
  sections <- (xml_files['//meta[@name="online_sections"]/@content'])[[1]][["content"]]
  day <- (xml_files['//meta[@name="publication_day_of_week"]/@content'])[[1]][["content"]]
  year <- (xml_files['//meta[@name="publication_year"]/@content'])[[1]][["content"]]
  tokens <- c()
  
  if (!(is.null(lead)) && !(is.na(lead)) && (lead!="") && !(is.null(sections)) && !(is.na(sections)) && (sections!="") && !(is.null(day)) && !(is.na(day)) && (day!="")) {
    for (p in 2:length(lead) - 1) {
      tokens <- c(tokens, str_split(as.character(lead[p]), ' ')[[1]])
    }
   
    formatted_tokens <- format_word(tokens)
    
    if((is.null(year)) || (is.na(year))){
      year <- "1992"
    }
    tbl[count,"published_year"] <- year
    tbl[count,"time_"] <- day
    for(value in unique(formatted_tokens)){
      if ((value != "") && (!(is.null(value))) && (!(is.na(value)))) {
        if (value %in% top_tokens) {
          tbl[count,value] <- "Y"
        }
      }
    }
    tbl[count,"target_class"] <- getLabel(sections)
  }
  
  if((f %% 500) == 0){
    print(f)
  }
  
  if((f %% 1000) == 0){
    write.csv(tbl, na = "N", paste('C:/Projects/NYTimes/Outputs/csv_files/data_',f,'.csv', sep=''), row.names = FALSE)
    
    count <- 1
    
    cols <- c("published_year", "time_")
    classes <- c("character", "character")
    for(token in top_tokens){
      cols <- c(cols, as.character(token))
      classes <- c(classes, "character")
    }
    cols <- c(cols, "target_class")
    classes <- c(classes, "character")
    tbl <- read.table(text = "", colClasses = classes, col.names = cols)
  }
  else{
    count <- count +1 
  }
}



















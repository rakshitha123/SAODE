#Can use to get word counts for any month

require(XML)
library(hash)
library(tm)
library(stopwords)
library(stringr)
library(pluralize)

dir <- 'D:/NYTimes'


all_files<-list.files(dir)
xml_files <- list()

word_counts <- hash()


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


writeFile <- function(){
  cols <- c("Word", "Frequency")
  classes <- c("character", "integer")
  tbl <- read.table(text = "", colClasses = classes, col.names = cols)
  
  count <- 1
  for (item in ls(word_counts)) {
    tbl[count,"Word"] <- item
    tbl[count,"Frequency"] <- word_counts[[item]]
    count <- count + 1
  }
  
  write.csv(tbl, paste('C:/Projects/NYTimes/Outputs/word_counts.csv', sep=''), row.names = FALSE)
}


for (f in 1:length(all_files)) {
  temp <- xmlParse(paste0(dir, "/", all_files[f]))
  temp <- xmlToList(temp)
  lead <- temp$body$body.content$block
  tokens <- c()
  
  if (!(is.null(lead))) {
    for (p in 2:length(lead) - 1) {
      tokens <- c(tokens, str_split(as.character(lead[p]), ' ')[[1]])
    }
    #print(all_files[f])
    #print(tokens)
    formatted_tokens <- format_word(tokens)
    #print(formatted_tokens)
    # tokens_rle <- rle(formatted_tokens) #unique tokens with their respective counts
    # names <- tokens_rle$values  #unique token values
   
    
   for(value in unique(formatted_tokens)){
     if ((value != "") && (!(is.null(value))) && (!(is.na(value)))) {
        if (is.null(word_counts[[value]])) {
          word_counts[[value]] <- 1
        }
        else{
          word_counts[[value]] <- word_counts[[value]] + 1
        }
     }
   }
    
  }
  
  if((f %% 100) == 0){
    print(f)
  }
  
}

writeFile()

















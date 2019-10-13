library(hash)
library(rlist)


dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'



all_token_counts <- readRDS(file=file.path(dir, 'all_token_counts.rds'))   # contains doc id and its token names with their counts

full_doc_counts <- read.csv(file.path(dir, 'token_doc_counts.csv'), row.names = NULL, header=TRUE, stringsAsFactors = FALSE)   # contains token names and counts in all docs

top_token_doc_counts <- head(full_doc_counts[order(full_doc_counts$count, decreasing=TRUE),], 2000)  # sort tokens by descending order 

labels <- read.csv(paste(dir, '/top-topics2.csv',sep=''), stringsAsFactors = FALSE)
labels$dow <- factor(labels$dow, levels=c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
labels$iswd <- factor(as.integer(labels$dow) < 6)

# preallocate list for all of the entries
top_topic_counts <- vector(mode='list', length=length(all_token_counts))
names(top_topic_counts) <- names(all_token_counts)  #document id s


ccat <- labels[labels$CCAT==1,]
ccat <- ccat$itemid
ecat <- labels[labels$ECAT==1,]
ecat <- ecat$itemid
gcat <- labels[labels$GCAT==1,]
gcat <- gcat$itemid
mcat <- labels[labels$MCAT==1,]
mcat <- mcat$itemid


cols <- c("Token", "Weekdays", "Weekends")
classes <- c("character", "integer", "integer")
tbl <- read.table(text = "", colClasses = classes, col.names = cols)

row_Nums <- hash()
weekday_count <- 0
weekend_count <- 0

ids <- names(all_token_counts)
mapp <- hash()
for (i in 1:length(all_token_counts)) {
  mapp[[ids[i]]] <- all_token_counts[i][[1]]
  if(i %% 500 ==0){
    print(i)
  }
}
  
for (i in 1:length(gcat)) {
 
    doc_token_counts <- mapp[[as.character(gcat[i])]]
    
    selected_doc_token_counts <- doc_token_counts[intersect(top_token_doc_counts$token, names(doc_token_counts))]   #get most frequent tokens from each document with intersection
    
    iswd <- as.character(labels[i,"iswd"])
    
    if(iswd=="TRUE"){
      weekday_count <- weekday_count + 1
    }
    else{
      weekend_count <- weekend_count + 1
    }
    
    for(j in 1:length(selected_doc_token_counts)){
      currentToken <- names(selected_doc_token_counts[j])
      
      
      if(!(currentToken %in% tbl$Token)){
        row <- nrow(tbl)+1
        tbl[row,"Token"]<- currentToken
        tbl[row,"Weekdays"]<- 0
        tbl[row,"Weekends"]<- 0
        row_Nums[[currentToken]] <- row
      }
      if(iswd=="TRUE"){
        tbl[row_Nums[[currentToken]], "Weekdays"] <- tbl[row_Nums[[currentToken]], "Weekdays"] + 1
      }
      else{
        tbl[row_Nums[[currentToken]], "Weekends"] <- tbl[row_Nums[[currentToken]], "Weekends"] + 1
      }
      
    }
  
  
  if(i %% 500 ==0){
    print(i)
  }
}


write.csv(tbl, file.path(dir, 'concept_drift_gcat_counts.csv'), row.names = FALSE)

tbl$Weekdays <- tbl$Weekdays/weekday_count
tbl$Weekends <- tbl$Weekends/weekend_count
tbl$diff <- abs(tbl$Weekdays - tbl$Weekends)
write.csv(tbl, file.path(dir, 'concept_drift_gcat_probs.csv'), row.names = FALSE)


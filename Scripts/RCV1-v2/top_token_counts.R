# get token counts for most frequent tokens


library(hash)

#-----------------------------------------------------------------------------------------------

dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'

# number of tokens to include
token_count <- 5000

all_token_counts <- readRDS(file=file.path(dir, 'all_token_counts.rds'))   # contains doc id and its token names with their counts

full_doc_counts <- read.csv(file.path(dir, 'token_doc_counts.csv'), row.names = NULL, header=TRUE, stringsAsFactors = FALSE)   # contains token names and counts in all docs

top_token_doc_counts <- head(full_doc_counts[order(full_doc_counts$count, decreasing=TRUE),], token_count)  # sort tokens by descending order upto 5000name

# preallocate list for all of the entries
top_topic_counts <- vector(mode='list', length=length(all_token_counts))
names(top_topic_counts) <- names(all_token_counts)  #document id s

for (i in 1:length(all_token_counts)) {
  doc_token_counts <- all_token_counts[[i]]

  selected_doc_token_counts <- doc_token_counts[intersect(top_token_doc_counts$token, names(doc_token_counts))]   #get most frequent tokens from each document with intersection

  top_topic_counts[[i]] <- selected_doc_token_counts  # doc id and frequently used tokens with their counts
  
  #if (i %% 5000 == 0) {
   # print(i)
  #}
  #if (i >= 10000) break

}


saveRDS(top_topic_counts, file=file.path(dir, 'top_token_counts.rds'), compress='bzip2')

write.csv(top_token_doc_counts, file.path(dir, 'top_token_doc_counts.csv'), row.names = FALSE)


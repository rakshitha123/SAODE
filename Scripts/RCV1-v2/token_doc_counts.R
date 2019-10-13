# count the number of documents for each token

library(hash)

dir <- 'd:/temp'
all_token_counts <- loadRDS(file=file.path(dir, 'all_token_counts.rbin'))

# find all of the tokens used, along with document counts
doc_counts <- hash()
docs_processed <- 0
for (doc_id in names(all_token_counts)) {
  #print(doc_id)
  doc <- all_token_counts[[doc_id]]
  tokens <- names(doc)
  for (token in tokens) {
    #print(token)
    if (has.key(token, doc_counts)) {
      token_count <- doc_counts[[token]] + 1
    } else {
      token_count <- 1
    }
    doc_counts[[token]] <- token_count
  }
  docs_processed <- docs_processed + 1
  if (docs_processed %% 500 == 0) {
    print(docs_processed)
  }
  #if (length(doc_counts) > 20000) break
}

# convert counts to a data frame
doc_counts_df <- data.frame(token=keys(doc_counts), count=values(doc_counts))
rownames(doc_counts_df) <- NULL

write.csv(doc_counts_df, file.path(dir, 'token_doc_counts.csv'), row.names = FALSE)

# get token counts for each sample
# execution time 3+ hours

library(stringr)
library(hash)



# open a token file
openTokenFile <- function(filename) {
  gzfile(filename, open = "rt")
}



# get token counts for a single file
# con is similar to a file connection
getEntry <- function(con) {
  id=NULL
  token_counts=NULL

  lc <- 0 #may be line count
  tokens <- NULL
  while(lc < 1000) {
    line=str_trim(readLines(con, n=1))  #str_trim() removes whitespace from start and end of string and readLines function reads one line from file
    if (length(line) == 0 || str_length(line) == 0)  #Remove empty lines
      break
    #print(paste(str_length(line), '[', line, ']'))

    if (substr(line, 1, 3) == '.I ') {
      id <- as.integer(substr(line, 4, str_length(line)))
    } else if (substr(line, 1, 2) == '.W') {
      # do nothing - this is a sepparator for the tokens
    } else {
      tokens <- c(tokens, str_split(line, ' ')[[1]]) #split from space and get tokens of doc and append to tokens vector and get first element
    }

    lc <- lc + 1
  }

  if (is.null(id)){
    return(NULL)
  }
  

  tokens_rle <- rle(tokens) #unique tokens with their respective counts
  token_counts <- tokens_rle$lengths # unique token counts
  names(token_counts) <- tokens_rle$values  #unique token values
  
  return(list(id=id, token_counts=token_counts))
}



# -----------------------------------------------------------------------------



dir <- 'C:/Projects/Trevor/Article Classification/data/token_files'  #location of input files
outdir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'

entry_count <- 0
all_token_counts <- hash()  # creating hashmap



#inputfiles <- c('test.dat')

#inputfiles <- c('lyrl2004_tokens_train.dat')

#inputfiles <- c('lyrl2004_tokens_test_pt0.dat')

inputfiles <- c('lyrl2004_tokens_train.dat', 'lyrl2004_tokens_test_pt0.dat', 'lyrl2004_tokens_test_pt1.dat', 'lyrl2004_tokens_test_pt2.dat', 'lyrl2004_tokens_test_pt3.dat')



doc_counts <- hash() # creating hashmap

for (inputfile in inputfiles) {
  con <- openTokenFile(file.path(dir, inputfile))
  while(TRUE) {
    entry <- getEntry(con)
    if (is.null(entry))
        break
    #print(entry$id)

    # update document counts for the tokens
    for (token in names(entry$token_counts)) {
      #print(token)
      if (has.key(token, doc_counts)) {
        token_count <- doc_counts[[token]] + 1
      } else {
        token_count <- 1
      }
      doc_counts[[token]] <- token_count  #hashmap containing count of a particular token in all documents
    }

    all_token_counts[[as.character(entry$id)]] <- entry$token_counts

    entry_count <- entry_count + 1
    

    #if (entry_count %% 500 == 0) {
      #print(entry_count)
    #}
  }

  close(con)
}



# convert hash into a list
atc_list <- values(all_token_counts)   #contains token name and count for each document separately
names(atc_list) <- keys(all_token_counts) #get token names

saveRDS(atc_list, file=file.path(outdir, 'all_token_counts.rds'))

#load(file=file.path(outdir, 'all_token_counts.rbin'))

# convert counts to a data frame
doc_counts_df <- data.frame(token=keys(doc_counts), count=values(doc_counts))   #get all tokens and counts to get most frequent counts
rownames(doc_counts_df) <- NULL

write.csv(doc_counts_df, file.path(outdir, 'token_doc_counts.csv'), row.names = FALSE)


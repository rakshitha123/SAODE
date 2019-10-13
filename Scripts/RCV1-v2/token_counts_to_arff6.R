# convert token counts to an arff file, with minimum number of docs for tokens and encoding of time

library(hash)

#-----------------------------------------------------------------------------------------------



encodeCounts <- function(itemid, target, all_tokens, all_token_counts,
                         time_encoding_type=NULL, encoded_time=NULL, outdir='.', maxitems=1000, is.binary=FALSE, include_emptyset=FALSE) {
  if (is.null(encoded_time)) {
    time_attr_count <- 0
    time_encoding_type <- 'none'
  }
  else if ('data.frame' %in% class(encoded_time)) {
    time_attr_count <- ncol(encoded_time)
  }
  else {
    time_attr_count <- 1
  }

  base.filename <- paste(ifelse(include_emptyset,'ps','tc'), length(all_tokens), '_', format(maxitems, scientific=FALSE), '_', time_encoding_type, '_', ifelse(is.binary, 'b', 'c'), sep='')
  print(paste('filename:', base.filename))
  tmp_arff_file <- file.path(outdir, paste(base.filename, '.tmp', sep=''))
  arff_file <- file.path(outdir, paste(base.filename, '.arff', sep=''))

  if (file.exists(tmp_arff_file)) {
    if (file.remove(tmp_arff_file)) {
      print('removed partial results')
    }
    else {
      print('skipping in progress file')
      return(NULL)
    }
  }

  if (file.exists(arff_file)) {
    print('skipping completed file')
    return(NULL)
  }

  token_indexes <- hash(keys=all_tokens, values=time_attr_count - 1 + (1:length(all_tokens)))

  # target attribute is after all of the other attributes
  target_attr_num <- length(token_indexes) + time_attr_count

  con <- file(tmp_arff_file, "wt")

  # output header to arff file
  cat('@RELATION token_counts\n\n', file=con)

  # encoding of time
  if (is.factor(encoded_time)) {
    cat('@ATTRIBUTE time_ {', paste(levels(encoded_time), collapse = ','), '}\n', sep='', file=con)
  }
  else if (is.numeric(encoded_time)) {
    cat('@ATTRIBUTE time_ NUMERIC\n', sep='', file=con)
  }
  else if (!is.null(encoded_time)) {
    stop('Unknown time encoding')
  }

  if (is.binary) {
    for (token in all_tokens) {
      cat('@ATTRIBUTE', token, '{ N, Y}\n', file=con)
    }
  }
  else {
    for (token in all_tokens) {
      cat('@ATTRIBUTE', token, 'NUMERIC\n', file=con)
    }
  }

  # target value - concatentated version of top level categories
  cat('@ATTRIBUTE target_class {', paste(levels(target), collapse = ','), ifelse(include_emptyset, ', none',''), '}\n', sep='', file=con)

  # output data in sparse format
  cat('\n@DATA\n', file=con)

  for (i in 1:nrow(labels)) {
    id <- itemid[i]
    doc <- all_token_counts[[as.character(id)]]
    if (is.null(doc)) {
      print(paste('item', itemid, 'missing data'))
      break
    }

    tokens <- names(doc)

    # get token index and value for the tokens
    token_ind <- sapply(tokens, function(t) token_indexes[[t]])
    token_ind <- unlist(token_ind) # discard any NULLs
    if (is.null(token_ind)) {
      token_ind <- numeric(0)
    }
    #print(paste(token_ind, collapse=', '))

    if (is.binary) {
      token_counts <- rep(1, length(doc))
    }
    else {
      token_counts <- doc
    }

    cat('{', file=con)

    # output encoded time
    if (!is.null(encoded_time)) {
      cat('0 ', as.character(encoded_time[i]), ', ', sep='', file=con)
    }

    #print(paste(token_ind, collapse=', '))
    for (ind in order(token_ind)) {
      ti <- token_ind[[ind]]
      if (is.binary)
        ct <- 'Y'
      else
        ct <- token_counts[[ind]]

      cat(ti, ' ', ct, ', ', sep='', file=con)
    }

    cat(target_attr_num, as.character(target[i]), '}\n', file=con)

    if ((i %% 5000) == 0) {
      print(i)
    }
    if (i >= maxitems) break

  }

  close(con)

  file.rename(tmp_arff_file, arff_file)
}



################################################################################################



data.dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'

input.dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'
output.dir <- 'C:/Projects/AODE/inputs'

top_token_counts <- readRDS(file=file.path(input.dir, 'top_token_counts.rds'))

top_doc_counts <- read.csv(file.path(input.dir, 'top_token_doc_counts.csv'), row.names = NULL, header=TRUE, stringsAsFactors = FALSE)

labels <- read.csv(paste(data.dir, '/top-topics2.csv',sep=''), stringsAsFactors = FALSE)
labels$dow <- factor(labels$dow, levels=c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))

labels$iswd <- factor(as.integer(labels$dow) < 6)

#labels$dayNo <- factor(labels$day.num, levels=c(1:348))
labels$dayNo <- factor(labels$day.num, levels=c(1:4))

combined_labels <- paste(ifelse(labels$CCAT, 'C', ''), ifelse(labels$ECAT, 'E', ''),
                         ifelse(labels$GCAT, 'G', ''), ifelse(labels$MCAT, 'M', ''), sep='')
labels$combined <- factor(combined_labels)


max.items <- 10000
max.tokens <- 500
include_emptyset=FALSE
#include_emptyset=TRUE

for(max.tokens in c(2000)) {
  #for(max.tokens in c(50, 100, 200, 500, 1000, 2000)) {
  # allocate indexes for tokens in descending order by the number of documents
  # this is done to give the most frequent tokens the lowest index, reducing the size of the arff file produced

  sorted_tokens <- top_doc_counts$token[order(top_doc_counts$count, decreasing = TRUE)][1:max.tokens]

  for(max.items in c(9837)) {
    #for(max.items in c(10000, 100000, 400000, 800000, 804414)) {
    
    #baseline, no time encoding
    #encodeCounts(itemid=labels$itemid, target=labels$combined, all_tokens=sorted_tokens,
     #           all_token_counts=top_token_counts, outdir=output.dir, maxitems=max.items, is.binary=TRUE,
      #          include_emptyset=include_emptyset)

    # dow as categorical
    #encodeCounts(itemid=labels$itemid, target=labels$combined, all_tokens=sorted_tokens,
     #            all_token_counts=top_token_counts, time_encoding_type='dow', encoded_time=labels$dow,
      #           outdir=output.dir, maxitems=max.items, is.binary=TRUE,include_emptyset=include_emptyset)

    # iswd as categorical
    #encodeCounts(itemid=labels$itemid, target=labels$combined, all_tokens=sorted_tokens,
    #             all_token_counts=top_token_counts, time_encoding_type='iswd', encoded_time=labels$iswd,
    #            outdir=output.dir, maxitems=max.items, is.binary=TRUE,include_emptyset=include_emptyset)
    
    #day number as categorical
    encodeCounts(itemid=labels$itemid, target=labels$combined, all_tokens=sorted_tokens,
                 all_token_counts=top_token_counts, time_encoding_type='dayNo', encoded_time=labels$dayNo,
                outdir=output.dir, maxitems=max.items, is.binary=TRUE,include_emptyset=include_emptyset)
  }
}

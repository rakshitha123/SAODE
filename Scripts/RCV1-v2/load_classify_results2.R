library(stringr)
library(plyr)
library(data.table)


output.dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'
  

# ------------------------------------------
# calculate matrices for obtaining measures for a pair of actual and predicted values
actualMap <- function(targetValues) {
  targetValueSets <- list()
  all_labels = NULL
  for (tv in targetValues) {
    if (tv == 'none') {
      labelSet <- c()
    } else {
      # convert string into a vector of characters, giving the label set
      labelSet <- regmatches(tv, gregexpr('.', tv))[[1]]
    }
    all_labels <- union(all_labels, labelSet)
    targetValueSets[[tv]] <- labelSet
  }

  m <- matrix(nrow = length(targetValues),
              ncol = length(all_labels), data = 0)
  colnames(m) <- all_labels
  row.names(m) <- targetValues


  for (r in targetValues) {
      s1 <- targetValueSets[[r]]

      m[r,s1] <- 1
    }

  return(m)
}

# ------------------------------------------

#files <- list.files(output.dir, pattern='^tc|tr[0-9]*_.*\\.csv')
files <- list.files(output.dir)
fi <- file.info(paste(output.dir, '/', files, sep=''), extra_cols = FALSE)

# regex to extract info from filenames
dataset.info.re <- '^([a-z]+)([0-9]+)_([0-9]+)_([^_]+)_([^-]*)-(.*)\\..*$'

dataset.info <- as.data.frame(str_match(files, dataset.info.re))[,-1]
colnames(dataset.info) <- c('dstype', 'token_count', 'sample_count', 'time_encoding', 'feature_encoding', 'classifier')
rownames(dataset.info) <- files

# convert columns to numeric
dataset.info$token_count <- as.integer(as.character(dataset.info$token_count))
dataset.info$sample_count <- as.integer(as.character(dataset.info$sample_count))

dataset.info$etime <- as.integer(difftime(fi$mtime, fi$ctime, units='secs'))

print(paste('#classification results =', nrow(dataset.info)))

#max.files.processed <- 10000
max.files.processed <- 1000000
#max.files.processed <- 2
files.processed <- 0

dataset.info$rdsfile <- paste(output.dir, '/', sub('.csv$','.rds', rownames(dataset.info)), sep='')
# read data
for (i in seq(nrow(dataset.info))) {
  if (files.processed >= max.files.processed) {
    print('reached max files processed - skipping rest')
    break
  }

  filename <- rownames(dataset.info)[i]
  print(paste('file =', filename, ', row =', i, '/', nrow(dataset.info)))

  if (file.exists(dataset.info$rdsfile[i])) {
    print('skipping already processed file')
    next
  }
  files.processed <- files.processed + 1

  print('loading')
  df <- read.table(paste(output.dir, '/', filename, sep=''), header=TRUE, sep=',', stringsAsFactors = FALSE)

  distribution <- df[,-1]
  colnames(distribution) <- gsub('^Pr', '', colnames(distribution))

  actual_mappings <- actualMap(colnames(distribution))

  actual <- actual_mappings[df$Actual,]
  predicted.distribution <- as.matrix(distribution) %*% actual_mappings
  predicted.discrete <- ifelse(predicted.distribution > 0.5,1.0,0)

  print('calculating measures')
  label.err <- predicted.distribution - actual

  eps=1e-15
  predicted.distribution2 <- pmin(pmax(predicted.distribution, eps), 1-eps)
  label.log.loss <- actual * log(predicted.distribution2) + (1 - actual) * log(1 - predicted.distribution2)

  label.eval <- as.data.frame(ifelse(actual,ifelse(predicted.discrete, 'tp', 'fn'), ifelse(predicted.discrete,'fp','tn')),
                              stringsAsFactors = FALSE)
  for (x in 1:ncol(label.eval)) {
    label.eval[,x] <- factor(label.eval[,x], levels=c('tp','fp','tn','fn'))
  }
  rownames(label.eval) <- NULL

  prediction.lcount <- apply(predicted.discrete,1,sum)
  actual.lcount <- apply(actual,1,sum)
  intersection.len <- apply(actual & predicted.discrete,1,sum)
  union.len <- apply(actual | predicted.discrete,1,sum)

  result.info <- as.list(dataset.info[i,])
  result.info$label_count <- ncol(predicted.discrete)

  result.info$row.stats <- data.frame(
    prediction_lcount=prediction.lcount,
    actual_lcount=actual.lcount,
    intersection_len=intersection.len,
    union_len=union.len
  )
  result.info$label.stats <- list(
    label.err=label.err,
    label.eval=label.eval,
    label.log.loss=label.log.loss
  )

  print('saving')
  saveRDS(result.info, file = dataset.info$rdsfile[i], compress = TRUE)

}

saveRDS(dataset.info, file = paste(output.dir, '/dataset_info.rds', sep=''), compress = TRUE)


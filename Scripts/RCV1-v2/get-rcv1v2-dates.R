library(utils)
library(XML)

rcv1.dir <- 'd:/corpora/rcv1'
rcv1v2.ids.file <- 'D:/corpora/reuters-v1-v2/jmlr.csail.mit.edu/papers/volume5/lewis04a/a07-rcv1-doc-ids/rcv1v2-ids.dat.gz'

output.dir = 'cooked/'

files <- list.files(path = rcv1.dir, pattern = '[0-9]{8}.zip', full.names = TRUE, recursive = TRUE)

all.articles <- NULL
for (file in files) {
  file <- file
  print(file)

  # get list of files inside the zip file
  xml.files <- unzip(file, files = NULL, list = TRUE)$Name
  xml.files <- xml.files[grepl('[0-9]+newsML.xml', xml.files, ignore.case = FALSE)]

  # extract info from the compressed documents
  article.info <- data.frame(itemid=rep(-1, length(xml.files)), date='', stringsAsFactors=F)

  file.date <- gsub('([0-9]{4})([0-9]{2})([0-9]{2}).*', '\\1-\\2-\\3', basename(file))
  item.ids <- as.integer(gsub('^([0-9]+).*', '\\1', xml.files))
  article.info <- data.frame(itemid=item.ids, date=file.date, stringsAsFactors=F)

  print(paste('File info read for', length(xml.files), 'files - ids', min(article.info$itemid), '-', max(article.info$itemid)))

  if (is.null(all.articles)) {
    all.articles <- article.info
  } else {
    all.articles <- rbind(all.articles, article.info)
  }
}

rcv1v2.ids <- read.table(gzfile(rcv1v2.ids.file), header=F, col.names=c('itemid'))

rcv1v2.article.dates <- merge(rcv1v2.ids, all.articles)

write.csv(rcv1v2.article.dates, paste(output.dir, 'rcv1v2_article_date.csv', sep=''), row.names = FALSE)

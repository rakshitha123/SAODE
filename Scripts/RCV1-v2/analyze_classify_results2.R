library(stringr)
library(ggplot2)
library(reshape2)
library(scales)
library(data.table)
library(RColorBrewer)
library(xtable)
library(tools)
library(ggrepel)

set.seed(42)


output.dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'


cooked.data.dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'

thesis.fig.dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs/figures/'

# ------------------------------------------

ReadResults <- function(rdsfile) {
  all_result_info <- NULL
  for (f in rdsfile) {
    result.info <-  readRDS(file = f)

    all_result_info[[length(all_result_info) + 1]] <- result.info
  }

  return(all_result_info)
}

# ------------------------------------------

MyCbind <- function(dt, col) {
  if (is.null(dt)) {
    return(data.table(col))
  }
  dt[[ncol(dt) + 1]] <- col
  return(dt)
}

# ------------------------------------------

CalcMeasures <- function(all_result_info, maxlen=NULL) {
  common_values <- all_result_info[[1]]

  raw.measures <- NULL

  for (e in all_result_info) {
      raw.measures$hamming <- MyCbind(raw.measures$hamming,
                                      (e$row.stats$union_len - e$row.stats$intersection_len) / common_values$label_count)
      raw.measures$subset_accuracy <- MyCbind(raw.measures$subset_accuracy,
                                              as.integer(e$row.stats$intersection_len == e$row.stats$union_len))
      raw.measures$accuracy <- MyCbind(raw.measures$accuracy,
                                       ifelse(e$row.stats$union_len==0, 1, e$row.stats$intersection_len / e$row.stats$union_len))
      raw.measures$fscore <- MyCbind(raw.measures$fscore,
                                     2 * e$row.stats$intersection_len / (e$row.stats$actual_lcount + e$row.stats$prediction_lcount))
      raw.measures$mse <- MyCbind(raw.measures$mse,
                                    rowMeans((e$label.stats$label.err)^2))
  }

  if(!is.null(maxlen)) {
    raw.measures <- sapply(raw.measures, function(m) m[1:maxlen,,drop=F])
  }
  return(raw.measures)
}

# ------------------------------------------
CalcLabelMetrics <- function(all_result_info) {
  all_rmse <- NULL
  all_log_loss_simple <- NULL
  all_log_loss <- NULL
  for (e in all_result_info) {
    se <- as.data.frame((e$label.stats$label.err)^2)
    mse <- sapply(se, function(c) cumsum(c) / seq_along(c))
    rmse <- sqrt(mse)

    ll <- as.data.frame(e$label.stats$label.log.loss)
    mll <- sapply(ll, function(c) cumsum(c) / seq_along(c))
    maxloss <- log(1:nrow(mll))
    mll.limited <- pmin(mll, maxloss)

    all_rmse[[length(all_rmse) + 1]] <- rmse
    all_log_loss_simple[[length(all_log_loss_simple) + 1]] <- mll
    all_log_loss[[length(all_log_loss) + 1]] <- mll.limited
  }

  return(list(rmse=all_rmse, log.loss.simple=all_log_loss_simple, log.loss=all_log_loss))
}

# ------------------------------------------

AnalyzeDatasets <- function(ds.info, row.attribute.names=TRUE, keep.data=TRUE, maxlen=NULL) {
  rdsfile <- ds.info$rdsfile
  ds.info$rdsfile <- NULL

  # find which columns have the same value for all selected rows
  col.unique.count <-
    apply(ds.info, 2, function(c)
      length(unique(c)))

  common.columns <- which(col.unique.count == 1)

  if (length(common.columns) > 0) {
    common.title <-
    paste(paste(names(common.columns), '=', sapply(ds.info[1, common.columns, drop=FALSE], as.character)), collapse = ', ')
  } else {
    common.title <- NULL
  }
  uncommon.columns <- which(col.unique.count > 1)

  # row titles should be built from the uncommon columns
  row.title.columns <- uncommon.columns

  row.title <- NULL
  if (length(row.title.columns) > 0) {
    row.title.info <-
      apply(ds.info[, row.title.columns, drop=FALSE], 2, as.character)
    for (cname in colnames(row.title.info)) {
      if (row.attribute.names) {
        rtitle <- paste(cname, '=', row.title.info[, cname], sep='')
      }
      else {
        rtitle <- row.title.info[, cname]
      }
      if (is.null(row.title))
        row.title <- rtitle
      else
        row.title <- paste(row.title, ', ', rtitle, sep = '')
    }
  }

  measure_names <- NULL
  raw.measures <- NULL
  label.measures <- NULL

  if ('sample_count' %in% colnames(ds.info)[common.columns]) {
    all_result_info <- ReadResults(rdsfile)

    raw.measures <- CalcMeasures(all_result_info, maxlen)
    measure_names <- c(names(raw.measures), 'rmse')
    for (measure in measure_names) {
      # get measures for the selected tables
      if (measure == 'rmse') {
        measure_table <- raw.measures[['mse']]
      } else {
        measure_table <- raw.measures[[measure]]
      }

      ds.info[,measure] <- sapply(measure_table, mean)
      if (measure == 'rmse') {
        ds.info[,measure] <- sqrt(ds.info[,measure])
      } else {
        if (!is.null(row.title)) {
          colnames(raw.measures[[measure]]) <- row.title
        }
      }
    }

    if (keep.data) {
      label.measures <- CalcLabelMetrics(all_result_info)
    } else {
      raw.measures <- NULL
    }
  }
  measure.columns <- which(colnames(ds.info) %in% measure_names)

  return(list(ds.info=ds.info[,c(uncommon.columns, measure.columns), drop=FALSE], n=nrow(ds.info),
              common.title = common.title, row.title = row.title,
              raw.measures=raw.measures, label.measures=label.measures))
}

# ------------------------------------------

AvgMeasures <- function(dataset.analysis, maxlen=NULL) {
  ds.info <- dataset.analysis$ds.info
  raw.measures <- dataset.analysis$raw.measures

  measure_names <- c(names(raw.measures), 'rmse')
  for (measure in measure_names) {
    # get measures for the selected tables
    if (measure == 'rmse') {
      measure_table <- raw.measures[['mse']]
    } else {
      measure_table <- raw.measures[[measure]]
    }

    if (!is.null(maxlen)) {
      measure_table <- measure_table[1:maxlen,,drop=F]
    }
    ds.info[,measure] <- sapply(measure_table, mean)
    if (measure == 'rmse') {
      ds.info[,measure] <- sqrt(ds.info[,measure])
    }
  }

  return(ds.info)
}

# ------------------------------------------

AvgCumulativeMeasure <- function(dataset.analysis, measure) {
  measures <- as.matrix(sapply(dataset.analysis$raw.measures[[measure]], function(c) cumsum(c) / seq_along(c)))
  if (measure == 'rmse') {
    measures <- sqrt(measures)
  }
  return(measures)
}

# ------------------------------------------

AvgGroupMeasure <- function(dataset.analysis, measure, groups, maxlen=NULL) {
  if (measure == 'rmse') {
    measures <- dataset.analysis$raw.measures[['mse']]
  } else {
    measures <- dataset.analysis$raw.measures[[measure]]
  }

  if (!is.null(maxlen)) {
    measures <- measures[1:maxlen,]
  }

  ds.names <- colnames(measures)

  sample.count <- nrow(measures)

  # truncate groups to match the length of the data
  groups <- groups[1:sample.count]
  group.counts <- aggregate(list(count=rep(1, sample.count)), by=list(group=groups), FUN=sum)

  group.measure <- sapply(measures, function(c) aggregate(c, by=list(groups), FUN=mean)[[2]])
  if (measure == 'rmse') {
    group.measure <- sqrt(group.measure)
  }
  row.names(group.measure) <- as.character(group.counts$group)
  colnames(group.measure) <- ds.names

  return(list(group.measure=group.measure, group.counts=group.counts))
}

# ------------------------------------------

ClassifierResults <- function(ds, encoding) {
  data.frame(classifier=ds$classifier,
             time_encoding=encoding,
             ds[,colnames(ds) != 'classifier'],
             stringsAsFactors=FALSE)
}

# ------------------------------------------

TransformVector <- function(data, name, nsmall=3) {
  if (is.numeric(data)) {
    data <- round(data, nsmall)
    if (grepl('loss|mse|err', name, ignore.case=TRUE)) {
      best <- min(data)
    } else {
      best <- max(data)
    }
    data <- paste(ifelse(data == best, '\\textbf{', ''), format(data, nsmall=nsmall), ifelse(data == best, '}', ''), sep='')
  }
  return(data)
}

# ------------------------------------------

ClassifierResultsToLatex <- function(tbl, filename, baseline=NULL) {
  tbl$mse <- NULL
  tbl$subset_accuracy <- NULL
  tbl <- tbl[with(tbl, order(classifier, time_encoding)),]

  if (!is.null(baseline)) {
    # put baseline to start of table
    tbl <- rbind(tbl[tbl$classifier == baseline,], tbl[tbl$classifier != baseline,])
    tbl[tbl$classifier == baseline, 'classifier'] <- paste('\\textit{', baseline, '}', sep='')
  }

  tbl$time_encoding <- ifelse(nchar(tbl$time_encoding) > 0, tbl$time_encoding, 'None')

  cn <- sub('hamming', 'Hamming Loss', sub('fscore', 'F\\\\textsubscript{1}-Score', colnames(tbl)))
  cn <- toTitleCase(gsub('_', ' ', cn))
  cn <- sub('mse', 'MSE', cn, ignore.case = TRUE)

  for (i in 1:ncol(tbl)) {
    tbl[, i] <- TransformVector(tbl[,i], cn[i])
  }

  sink(filename)
  cat('\\begin{tabular}{ll', paste(replicate(length(cn) - 2, 'r'), collapse=''), '}\n', sep='')
  cat('\\hline\n')
  cat(paste(cn, collapse=' & '), '\\\\\n', sep='')
  cat('\\hline\n')
  prev_classifier <- ''
  for (i in 1:nrow(tbl)) {
    row <- tbl[i,setdiff(colnames(tbl), c('classifier', 'time_encoding'))]
    time_encoding <- tbl$time_encoding[i]
    cl <- tbl$classifier[i]
    if (cl == prev_classifier) {
    } else {
      cat('\\noalign{\\vskip 1mm}',cl,sep='')
    }
    cat(' & ', time_encoding, ' & ', paste(row, collapse=' & '), '\\\\\n', sep='')
    prev_classifier <- cl
  }
  cat('\\hline\n')
  cat('\\end{tabular}\n')
  sink()
}

# ------------------------------------------

CmpTestTrainError <- function(test.info, train.info, time_encoding, classifier_name, filename, dates, cmplen=1000) {
  test_info = AnalyzeDatasets(test.info[test.info$token_count == max(test.info$token_count) &
                                          test.info$time_encoding == time_encoding &
                                          test.info$classifier == classifier_name,],
                            row.attribute.names=FALSE)
  test_rmse_per_day <- AvgGroupMeasure(test_info, 'rmse', groups = dates)
  test_error <- sqrt(mean(tail(test_info$raw.measures$mse[[1]], cmplen)))

  cairo_pdf(width=8, height=3, pointsize=8, filename=filename)
  p <- ggplot(data.frame(x=as.Date(rownames(test_rmse_per_day$group.measure)), y=test_rmse_per_day$group.measure[,1]), aes(x=x,y=y)) +
    geom_line(size=0) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
    theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
    labs(title='', y='RMSE per day', x='Article date')
  print(p)
  dev.off()

  train_error <- NULL
  train_info <- AnalyzeDatasets(train.info[train.info$token_count == max(test.info$token_count) &
                                            train.info$time_encoding == time_encoding &
                                            train.info$classifier == classifier_name,],
                              row.attribute.names=FALSE)

  if (!is.null(train_info$raw.measures$mse)) {
    train_error <- sqrt(mean(tail(train_info$raw.measures$mse[[1]], cmplen)))
  }

  return(list(classifier=classifier_name, time_encoding=time_encoding, test_error=test_error, train_error=train_error, cmplen=cmplen, test_rmse_per_day=test_rmse_per_day))
}

# ------------------------------------------

#dataset.info <-  readRDS(file = paste(output.dir, '/dataset_info.rds', sep=''))

files <- list.files(output.dir, pattern='^ps[0-9]*_.*\\.csv')
files <- list.files(output.dir, pattern='^[a-z][a-z][0-9]*_.*\\.rds')
fi <- file.info(paste(output.dir, '/', files, sep=''), extra_cols = FALSE)

# regex to extract info from filenames
dataset.info.re <- '^([a-z]+)([0-9]+)_([0-9]+)_([^_]+)_([^-]*)-(.*)\\..*$'

dataset.info <- as.data.frame(str_match(files, dataset.info.re))[,-1]
colnames(dataset.info) <- c('dstype', 'token_count', 'sample_count', 'time_encoding', 'feature_encoding', 'classifier')
rownames(dataset.info) <- files

# column fixups
dataset.info$feature_encoding <- NULL
dataset.info$etime <- NULL
dataset.info$token_count <- as.integer(as.character(dataset.info$token_count))
dataset.info$sample_count <- as.integer(as.character(dataset.info$sample_count))
dataset.info$rdsfile <- paste(output.dir, '/', sub('.csv$','.rds', rownames(dataset.info)), sep='')

# filter out uninteresting results
#dataset.info <- dataset.info[dataset.info$sample_count == 800000 &
dataset.info <- dataset.info[!(grepl('Tan[13]|TrevorNaiveBayesTree|OzaBagAdwin|TrevorNaiveBayes$|Tan4tp10DecayL2p0', dataset.info$classifier)),]
dataset.info <- dataset.info[order(dataset.info$classifier),]

print(paste('result count =', nrow(dataset.info)))

print(paste('classifier type count =', length(unique(dataset.info$classifier))))

all_linetypes <- c(rep("solid", 8), rep("dashed", 8),  rep("dotted", 8),  rep("dotdash", 8),  rep("longdash", 8))
all_colours <- c(brewer.pal(8, "Dark2"), brewer.pal(8, "Dark2"), brewer.pal(8, "Dark2"), brewer.pal(8, "Dark2"), brewer.pal(8, "Dark2"))

# rename classifier to make more meaningful
dataset.info$classifier <- sub('tp1',',N0=0.1',sub('TrevorNaiveBayesTime', 'TDB', dataset.info$classifier))
dataset.info$classifier <- sub('N0=0\\.10','N0=10', dataset.info$classifier)
dataset.info$classifier <- sub('StackedClassifier', 'NB-T', dataset.info$classifier)
dataset.info$classifier <- sub('(Tan[0-9])$','\\1,N0=1',sub('Tan$', 'Tan1', dataset.info$classifier))
dataset.info$classifier <- sub('WithTan', '-MLB', dataset.info$classifier)
dataset.info$classifier <- sub('MLB4', 'MLB2', sub('MLB2', 'MLB', dataset.info$classifier))
dataset.info$classifier <- sub('DecayL([0-9]+)p([0-9]+)', '-ED,lambda=\\1.\\2', dataset.info$classifier)
dataset.info$classifier <- sub('TDB-MLB2,N0=0.1-ED,lambda=2.0', 'TDB-MLB2-ED,N0=1,lambda=2.0', dataset.info$classifier)

# split data into test info and training info
training.info <- dataset.info[dataset.info$dstype == 'tr',]
#dataset.info <- dataset.info[dataset.info$dstype == 'ps',]
dataset.info <- dataset.info[dataset.info$dstype == 'tc',]

# ------------------------------------------
# get set of dates for samples
full.article.dates <- read.csv(file = paste(cooked.data.dir, '/rcv1v2_article_date.csv', sep=''), colClasses = c("integer", "Date"))
article.topics <- read.csv(file = paste(cooked.data.dir, '/top-topics.csv', sep=''))

# merge date info with labels
# note that date info is for the full dataset (prior to the lewis cleanup
# the merge discards any that are not part of the revised dataset)
article.dates <- full.article.dates[full.article.dates$itemid %in% article.topics$itemid,]
#article.dates <- merge(article.topics, full.article.dates)[,c('itemid', 'date')]

#sample.dates <- read.csv(file = paste(cooked.data.dir, '/article_date.csv', sep=''), colClasses = c("integer", "Date"))
# ------------------------------------------

all.classifiers <- unique(dataset.info$classifier)
baseline.classifiers <- setdiff(all.classifiers, grep('MLB|Trevor|NB-T|TDB|OzaBoostAdwin', all.classifiers, value=TRUE))
new.classifiers <- grep('TDB', all.classifiers, value=TRUE)
new.classifiers <- c(new.classifiers, 'NB-T')

baseline.classifiers
new.classifiers

# baseline classifiers (without special handling of time variable)
baseline_none = AnalyzeDatasets(dataset.info[dataset.info$token_count == max(dataset.info$token_count) &
                                    dataset.info$time_encoding == 'none' &
                                    dataset.info$classifier %in% baseline.classifiers,],
                     row.attribute.names=F)
baseline_none$ds.info
tbl <- baseline_none$ds.info
colnames(tbl) <- sub('hamming', 'Hamming Loss', sub('fscore', 'F-Score', colnames(tbl)))
colnames(tbl) <- toTitleCase(gsub('_', ' ', colnames(tbl)))
print(xtable(tbl), floating=FALSE, include.rownames=FALSE, file=paste(thesis.fig.dir, "baseline_with_none.tex", sep=''))

# baseline classifiers with dow time encoding
baseline_dow = AnalyzeDatasets(dataset.info[dataset.info$token_count == max(dataset.info$token_count) &
                                    dataset.info$time_encoding == 'dow' &
                                    dataset.info$classifier %in% baseline.classifiers,],
                     row.attribute.names=F)
baseline_dow$ds.info
tbl <- baseline_dow$ds.info
colnames(tbl) <- sub('hamming', 'Hamming Loss', sub('fscore', 'F-Score', colnames(tbl)))
colnames(tbl) <- toTitleCase(gsub('_', ' ', colnames(tbl)))
print(xtable(tbl), floating=FALSE, include.rownames=FALSE, file=paste(thesis.fig.dir, "baseline_with_dow.tex", sep=''))

# baseline classifiers with iswd time encoding
baseline_iswd = AnalyzeDatasets(dataset.info[dataset.info$token_count == max(dataset.info$token_count) &
                                    dataset.info$time_encoding == 'iswd' &
                                    dataset.info$classifier %in% baseline.classifiers,],
                     row.attribute.names=F)
baseline_iswd$ds.info
tbl <- baseline_iswd$ds.info   #Contains error. accuracy info
colnames(tbl) <- sub('hamming', 'Hamming Loss', sub('fscore', 'F-Score', colnames(tbl)))
colnames(tbl) <- toTitleCase(gsub('_', ' ', colnames(tbl)))
print(xtable(tbl), floating=FALSE, include.rownames=FALSE, file=paste(thesis.fig.dir, "baseline_with_iswd.tex", sep=''))


# combine performance stats for different time encodings
baseline_combined <- rbind(ClassifierResults(baseline_none$ds.info, ''),
                           ClassifierResults(baseline_dow$ds.info, 'DOW'),
                           ClassifierResults(baseline_iswd$ds.info, 'ISWD'))
ClassifierResultsToLatex(baseline_combined, paste(thesis.fig.dir, "baseline_performance.tex", sep=''))

# look at how final error varies according to the number of tokens
a2 = AnalyzeDatasets(dataset.info[dataset.info$time_encoding == 'dow' &
                                    dataset.info$classifier %in% baseline.classifiers,],
                     row.attribute.names=FALSE, keep.data=FALSE)

cairo_pdf(width=8, height=4, pointsize=10, filename=paste(thesis.fig.dir, "token_counts.pdf", sep=''))
ggplot(a2$ds.info, aes(x =token_count, y = accuracy, color = classifier, lty=classifier)) +
  scale_linetype_manual(values = c(rep("solid", 4), rep("dashed", 4), rep("dotted", 4))) +
  scale_color_manual(values = c(brewer.pal(4, "Set1"), brewer.pal(4, "Set1"), brewer.pal(4, "Set1"))) +
#  geom_line(aes(lty=classifier), lwd=1) +
  geom_line(lwd=0.6) +
#  geom_point() +
  theme_minimal() +
  guides(colour = guide_legend(ncol=3)) +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(1, "cm"),
        legend.text=element_text(size=9)) +
#  labs(title='Accuracy vs token count', subtitle=a1$common.title, y='Accuracy', x='Token count')
#  labs(title='', subtitle='', y='Accuracy', x='Token count')
  labs(y='Accuracy', x='Token count')
dev.off()

new_dow = AnalyzeDatasets(dataset.info[dataset.info$token_count == max(dataset.info$token_count) &
                                   dataset.info$time_encoding == 'dow' &
                                   dataset.info$classifier %in% new.classifiers,],
                    row.attribute.names=F)
new_dow$ds.info


new_iswd = AnalyzeDatasets(dataset.info[dataset.info$token_count == max(dataset.info$token_count) &
                                         dataset.info$time_encoding == 'iswd' &
                                         dataset.info$classifier %in% new.classifiers,],
                          row.attribute.names=F)
new_iswd$ds.info

new_combined <- ClassifierResults(new_dow$ds.info, 'DOW')
new_combined <- rbind(new_combined,
                           ClassifierResults(new_iswd$ds.info, 'ISWD'))
ClassifierResultsToLatex(new_combined, paste(thesis.fig.dir, "new_performance.tex", sep=''))

initial_count <- 25000

# combine performance stats for different time encodings
baseline_combined_initial <- rbind(ClassifierResults(AvgMeasures(baseline_none, maxlen=initial_count), ''),
                           ClassifierResults(AvgMeasures(baseline_dow, maxlen=initial_count), 'DOW'),
                           ClassifierResults(AvgMeasures(baseline_iswd, maxlen=initial_count), 'ISWD'))
ClassifierResultsToLatex(baseline_combined_initial, paste(thesis.fig.dir, "baseline_performance_initial.tex", sep=''))

new_combined_initial <- ClassifierResults(AvgMeasures(new_dow, maxlen=initial_count), 'DOW')
new_combined_initial <- rbind(new_combined_initial,
                      ClassifierResults(AvgMeasures(new_iswd, maxlen=initial_count), 'ISWD'))
ClassifierResultsToLatex(new_combined_initial, paste(thesis.fig.dir, "new_performance_initial.tex", sep=''))

new_comparison <- rbind(new_combined[!grepl('-MLB|-ED', new_combined$classifier),],
                        baseline_combined[baseline_combined$classifier == 'NaiveBayes',])
ClassifierResultsToLatex(new_comparison, paste(thesis.fig.dir, "new_comparison.tex", sep=''), baseline='NaiveBayes')

tan_comparison <- new_combined[grepl('-MLB|TDB$', new_combined$classifier) & !grepl('-ED', new_combined$classifier),]
ClassifierResultsToLatex(tan_comparison, paste(thesis.fig.dir, "tan_comparison.tex", sep=''), baseline='TDB')


best_comparison <- rbind(new_combined[!grepl('Tan', new_combined$classifier),],
                         baseline_combined[baseline_combined$classifier %in% c('NaiveBayes','HoeffdingTree'),])

decay_comparison <- new_combined[grepl('TDB$|-ED', new_combined$classifier) & !grepl('-MLB', new_combined$classifier),]
ClassifierResultsToLatex(decay_comparison, paste(thesis.fig.dir, "decay_comparison.tex", sep=''), baseline='TDB')

tan_ed_comparison <- new_combined[(new_combined$classifier == 'TDB') |
                                    (new_combined$classifier == 'TDB-ED,lambda=2.0') |
                                    (new_combined$classifier == 'TDB-MLB2,N0=1') |
                                    (new_combined$classifier == 'TDB-MLB2-ED,N0=1,lambda=2.0'),]
ClassifierResultsToLatex(tan_ed_comparison, paste(thesis.fig.dir, "tan_ed_comparison.tex", sep=''))

tan_dow_comparison <- AvgCumulativeMeasure(new_dow, 'accuracy')[, grep('-MLB', new_dow$ds.info$classifier, value=T)]
colnames(tan_dow_comparison) <- paste(colnames(tan_dow_comparison), '(DOW)')
tan_iswd_comparison <- AvgCumulativeMeasure(new_iswd, 'accuracy')[,  grep('-MLB', new_iswd$ds.info$classifier, value=T), drop=FALSE]
colnames(tan_iswd_comparison) <- paste(colnames(tan_iswd_comparison), '(ISWD)')

learning_rate_comparison <- cbind(tan_dow_comparison, tan_iswd_comparison)

m_lrc <- melt(learning_rate_comparison[1:100000,])
m_lrc <- m_lrc[m_lrc$Var1 %% 200 == 0,]
cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "learnrate_MLB.pdf", sep=''), fallback_resolution = 150)
ggplot(m_lrc, aes(x=Var1,y=value, color=Var2)) +
  geom_line(size=0.5) +
  theme_minimal() +
  ylim(0.7,0.85) +
  scale_x_continuous(labels=function(x) format(x, scientific=FALSE)) +
  guides(colour = guide_legend(ncol = 3)) +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
  labs(title='', y='Accuracy', x='Sample Number')
dev.off()

learning_rate_comparison <- cbind(AvgCumulativeMeasure(baseline_none, 'accuracy')[, c('HoeffdingTree', 'NaiveBayes')],
                                  AvgCumulativeMeasure(new_dow, 'accuracy')[, c('TDB', 'NB-T', 'TDB-MLB2,N0=1')],
                                  AvgCumulativeMeasure(new_iswd, 'accuracy')[, grep('-ED.*=2', new_iswd$ds.info$classifier, value = TRUE), drop=FALSE])

m_lrc <- melt(learning_rate_comparison)
m_lrc <- m_lrc[m_lrc$Var1 %% 50 == 0,]
m_lrc$Var2 <- paste(m_lrc$Var2, ifelse(m_lrc$Var2 %in% c('HoeffdingTree', 'NaiveBayes'), '(None)',
                                       ifelse(m_lrc$Var2 %in% c('TDB', 'NB-T', 'TDB-MLB2,N0=1'), '(DOW)', '(ISWD)')))
cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "learnrate.pdf", sep=''), fallback_resolution = 150)
ggplot(m_lrc, aes(x=Var1,y=value, color=Var2)) +
  geom_line(size=0.5) +
  theme_minimal() +
  ylim(0.7,0.85) +
  scale_x_continuous(labels=function(x) format(x, scientific=FALSE)) +
  guides(colour = guide_legend(ncol = 3)) +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
  labs(title='', y='Accuracy', x='Sample Number')
dev.off()

learning_rate_comparison <- cbind(AvgCumulativeMeasure(baseline_none, 'accuracy')[, c('HoeffdingTree'), drop=FALSE],
                                  AvgCumulativeMeasure(new_dow, 'accuracy')[, c('TDB', 'NB-T')],
                                  AvgCumulativeMeasure(new_iswd, 'accuracy')[, grep('-ED.*=2', new_iswd$ds.info$classifier, value = TRUE), drop=FALSE],
                                  AvgCumulativeMeasure(new_iswd, 'accuracy')[, c('TDB-MLB2,N0=1'), drop=FALSE])

m_lrc <- melt(learning_rate_comparison[1:25000,])
m_lrc <- m_lrc[m_lrc$Var1 %% 50 == 0,]
m_lrc$Var2 <- paste(m_lrc$Var2, ifelse(m_lrc$Var2 %in% c('HoeffdingTree', 'NaiveBayes'), '(None)',
                                       ifelse(m_lrc$Var2 %in% c('TDB', 'NB-T'), '(DOW)', '(ISWD)')))
cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "learnrate_25000.pdf", sep=''), fallback_resolution = 150)
ggplot(m_lrc, aes(x=Var1,y=value, color=Var2, lty=Var2)) +
  scale_linetype_manual(values = c(rep("solid", 4), rep("dashed", 4), rep("dotted", 4))) +
#  scale_color_manual(values = c(brewer.pal(4, "Set1"), brewer.pal(4, "Set1"), brewer.pal(4, "Set1"))) +
  geom_line(size=0.7) +
  theme_minimal() +
  ylim(0.7,0.85) +
  scale_x_continuous(labels=function(x) format(x, scientific=FALSE)) +
  guides(colour = guide_legend(ncol = 3)) +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
  labs(title='', y='Accuracy', x='Sample Number')
dev.off()

# look at how final error varies according to the number of tokens
decayset = AnalyzeDatasets(dataset.info[dataset.info$time_encoding == 'iswd' &
                                    dataset.info$classifier %in% new.classifiers & grepl('-ED|TDB$', dataset.info$classifier),],
                     row.attribute.names=FALSE, keep.data=FALSE)

cairo_pdf(width=8, height=4, pointsize=10, filename=paste(thesis.fig.dir, "token_counts2.pdf", sep=''))
ggplot(decayset$ds.info, aes(x =token_count, y = accuracy, color = classifier, lty=classifier)) +
  scale_linetype_manual(values = c(rep("solid", 8), rep("dashed", 8), rep("dotted", 8))) +
  scale_color_manual(values = c(brewer.pal(8, "Set1"), brewer.pal(8, "Set1"), brewer.pal(8, "Set1"))) +
  #  geom_line(aes(lty=classifier), lwd=1) +
  ylim(0.50,0.90) +
  xlim(0, 2000) +
  geom_line(size=0.7) +
  #  geom_point() +
  theme_minimal() +
  guides(colour = guide_legend(ncol=2)) +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(1, "cm"),
        legend.text=element_text(size=9)) +
  #  labs(title='Accuracy vs token count', subtitle=a1$common.title, y='Accuracy', x='Token count')
  #  labs(title='', subtitle='', y='Accuracy', x='Token count')
  labs(y='Accuracy', x='Token count')
dev.off()


for (classifier.number in seq.int(1, nrow(new_dow$ds.info))) {
  print(ggplot(melt(new_dow$label.measures$rmse[[classifier.number]][1:100000,]), aes(x=Var1, y=value, color=Var2)) +
    geom_line() +
    ylim(0,2) +
    scale_x_continuous(labels=function(x) format(x, scientific=FALSE)) +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
    labs(title=paste('RMSE over samples processed', new_dow$row.title[classifier.number]), subtitle=new_dow$common.title, y='RMSE', x='Sample number'))
}

cmplen=initial_count
snb.info <- CmpTestTrainError(dataset.info, training.info, 'dow', 'NB-T',
                         filename=paste(thesis.fig.dir, "nbdow_rmse.pdf", sep=''), article.dates$date, cmplen=cmplen)

ht.info = CmpTestTrainError(dataset.info, training.info, 'none', 'HoeffdingTree',
                            filename=paste(thesis.fig.dir, "ht_rmse.pdf", sep=''), article.dates$date, cmplen=cmplen)

bnt.info = CmpTestTrainError(dataset.info, training.info, 'dow', 'TDB',
                            filename=paste(thesis.fig.dir, "bnt_rmse.pdf", sep=''), article.dates$date, cmplen=cmplen)

nb.info = CmpTestTrainError(dataset.info, training.info, 'none', 'NaiveBayes',
                             filename=paste(thesis.fig.dir, "nb_rmse.pdf", sep=''), article.dates$date, cmplen=cmplen)

ed.info = CmpTestTrainError(dataset.info, training.info, 'iswd', 'TDB-ED,lambda=2.0',
                            filename=paste(thesis.fig.dir, "ed_rmse.pdf", sep=''), article.dates$date, cmplen=cmplen)

tan.info = CmpTestTrainError(dataset.info, training.info, 'iswd', 'TDB-MLB2,N0=1',
                            filename=paste(thesis.fig.dir, "tan_rmse.pdf", sep=''), article.dates$date, cmplen=cmplen)

tan_ed.info = CmpTestTrainError(dataset.info, training.info, 'iswd', 'TDB-MLB2-ED,N0=1,lambda=2.0',
                            filename=paste(thesis.fig.dir, "tan_ed_rmse.pdf", sep=''), article.dates$date, cmplen=cmplen)

test_train_cols = c('classifier', 'time_encoding', 'test_error', 'train_error')
test_train_cmp <- rbind(as.data.frame(snb.info[test_train_cols]), as.data.frame(ht.info[test_train_cols]),
                        as.data.frame(bnt.info[test_train_cols]), as.data.frame(nb.info[test_train_cols]),
                        as.data.frame(ed.info[test_train_cols]), as.data.frame(tan_ed.info[test_train_cols]))
test_train_cmp$time_encoding <- ifelse(test_train_cmp$time_encoding == 'none', '', toupper(as.character(test_train_cmp$time_encoding)))
test_train_cmp$classifier <- as.character(test_train_cmp$classifier)
colnames(test_train_cmp)[c(3,4)] <- c('Test RMSE', 'Train RMSE')
ClassifierResultsToLatex(test_train_cmp, paste(thesis.fig.dir, "final_rmse.tex", sep=''))

bias_variance_data <- data.frame(label=paste(test_train_cmp$classifier, ' (',
                                             ifelse(test_train_cmp$time_encoding == '','None',test_train_cmp$time_encoding),
                                             ')', sep=''),
                                 var=abs(test_train_cmp[,'Test RMSE'] - test_train_cmp[,'Train RMSE']),
                                 bias=test_train_cmp[,'Train RMSE'])

cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "bias_variance.pdf", sep=''))
p <- ggplot(bias_variance_data, aes(x=var, y=bias, label=label)) +
  theme_minimal() +
  geom_text_repel(fontface = 'bold',
                  box.padding = 0.35, point.padding = 0.5,
                  segment.color = 'grey50', segment.size = 0.5) +
  labs(title='', y='Training RMSE (Bias proxy)', x='|Training Error - Test Error| (Proxy for Variance)') +
  geom_point(color='red')
print(p)
dev.off()

cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "bnt_rmse_per_day60.pdf", sep=''))
rmse.per.day <- data.frame(rmse=bnt.info$test_rmse_per_day$group.measure[1:60,1])
rmse.per.day$x <- 1:nrow(rmse.per.day)
rmse.per.day.dow <- substr(strftime(as.Date(rownames(bnt.info$test_rmse_per_day$group.measure)[1:nrow(rmse.per.day)]), '%a'), 1, 1)
ggplot(rmse.per.day, aes(x=x, y=rmse)) +
  geom_line(size=1) +
  theme_minimal() +
  scale_x_continuous(breaks=rmse.per.day$x, labels = rmse.per.day.dow) +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
  labs(title='', y='RMSE per day', x='Article day of week')
dev.off()

tdb_tan_rmse_daily <- rbind(data.frame(rmse=bnt.info$test_rmse_per_day$group.measure[,1], classifier=bnt.info$classifier, date=as.Date(rownames(bnt.info$test_rmse_per_day$group.measure))),
                            data.frame(rmse=tan.info$test_rmse_per_day$group.measure[,1], classifier=tan.info$classifier, date=as.Date(rownames(bnt.info$test_rmse_per_day$group.measure))))
cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "tdb_tan_rmse_per_day.pdf", sep=''))
ggplot(tdb_tan_rmse_daily, aes(x=date,y=rmse,colour=classifier)) +
  geom_line(size=0) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
  labs(title='', y='RMSE per day', x='Article date')
dev.off()

tdb_tan_rmse_delta_daily <- data.frame(rmse_delta=tan.info$test_rmse_per_day$group.measure[,1]-bnt.info$test_rmse_per_day$group.measure[,1], date=as.Date(rownames(bnt.info$test_rmse_per_day$group.measure)))
cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "tdb_tan_rmse_delta_per_day.pdf", sep=''))
ggplot(tdb_tan_rmse_delta_daily, aes(x=date,y=rmse_delta)) +
  geom_line(size=0) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
  labs(title='', y='RMSE per day delta', x='Article date')
dev.off()


# all.dow=factor(weekdays(article.dates$date, abbreviate = T), levels=c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
#
# a5 = AnalyzeDatasets(dataset.info[dataset.info$token_count == max(dataset.info$token_count) &
#                                     dataset.info$time_encoding == 'iswd' &
#                                     dataset.info$classifier %in% new.classifiers,],
#                      row.attribute.names=FALSE)
# a5.group.info <- AvgGroupMeasure(a5, 'accuracy', groups = all.dow)
# cor(a5.group.info$group.measure, a5$group.counts$count)
#
# ggplot(melt(a5.group.info$group.measure), aes(x=Var1, y=value, fill=Var2)) +
#   geom_bar(stat='identity', position='dodge')

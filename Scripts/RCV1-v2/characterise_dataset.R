library(forecast)
library(ggplot2)
library(reshape2)
library(zoo)

#output.dir <-Sys.getenv('THESIS_OUTPUT_DIR')
#if (nchar(output.dir) == 0) {
  output.dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'
#}

cooked.data.dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs'

thesis.fig.dir <- 'C:/Projects/Trevor/Article Classification/data/Outputs/figures/'

article.dates <- read.csv(file = paste(cooked.data.dir, '/rcv1v2_article_date.csv', sep=''), colClasses = c("integer", "Date"))
article.topics <- read.csv(file = paste(cooked.data.dir, '/top-topics.csv', sep=''))



# merge date info with labels
# note that date info is for the full dataset (prior to the lewis cleanup
# the merge discards any that are not part of the revised dataset)
dataset <- merge(article.topics, article.dates)
print(paste('dataset nrow=', nrow(dataset)))

topics <- dataset
topics$itemid <- NULL
topics$date <- NULL   #Now 'topics' contains 4 columns for categories and 1,0 for relavance



# count the average number of labels per article
mean.labels.per.article <- sum(topics)/nrow(topics)  #sum(topics) adds all 1s in 'topics' which eventually gives total of assigned categories to all docs
cat(format(round(mean.labels.per.article, 2), nsmall = 2), file=paste(thesis.fig.dir, 'mean_labels_per_article.tex', sep=''))


# count the number of articles per day for each topic
daily.topic.counts <- aggregate(topics, list(date=dataset$date), sum)  #Give some of documents for each category for each date



#calculate proportion of each topic
daily.article.counts <- table(dataset$date)  #Give date and total of articles for that day
daily.topic.proportions <- sweep(daily.topic.counts[,2:5], 1, daily.article.counts, '/')
daily.topic.proportions$date <- daily.topic.counts$date



#Create pdf with a graph showing propotions of articles for each category for all months
cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "cat_proportions.pdf", sep=''))
ggplot(melt(daily.topic.proportions, id.vars='date'), aes(x=date, y=value, color=variable)) +
  geom_line(size=0) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
  labs(title='', y='Proportion of articles\nwith category', x='Article date')
dev.off()



cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "cat_proportions30.pdf", sep=''))
initial.daily.proportions <- daily.topic.proportions[1:30,]
initial.daily.proportions.dow <- substr(strftime(initial.daily.proportions$date, '%a'), 1, 1)
initial.daily.proportions$date <- 1:nrow(initial.daily.proportions)
ggplot(melt(initial.daily.proportions, id.vars='date'), aes(x=date, y=value, color=variable)) +
  geom_line(size=1) +
  theme_minimal() +
  scale_x_continuous(breaks=1:nrow(initial.daily.proportions), labels = initial.daily.proportions.dow) +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
  labs(title='', y='Proportion of articles\nwith category', x='Article day of week')
dev.off()



cairo_pdf(width=8, height=3, pointsize=8, filename=paste(thesis.fig.dir, "samples_per_day30.pdf", sep=''))
initial.daily.counts <- as.data.frame(daily.article.counts)[1:30,]
colnames(initial.daily.counts) <- c('date', 'count')
initial.daily.counts.dow <- substr(strftime(initial.daily.counts$date, '%a'), 1, 1)
initial.daily.counts$date <- 1:nrow(initial.daily.counts)
ggplot(melt(initial.daily.counts, id.vars='date'), aes(x=date, y=value, color=variable)) +
  geom_line(size=1) +
  theme_minimal() +
  scale_x_continuous(breaks=1:nrow(initial.daily.counts), labels = initial.daily.counts.dow) +
  theme(legend.title = element_blank(), legend.position='bottom', legend.key.width = unit(2, "cm")) +
  labs(title='', y='Count of articles', x='Article day of week')
dev.off()



#Drawing autocorrelations
for (cat in c('CCAT', 'GCAT', 'MCAT', 'ECAT')) {
  cairo_pdf(width=5, height=4, pointsize=10, filename=paste(thesis.fig.dir, cat, "_pacf.pdf", sep=''))
#  pdf(width=2, height=2, pointsize=8, file=paste(thesis.fig.dir, cat, "_pacf.pdf", sep=''))
  print(ggPacf(daily.topic.proportions[, cat], lag.max = 35, size=0) +
    theme_minimal() +
    scale_x_continuous(minor_breaks=NULL, breaks=c(5,10,15,20,25,30,35)) +
    scale_y_continuous(minor_breaks=NULL) +
    ylim(-1,1) +
    labs(title=''))
  dev.off()
}


cairo_pdf(width=5, height=4, pointsize=10, filename=paste(thesis.fig.dir, "CCAT_acf.pdf", sep=''))
print(ggAcf(daily.topic.proportions[, 'CCAT'], lag.max = 15, size=0) +
        theme_minimal() +
        scale_x_continuous(minor_breaks=NULL, breaks=c(5,10,15)) +
        scale_y_continuous(minor_breaks=NULL) +
        ylim(-1,1) +
        labs(title=''))
dev.off()



plot(daily.topic.proportions$CCAT, type='l')
plot(daily.topic.proportions$ECAT, type='l')
plot(daily.topic.proportions$GCAT, type='l')
plot(daily.topic.proportions$MCAT, type='l')


pacf(daily.topic.proportions$CCAT, lag.max = 100)
pacf(daily.topic.proportions$ECAT, lag.max = 100)
pacf(daily.topic.proportions$GCAT, lag.max = 100)
pacf(daily.topic.proportions$MCAT, lag.max = 100)


auto.arima(daily.topic.proportions$CCAT)
auto.arima(daily.topic.proportions$ECAT)
auto.arima(daily.topic.proportions$GCAT)
auto.arima(daily.topic.proportions$MCAT)


ccf(daily.topic.proportions$CCAT, daily.topic.proportions$ECAT, lag.max = 100)
corr.by.day = NULL
for (dt in unique(dataset$date)) {
  items <- dataset[dataset$date == dt, 2:5]
  print(paste('date:', as.Date(dt), 'item count:', nrow(items)))
  items.corr <- cor(items)
  items.cov <- cov(items)

  df <-data.frame(label=paste(rownames(items.corr)[row(items.corr)[upper.tri(items.corr)]],
                              colnames(items.corr)[col(items.corr)[upper.tri(items.corr)]],
                              sep=':'),
                  corr=items.corr[upper.tri(items.corr)],
                  dt=dt)

  if (is.null(corr.by.day)) {
    corr.by.day <- df
  } else {
    corr.by.day <- rbind(corr.by.day, df)
  }
}

# fisher transform of correlations to allow meaningful mean and intervals for the values
corr.by.day2 <- corr.by.day
corr.by.day2$corr <- 0.5 * log((1 + corr.by.day2$corr) / (1 - corr.by.day2$corr))


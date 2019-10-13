library(reshape)
library(corrplot)
library(ggplot2)
library(forecast)



topic.file <- 'C:/Projects/Trevor/Article Classification/data/rcv1-v2.topics.qrels'  #category and doc id for all docs in both train and test sets



# read topics from file
# format of line is <cat> <itemid> <value>
# <value> is always the constant 1
# read data
raw.topics <- read.delim(topic.file, header=FALSE, sep=' ',
                         colClasses=c('character', 'integer', 'integer'),
                         col.names = c('category', 'itemid', 'value'))

topic.counts <- aggregate(raw.topics$value, list(factor(raw.topics$category)), sum)  #get total docs for each category
colnames(topic.counts) <- c("category", "story.count")
rownames(topic.counts) <- NULL
topic.counts <- topic.counts[order(topic.counts$story.count, decreasing = TRUE),]   # category name and count of docs in descending order
topic.counts



# restrict the categories to the top level
top.topics <- raw.topics[grep('.CAT', raw.topics$category),]  #filter categories with only CAT at the end from row.topics file. Remove other categories
top.topics



# flatten topics into a single row per itemid
topics <- as.data.frame(cast(top.topics, itemid ~ category, value="value", fill=0, fun.aggregate = sum))
#print(paste('topics nrow=', nrow(topics)))   #total number of docs 804414
write.csv(topics, 'C:/Projects/Trevor/Article Classification/data/outputs/top-topics.csv', row.names = FALSE)   # doc id and 4 categories as columns / has 1 under a category column if doc belongs to that category or 0 else



num.slices <- 12
groups <- cut(1:nrow(topics), num.slices, labels=FALSE)   #804414 rows

for (slice in 1:num.slices) {   # Give corelation plot-------ex:how many have both mcat and ccat etc.
  corrplot(cor(topics[groups==slice, 2:5]), type="lower", order="alphabet", tl.col="black", diag = FALSE,
           mar=c(0,0,2,0), title=paste('slice', slice))
}




#article.date <- read.csv('article-age.csv')
article.date <- read.csv('C:/Projects/Trevor/Article Classification/data/outputs/rcv1v2_article_date.csv')

print(paste('article.date nrow=', nrow(article.date)))

topics2 <- merge(topics, article.date)   #Merge categories and dates for all doc ids
print(paste('topics2 nrow=', nrow(topics2)))

topics2$date <- as.Date(topics2$date)

start.date <- min(topics2$date)

topics2$day.num <- as.integer(topics2$date - start.date) + 1   #day number
topics2$dow <- as.factor(weekdays(topics2$date, abbreviate=TRUE))   #day of week such as mon, tue, wed etc.

write.csv(topics2, 'C:/Projects/Trevor/Article Classification/data/outputs/top-topics2.csv', row.names = FALSE)





# ------------------------------------------------------------------------------

corr.df <- NULL
marginal.df <- NULL

dow.names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

par(mfrow=c(2,4))
for (dow in 1:7) {   #corelation plt of each day of week
  dow.name <- dow.names[dow]

  items <- topics2[topics2$dow == dow.name, 2:5]
  print(paste(dow, str(items)))
  items.corr <- cor(items)

  # dow.df <-data.frame(row=rownames(m)[row(m)[upper.tri(m)]],
  #               col=colnames(m)[col(m)[upper.tri(m)]],
  #               corr=m[upper.tri(m)],
  #               dow=dow)
  dow.df <-data.frame(label=paste(rownames(items.corr)[row(items.corr)[upper.tri(items.corr)]], colnames(items.corr)[col(items.corr)[upper.tri(items.corr)]]),
                      corr=items.corr[upper.tri(items.corr)],
                      dow=dow)

  dow.marginal <- data.frame(matrix(1 - sapply(items, sum) / nrow(items), nrow=1, dimnames=list(NULL, colnames(items))))

  if (is.null(corr.df)) {
    corr.df <- dow.df
    marginal.df <- dow.marginal
  } else {
    corr.df <- rbind(corr.df, dow.df)
    marginal.df <- rbind(marginal.df, dow.marginal)
  }
  corrplot(cor(items), type="upper", order="alphabet", tl.col="black", diag = FALSE, title=dow.name, mar=c(0,0,2,0), tl.srt=45)
}



ggplot(corr.df, aes(x=dow, y=corr, col=label)) + geom_line(size=1) +
  scale_colour_discrete(name  ="Category combo") +
  scale_x_continuous(breaks=1:7, labels=dow.names) +
  ggtitle("Correlation between major categories") +
  xlab("Day of week") +
  ylab("correlation between major category labels")



window.size <- 1
corr.by.day = NULL
for (day.num in window.size:max(topics2$day.num)) {
  items <- topics2[(topics2$day.num > (day.num - window.size)) & (topics2$day.num <= day.num), 2:5]
  print(paste('day:', day.num, 'item count:', nrow(items)))
  items.corr <- cor(items)
  items.cov <- cov(items)

  df <-data.frame(label=paste(rownames(items.corr)[row(items.corr)[upper.tri(items.corr)]],
                              colnames(items.corr)[col(items.corr)[upper.tri(items.corr)]],
                              sep=':'),
                  corr=items.corr[upper.tri(items.corr)],
                  day.num=day.num)

  if (is.null(corr.by.day)) {
    corr.by.day <- df
  } else {
    corr.by.day <- rbind(corr.by.day, df)
  }
}



ggplot(corr.by.day, aes(x=day.num, y=corr, col=label)) + geom_line(size=1.5) +
  scale_colour_brewer(name  ="Category combo", palette = "Set2") +
  ggtitle("Correlation between major categories") +
  xlab("Day of news story") +
  ylab("correlation between major category labels")




# fisher transform of correlations to allow meaningful mean and intervals for the values
corr.by.day2 <- corr.by.day
corr.by.day2$corr <- 0.5 * log((1 + corr.by.day2$corr) / (1 - corr.by.day2$corr))

# partial autocorrelation function plots to see where there are periodic contributions to the data
par(mfrow=c(2,3))
for (var.combo in levels(corr.by.day2$label)) {
  print(var.combo)
  Pacf(corr.by.day2[as.character(corr.by.day2$label) == var.combo, "corr"],
       lag.max =45,
       main=paste("PACF for ", var.combo))
}

for (var.combo in levels(corr.by.day2$label)) {
  print(var.combo)
  Acf(corr.by.day2[as.character(corr.by.day2$label) == var.combo, "corr"],
       lag.max =45,
       main=paste("ACF for ", var.combo))
}



combo.counts <- aggregate(itemid ~ ., topics, FUN=length) #Give doc counts for each category combination -16 combinations for 4 categories
combo.counts$cat.count <- with(combo.counts, CCAT + ECAT + GCAT + MCAT)
combo.counts$item.count <- combo.counts$itemid
combo.counts$itemid <- NULL

combo.counts



# count how many stories have each count of categories
dc.per.lc <- aggregate(item.count ~ cat.count, combo.counts, sum) #eg  : how many docs have 1 category / how many docs ave 2 categories etc.
dc.per.lc



# get Label Cardinality - the average number of labels per document
LC <- sum(dc.per.lc$cat.count * dc.per.lc$item.count) / sum(dc.per.lc$item.count)

# get correlation between top categories over whole dataset
cat.corr <- cor(topics2[,2:5])

# estimate marginal probabilities of each category
cat.marginal <- 1 - sapply(topics2[,2:5], sum) / nrow(topics2)



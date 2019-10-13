library(ggplot2)
library(fpp2)
library(forecast)

#data <- read.csv("C:/Projects/NYTimes/outputs/seasonal_plots/accuracy plots/tc2000_985095_dow_b-AODE_B_T.csv")

full_data <- c("C:/Projects/NYTimes/outputs/seasonal_plots/accuracy plots/no_time-HoeffdingTree.csv",
               "C:/Projects/NYTimes/outputs/seasonal_plots/accuracy plots/time-HoeffdingTree.csv",
               "C:/Projects/NYTimes/outputs/seasonal_plots/accuracy plots/tc2000_985095_none_b-AODE.csv",
               "C:/Projects/NYTimes/outputs/seasonal_plots/accuracy plots/tc2000_985095_dow_b-AODE.csv",
               "C:/Projects/NYTimes/outputs/seasonal_plots/accuracy plots/tc2000_985095_dow_b-AODE_B_T.csv",
               "C:/Projects/NYTimes/outputs/seasonal_plots/accuracy plots/tc2000_985095_dow_b-AODE_T.csv")

names <- c("Hoeffding Tree without Time","Hoeffding Tree with Time","AODE without Time","AODE with Time","Multiple AODE Classifiers","SAODE")

days <- c(2,6,7,5,1,3,4)
day_names <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')

cols <- c("Classifier","Day","Accuracy")
classes <- c("character","character","integer")
tbl <- read.table(text = "", colClasses = classes, col.names = cols)


getAccuracyPercentage <- function(results){
  count <- 0
  for(i in 1:nrow(results)){
    #print(paste0(i," ",as.character(results[i,"Actual"])," ",as.character(results[i,"Predicted"])))
    if((!(is.na(as.character(results[i,"Actual"])))) && (!(is.na(as.character(results[i,"Predicted"]))))){
      if(as.character(results[i,"Actual"])==as.character(results[i,"Predicted"])){
        count <- count+1
      }
    }
  }
  count/nrow(results)
}


getAccuracy <- function(results){
  val <- 0
  for(doc in 1:nrow(results)){
    
    if((!(is.na(as.character(results[doc,"Actual"])))) && (!(is.na(as.character(results[doc,"Predicted"]))))){
      actual <- c()
      predicted <- c()
      for(i in 1:nchar(as.character(results[doc,"Actual"]))){
        actual[i] <- substr(as.character(results[doc,"Actual"]), i, i)
      }
      for(i in 1:nchar(as.character(results[doc,"Predicted"]))){
        predicted[i] <- substr(as.character(results[doc,"Predicted"]), i, i)
      }
      
      anp <- intersect(actual,predicted)
      aop <- length(union(actual,predicted))
      
      if(identical(anp,character(0))){
        anp <- 0
      }
      else{
        anp <- length(anp)
      }
      
      val <- val+(anp/aop)
    }
  }
  val/nrow(results)
}


count <- 1
for(data in 1:length(full_data)){
  current_data <- read.csv(full_data[data])
  #x <- dplyr::filter(current_data, grepl("O",Actual))
  #y <- dplyr::filter(current_data, grepl("RO",Actual))
  #p <- dplyr::filter(current_data, grepl("BO",Actual))
  #q <- dplyr::filter(current_data, grepl("AO",Actual))
  #current_data <- rbind(x,y,p,q)
  
  for(day in 1:length(days)){
    print(paste0(data," ",day))
    day_data <- current_data[(current_data$Time == days[day]),]
    tbl[count,"Classifier"] <- names[data]
    tbl[count,"Day"] <- as.character(day_names[day])
    tbl[count,"Accuracy"] <- getAccuracy(day_data)
    count <- count + 1
  } 
}


tbl

write.csv(tbl, paste('C:/Projects/NYTimes/outputs/seasonal_plots/accuracy plots/all_labels/opinion_results.csv', sep=''), row.names = FALSE)


day_order <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
test <- read.csv("C:/Projects/NYTimes/outputs/seasonal_plots/accuracy plots/all_labels/region_edited_results.csv")
ggplot(data=test, aes(x=factor(Day, level = day_order), y=Accuracy, group=Classifier)) +
  geom_line(aes(color=Classifier),size=2)+
  geom_point(aes(color=Classifier))+
  theme(axis.text=element_text(size=23), axis.title=element_text(size=24,face="bold"))+
  xlab("Day of Week") + 
  ylab("MLA") 
 
  
  

  




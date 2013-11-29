#This is a cool demo that shows the potential of connecting with Google Visualizations
install.packages("googleVis")
library(googleVis)
demo(WorldBank)



#Start of Kaggle See Click Predict 
setwd("C:/Users/zlxstc/Desktop/Business Analytics/SeeClickPredictFix")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
str(train)
head(train)

#We know there are 4 Cities
geo <- train[,2:3]
set.seed(2)
fit <- kmeans(geo$latitude, 4) # 4 cluster solution

#Make sure clustering worked, should show 4 distinct areas You might have to run again if dots not colored right
plot(geo$latitude, geo$longitude, col = fit$cluster)

#caputure cluster assignment
citycl<-fit$cluster
geob<-cbind(geo,citycl)
cities<-aggregate(geob, by=list(geob$citycl), FUN=mean)
train<-cbind(train, citycl)
View(cities)
#test------------------------------------------------------------------
geo2 <- test[,2:3]
set.seed(2)
fit2 <- kmeans(geo2, 4,nstart=4) # 4 cluster solution

#Make sure clustering worked, should show 4 distinct areas You might have to run again if dots not colored right
plot(geo2$latitude, geo2$longitude, col = fit2$cluster)

#caputure cluster assignment
citycl<-fit2$cluster
geob2<-cbind(geo2,citycl)
cities<-aggregate(geob2, by=list(geob2$citycl), FUN=mean)

#Once you have the average long/Latitude for each cluster you can find city online. 
#1  41.85662	-87.68507	   1 Chicago, IL
#2	2	37.54046	-77.46269	 2 Richmond, VA
#3	3	41.31132	-72.92412	 3 New Haven, CT
#4	4	37.80239	-122.24116 4 Oakland, CA

#You might need to change the ordering
levels(citycl) = c('Chicago','Richmond','New Haven', 'Oakland')

#Let's add our new feature back to the training dataset
train<-cbind(train, citycl)
test<-cbind(test, citycl)
#Intro to text data analysis

#Basic Text Data Features
train$summarync<-nchar(as.character(train$summary))
test$summarync<-nchar(as.character(test$summary))
train$descriptionnc<-nchar(as.character(train$description))
test$descriptionnc<-nchar(as.character(test$description))

#Currently variables are treated as factors, and they have repeating digits.
#install.packages("tm")
library(tm)
myCorpus <- Corpus(VectorSource(train$summary))
myCorpus2 <- Corpus(VectorSource(train$description))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus2 <- tm_map(myCorpus2, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus2 <- tm_map(myCorpus2, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus2 <- tm_map(myCorpus2, removeNumbers)
#summary(myCorpus)
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 3))
myDtm2 <- TermDocumentMatrix(myCorpus2, control = list(minWordLength = 3))

myCorpus3 <- Corpus(VectorSource(test$summary))
myCorpus4 <- Corpus(VectorSource(test$description))
myCorpus3 <- tm_map(myCorpus3, tolower)
myCorpus4 <- tm_map(myCorpus4, tolower)
myCorpus3 <- tm_map(myCorpus3, removePunctuation)
myCorpus4 <- tm_map(myCorpus4, removePunctuation)
myCorpus3 <- tm_map(myCorpus3, removeNumbers)
myCorpus4 <- tm_map(myCorpus4, removeNumbers)
#summary(myCorpus)
myDtm3 <- TermDocumentMatrix(myCorpus3, control = list(minWordLength = 3))
myDtm4 <- TermDocumentMatrix(myCorpus4, control = list(minWordLength = 3))










#install.packages("wordcloud")
library(wordcloud)
m <- as.matrix(myDtm)
> # calculate the frequency of words
  v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
k <- which(names(v)=="miners")
myNames[k] <- "mining"
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=15)

#This provides a list of frequently used terms. 
freqTerms <-findFreqTerms(myDtm, lowfreq=30)
freqTerms2 <-findFreqTerms(myDtm2, lowfreq=30)

freqTerms3 <-findFreqTerms(myDtm3, lowfreq=30)
freqTerms4 <-findFreqTerms(myDtm4, lowfreq=30)

#This will get rid of some irrelevant terms
myStopwords <- stopwords('english')
idx <- which(myStopwords == "r")
#myStopwords <- myStopwords[-idx]
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus2 <- tm_map(myCorpus2, removeWords, myStopwords)
freqTerms <- tm_map(freqTerms, removeWords, myStopwords)
freqTerms <- factor(freqTerms)
freqTerms2 <- factor(freqTerms2)
myStopwords <- factor(myStopwords)
usefulfreqTerms <- setdiff(freqTerms,myStopwords)
usefulfreqTerms2 <- setdiff(freqTerms2,myStopwords)
#test data
myCorpus3 <- tm_map(myCorpus3, removeWords, myStopwords)
myCorpus4 <- tm_map(myCorpus4, removeWords, myStopwords)
freqTerms3 <- tm_map(freqTerms3, removeWords, myStopwords)
freqTerms3 <- factor(freqTerms3)
freqTerms4 <- factor(freqTerms4)
#myStopwords <- factor(myStopwords)
usefulfreqTerms3 <- setdiff(freqTerms3,myStopwords)
usefulfreqTerms4 <- setdiff(freqTerms4,myStopwords)


summary1<- summary(train$summary)
summary<- summary(train$tag_type)
#This will create a feature based on one item
#train$s_illeg <- grepl("graffiti", train$summary, ignore.case=TRUE)

train$s_freqw <- grepl(paste(usefulfreqTerms, collapse='|'), train$summary, ignore.case=TRUE)
train$s_freqw_desc <- grepl(paste(usefulfreqTerms2, collapse='|'), train$description, ignore.case=TRUE)
testing<-data.frame(train)

count_word <- function(text, words){  
  count<-array()
  for (i in 1:length(text)){
    #typeof(words)
    #aaa<-as.character(text)[i]
    myCorpus <- Corpus(VectorSource((text)[i]))
    myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 3))
    freqTerms <-findFreqTerms(myDtm, lowfreq=1)
    count[i]<-length(intersect(freqTerms,words))
  }
  return(count)
}
NA_converter <- function(data, text, NA_remove){
  summary<- summary(data[NA_remove])
  #return (summary)  
  for (i in 1:length(data$text)){
    if (((data$NA_remove)[i])=="other") {
      myCorpus <- Corpus(VectorSource((data$text)[i]))
      myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 3))
      freqTerms <-findFreqTerms(myDtm, lowfreq=1)
      data[NA_remove][i]<-intersect(freqTerms,summary)[0]
    }
  }  
}
#test$time_diff2<- NA
train$tag_type<-ifelse(train$tag_type=="other",NA,train$tag_type)
train$tag_type[is.na(train$tag_type)] <- "other"
test$tag_type<-factor(test$tag_type)
test$tag_type[is.na(test$tag_type)] <- "others"
NA_converter(train[1:14,], summary, "tag_type")
train$freq_num <-count_word(as.character(train$summary),usefulfreqTerms)
train$freq_num_desc<-count_word(as.character(train$description),usefulfreqTerms2)

test$freq_num <-count_word(as.character(test$summary),usefulfreqTerms3)
test$freq_num_desc<-count_word(as.character(test$description),usefulfreqTerms4)

#This will create a feature based on an array of related terms.
illegal<-c("graffiti", "illegal", "drug","bad","prostitution","robbery","roadkill" )
train$s_illeg <- grepl(paste(illegal, collapse='|'), train$summary, ignore.case=TRUE)
train$s_illeg_desc <- grepl(paste(illegal, collapse='|'), train$description, ignore.case=TRUE)
#######
train$freq_num_illegal <-count_word(as.character(train$summary),illegal)
train$freq_num_desc_illegal<-count_word(as.character(train$description),illegal)

test$freq_num_illegal <-count_word(as.character(test$summary),illegal)
test$freq_num_desc_illegal<-count_word(as.character(test$description),illegal)

train$freq_num_trash <-count_word(as.character(train$summary),trash)
train$freq_num_desc_trash<-count_word(as.character(train$description),trash)

test$freq_num_trash <-count_word(as.character(test$summary),trash)
test$freq_num_desc_trash<-count_word(as.character(test$description),trash)

train$freq_num_infra <-count_word(as.character(train$summary),infrastructure)
train$freq_num_desc_infra<-count_word(as.character(train$description),infrastructure)

test$freq_num_infra <-count_word(as.character(test$summary),infrastructure)
test$freq_num_desc_infra<-count_word(as.character(test$description),infrastructure)
#######
trash<-c("dump", "abandon", "trash", "pickup","recycling","pile", "refuse","bulk")
train$s_trash <- grepl(paste(trash, collapse='|'), train$summary, ignore.case=TRUE)
train$s_trash_desc <- grepl(paste(trash, collapse='|'), train$description, ignore.case=TRUE)

infrastructure <- c("bench", "bridge", "walk", "drain","flood","heat", "homeless","hydrant","parking","sign","light","pothole")
train$s_infra <- grepl(paste(infrastructure, collapse='|'), train$summary, ignore.case=TRUE)
train$s_infra_desc <- grepl(paste(infrastructure, collapse='|'), train$description, ignore.case=TRUE)

train$source_numeric<-ifelse(train$source=="android",1,train$source)
train$missing.source <- is.na(train$source_numeric)

#train$source_numeric<-ifelse(train$source=="android",1,train$source)
train$source_numeric[is.na(train$source_numeric)] <- 0 

train$tag_numeric<-ifelse(train$tag_type=="abandoned_vehicle",1,train$tag_type)
train$tag_numeric[is.na(train$tag_numeric)] <- 0 
#test__________________________________________________________________________________________
test$s_freqw <- grepl(paste(usefulfreqTerms3, collapse='|'), test$summary, ignore.case=TRUE)
test$s_freqw_desc <- grepl(paste(usefulfreqTerms4, collapse='|'), test$description, ignore.case=TRUE)

#This will create a feature based on an array of related terms.
illegal<-c("graffiti", "illegal", "drug","bad","prostitution","robbery","roadkill" )
test$s_illeg <- grepl(paste(illegal, collapse='|'), test$summary, ignore.case=TRUE)
test$s_illeg_desc <- grepl(paste(illegal, collapse='|'), test$description, ignore.case=TRUE)

trash<-c("dump", "abandon", "trash", "pickup","recycling","pile", "refuse","bulk")
test$s_trash <- grepl(paste(trash, collapse='|'), test$summary, ignore.case=TRUE)
test$s_trash_desc <- grepl(paste(trash, collapse='|'), test$description, ignore.case=TRUE)

infrastructure <- c("bench", "bridge", "walk", "drain","flood","heat", "homeless","hydrant","parking","sign","light","pothole")
test$s_infra <- grepl(paste(infrastructure, collapse='|'), test$summary, ignore.case=TRUE)
test$s_infra_desc <- grepl(paste(infrastructure, collapse='|'), test$description, ignore.case=TRUE)

test$source_numeric<-ifelse(test$source=="android",1,test$source)
test$missing.source <- is.na(test$source_numeric)

#test$source_numeric<-ifelse(test$source=="android",1,test$source)
test$source_numeric[is.na(test$source_numeric)] <- 0 
test$tag_numeric<-ifelse(test$tag_type=="abandoned_vehicle",1,test$tag_type)
test$tag_numeric[is.na(test$tag_numeric)] <- 0 
############
origin_time<-strptime("2012-01-01 00:00:00",format = "%Y-%m-%d %H:%M:%S")
time_char<-as.character((train$created_time))
#Then to POSIX (time)
extracted_time<-strptime(time_char, format = "%Y-%m-%d %H:%M:%S")
train$time_diff<-data.frame(difftime(extracted_time,origin_time))
my_month<-data.frame(
  date=extracted_time,
  time_diff=as.POSIXct(format(extracted_time, "%Y-%m-%d"))-origin_time)


train$month<-as.integer(my_month$time_diff/24)


time_char2<-as.character((test$created_time))
#Then to POSIX (time)
extracted_time2<-strptime(time_char2, format = "%Y-%m-%d %H:%M:%S")
test$time_diff2<-data.frame(difftime(extracted_time2,origin_time))
my_month2<-data.frame(
  date=extracted_time2,
  time_diff=as.POSIXct(format(extracted_time2, "%Y-%m-%d"))-origin_time)

test$month<-as.integer(my_month2$time_diff/24)
#install.packages("neuralnet")
m1 <- model.matrix( ~latitude+longitude+summarync+descriptionnc+source_numeric+freq_num+freq_num_desc+tag_numeric+time_diff+num_views, data = train)
m2 <- model.matrix( ~latitude+longitude+summarync+descriptionnc+source_numeric+freq_num+freq_num_desc+tag_numeric+time_diff+num_votes, data = train)
m3 <- model.matrix( ~latitude+longitude+summarync+descriptionnc+source_numeric+freq_num+freq_num_desc+tag_numeric+time_diff+num_comments, data = train)
n <- model.matrix( ~latitude+longitude+summarync+descriptionnc+source_numeric+freq_num+freq_num_desc+freq_num_illegal+freq_num_desc_illegal+tag_numeric+time_diff2, data = test)
m1<-data.frame(m1)
m2<-data.frame(m2)
m3<-data.frame(m3)
n<-data.frame(n)
#m<-m[c(3,4,5,6,7,8,9,10,11,12,13,14,15,2)]
#n<-n[c(2:14)]
write.csv(m1, 'm1.csv')
write.csv(m2, 'm2.csv')
write.csv(m3, 'm3.csv')
write.csv(n, 'n.csv')
write.csv(train, 'mm.csv')
summary(n)
#n2<- n[,2:14]

library("nnet")
train.nn2 <- nnet(num_votes~citycl+summarync+descriptionnc+s_illegTRUE+s_illeg_descTRUE+s_trashTRUE+s_trash_descTRUE+s_infraTRUE+s_infra_descTRUE+source_numeric+missing.sourceTRUE+s_freqwTRUE+s_freqw_descTRUE, data = m, size = 15, rang = 0.1,
                  decay = 5e-4, maxit = 500)
predict(train.nn2, n )

library("e1071")
model<- svm(num_votes~citycl+summarync+descriptionnc+s_illegTRUE+s_illeg_descTRUE+s_trashTRUE+s_trash_descTRUE+s_infraTRUE+s_infra_descTRUE+source_numeric+missing.sourceTRUE+s_freqwTRUE+s_freqw_descTRUE, data = m)

library("neuralnet")
train.nn2 <- neuralnet(num_votes~citycl+summarync+descriptionnc+s_illegTRUE+s_illeg_descTRUE+s_trashTRUE+s_trash_descTRUE+s_infraTRUE+s_infra_descTRUE+source_numeric+missing.sourceTRUE+s_freqwTRUE+s_freqw_descTRUE, data = m, hidden = 23,threshold=0.01)

result<-compute(train.nn2, n)
write.csv(result, 'predict.csv')
table(test$num_votes, )


library("randomForest")
train.rf <- randomForest(num_votes~citycl+summarync+descriptionnc+s_illeg+s_illeg_desc+s_trash+s_trash_desc+s_infra+s_infra_desc+source_numeric+missing.source+s_freqw+s_freqw_desc, data=train)

pred <- predict(train.rf, test)
#pred_b <- ifelse(pred > 0.5, 1, 0)
write.csv(pred, 'predict.csv')
#train$source_numeric<-0

z <- strptime(train$created_time, "%H:%M:%S")
time=as.POSIXct(as.numeric(train$Created_Time),origin = "1582-10-14", format="%Y-%m-%d %H:%M:%S")
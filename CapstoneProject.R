library(quanteda)
library(SnowballC)
library(tm)
library(quanteda.textstats)
library(dplyr)
library(data.table)
library(tidyr)


blogs <- readLines("C:/MyFolders/Coursera/Capstone Project/final/en_US/en_US.blogs.txt",encoding = "UTF-8", skipNul = TRUE)
news <- readLines("C:/MyFolders/Coursera/Capstone Project/final/en_US/en_US.news.txt",encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
twitter <- readLines("C:/MyFolders/Coursera/Capstone Project/final/en_US/en_US.twitter.txt",encoding = "UTF-8", skipNul = TRUE)


sampleBlogs <- blogs[rbinom(length(blogs)*.01, length(blogs), .5)]
sampleNews <- news[rbinom(length(news)*.01, length(news), .5)]
sampleTwitter <- twitter[rbinom(length(twitter)*.01, length(twitter), .5)]

set.seed(1234)
trainingDataSet <- c(sampleBlogs,sampleNews,sampleTwitter)
TrainingDataCorpus <- Corpus(VectorSource(trainingDataSet), readerControl = list(reader=readPlain,language="en"))

TrainingDataCorpus <- corpus(TrainingDataCorpus)

save(TrainingDataCorpus, file = "C:/MyFolders/Coursera/Capstone Project/TrainingDataCorpus.RData")

mytokens <- tokens(TrainingDataCorpus) %>%
        tokens(remove_punct=TRUE,remove_numbers=TRUE,remove_separators=TRUE) %>%
        tokens_remove(pattern=letters) %>%
        tokens_remove(stopwords("english")) %>%
        tokens_wordstem()
save(mytokens, file = "C:/MyFolders/Coursera/Capstone Project/mytokens.RData")
# release memory
TrainingDataCorpus = NULL
gc() # clean gabage
# the following code will generate ngrams
unigram <- tokens_ngrams(mytokens,n=1) 
save(unigram, file = "C:/MyFolders/Coursera/Capstone Project/unigram.RData")
bigram <- tokens_ngrams(mytokens,n=2)
save(bigram, file = "C:/MyFolders/Coursera/Capstone Project/bigram.RData")
trigram <- tokens_ngrams(mytokens,n=3)
save(trigram, file = "C:/MyFolders/Coursera/Capstone Project/trigram.RData")
fourgram <- tokens_ngrams(mytokens,n=4)
save(fourgram, file = "C:/MyFolders/Coursera/Capstone Project/fourgram.RData")
fivegram <- tokens_ngrams(mytokens,n=5)
save(fivegram, file = "C:/MyFolders/Coursera/Capstone Project/fivegram.RData")


mytokens = NULL
gc() 



uni.dfm <- dfm(unigram)
save(uni.dfm, file = "C:/MyFolders/Coursera/Capstone Project/uni.dfm.RData")
#load("unigram.RData")
uni.freq <- textstat_frequency(uni.dfm)
save(uni.freq, file = "C:/MyFolders/Coursera/Capstone Project/uni.freq.RData")
uni.freq2 <- uni.freq[uni.freq$frequency>1,na.rm=TRUE,1:2]
save(uni.freq2, file = "C:/MyFolders/Coursera/Capstone Project/uni.freq2.RData")



bi.dfm <- dfm(bigram)
save(bi.dfm, file = "C:/MyFolders/Coursera/Capstone Project/bi.dfm.RData")
bi.freq <- textstat_frequency(bi.dfm)
save(bi.freq, file = "C:/MyFolders/Coursera/Capstone Project/bi.freq.RData")
bi.freq2 <- bi.freq[bi.freq$frequency>1,na.rm=TRUE,1:2]
save(bi.freq2, file = "C:/MyFolders/Coursera/Capstone Project/bi.freq2.RData")
bi.prob2 <- bi.freq2[order(bi.freq2$feature,-bi.freq2$frequency),] %>% 
        mutate(prob=(frequency/sum(frequency)),na.rm=TRUE)
bi.prob2 <- bi.prob2[,c(1,3)]
save(bi.prob2, file = "C:/MyFolders/Coursera/Capstone Project/bi.prob2.RData")
bi.prob2.dt <- as.data.table(bi.prob2)
save(bi.prob2.dt, file = "C:/MyFolders/Coursera/Capstone Project/bi.prob2.dt.RData")
bi.lookup2 <- bi.prob2.dt %>% 
        separate(feature,c("feature","suggest"),"\\_(?!.*_)",extra="merge",fill="left") %>%
        setkey(feature)
save(bi.lookup2, file = "C:/MyFolders/Coursera/Capstone Project/bi.lookup2.RData")



tri.dfm <- dfm(trigram)
save(tri.dfm, file = "C:/MyFolders/Coursera/Capstone Project/tri.dfm.RData")
tri.freq <- textstat_frequency(tri.dfm)
save(tri.freq, file = "C:/MyFolders/Coursera/Capstone Project/tri.freq.RData")
tri.freq2 <- tri.freq[tri.freq$frequency>1,na.rm=TRUE,1:2]
save(tri.freq2, file = "C:/MyFolders/Coursera/Capstone Project/tri.freq2.RData")
tri.prob2 <- tri.freq2[order(tri.freq2$feature,-tri.freq2$frequency),] %>% 
        mutate(prob=(frequency/sum(frequency)),na.rm=TRUE)
tri.prob2 <- tri.prob2[,c(1,3)]
save(tri.prob2, file = "C:/MyFolders/Coursera/Capstone Project/tri.prob2.RData")
tri.prob2.dt <- as.data.table(tri.prob2,key="feature")
save(tri.prob2.dt, file = "C:/MyFolders/Coursera/Capstone Project/tri.prob2.dt.RData")
# use negative lookahead to split feature
tri.lookup2 <- tri.prob2.dt %>%
        separate(feature,c("feature","suggest"),"\\_(?!.*_)",extra="merge",fill="left") %>%
        setkey(feature)
save(tri.lookup2, file = "C:/MyFolders/Coursera/Capstone Project/tri.lookup2.RData")



four.dfm <- dfm(fourgram)
save(four.dfm, file = "C:/MyFolders/Coursera/Capstone Project/four.dfm.RData")
four.freq <- textstat_frequency(four.dfm)
save(four.freq, file = "C:/MyFolders/Coursera/Capstone Project/four.freq.RData")
four.freq2 <- four.freq[four.freq$frequency>1,na.rm=TRUE,1:2]
save(four.freq2, file = "C:/MyFolders/Coursera/Capstone Project/four.freq2.RData")
four.prob2 <- four.freq2[order(four.freq2$feature,-four.freq2$frequency),] %>% 
        mutate(prob=(frequency/sum(frequency)),na.rm=TRUE)
four.prob2 <- four.prob2[,c(1,3)]
save(four.prob2, file = "C:/MyFolders/Coursera/Capstone Project/four.prob2.RData")
four.prob2.dt <- as.data.table(four.prob2,key="feature")
save(four.prob2.dt, file = "C:/MyFolders/Coursera/Capstone Project/four.prob2.dt.RData")
four.lookup2 <- four.prob2.dt %>%
        separate(feature,c("feature","suggest"),"\\_(?!.*_)",extra="merge",fill="left") %>%
        setkey(feature)
save(four.lookup2, file = "C:/MyFolders/Coursera/Capstone Project/four.lookup2.RData")



five.dfm <- dfm(fourgram)
save(five.dfm, file = "C:/MyFolders/Coursera/Capstone Project/five.dfm.RData")
five.freq <- textstat_frequency(five.dfm)
save(five.freq, file = "C:/MyFolders/Coursera/Capstone Project/five.freq.RData")
five.freq2 <- five.freq[five.freq$frequency>1,na.rm=TRUE,1:2]
save(five.freq2, file = "C:/MyFolders/Coursera/Capstone Project/five.freq2.RData")



## Next Word Predictor function wrote in server.R file


#load("C:/MyFolders/Coursera/Capstone Project/five.freq2.RData")
#load("C:/MyFolders/Coursera/Capstone Project/four.freq2.RData")
#load("C:/MyFolders/Coursera/Capstone Project/tri.freq2.RData")
#load("C:/MyFolders/Coursera/Capstone Project/bi.freq2.RData")


# getlast2words <- function(str){
# 
# 
# 
# pred1 <- NULL
# pred2 <- NULL
# pred3 <- NULL
# pred4 <-NULL
# 
# 
# strText <- corpus(str)
# 
# mytokensStr <- tokens(strText) %>%
# tokens(remove_punct=TRUE,remove_numbers=TRUE,remove_separators=TRUE) %>%
# tokens_remove(pattern=letters) %>%
# tokens_remove(stopwords("english")) %>%
# tokens_wordstem()
#         
# 
# strText.unigram <- tokens_ngrams(mytokensStr,n=1) 
# strText.bigram <- tokens_ngrams(mytokensStr,n=2)
# strText.trigram <- tokens_ngrams(mytokensStr,n=3)
# strText.fourgram <- tokens_ngrams(mytokensStr,n=4)
# 
#    
#         
# inquiry4 <- tail(as.character(strText.fourgram),1)
# 
# if (length(inquiry4)!=0){
# 
# match4 <- five.freq2[grep(paste0("^",inquiry4),five.freq2$feature)]
# #match4.prob <- na.omit(match4) %>% mutate(frequency=(frequency/sum(frequency)))
# pred4 <- head(match4,10)
# 
# }else{pred4 <- NULL}
# 
# 
# inquiry3 <- tail(as.character(strText.trigram),1)
# if (length(inquiry3)!=0){
# 
# match3 <- four.freq2[grep(paste0("^",inquiry3),four.freq2$feature)]
# #match3.prob <- na.omit(match3) %>% mutate(frequency=(frequency/sum(frequency)))
# pred3 <- head(match3,10)
# 
# }else{pred3 <- NULL}
# 
# 
# inquiry2 <- tail(as.character(strText.bigram),1)
# if (length(inquiry2)!=0){
# match2 <- tri.freq2[grep(paste0("^",inquiry2),tri.freq2$feature),1:2]
# match2.prob <- na.omit(match2) %>% mutate(frequency=(frequency/sum(frequency))*0.4)
# pred2 <- head(match2.prob,10)
# }else{pred2 <- NULL}
# 
# inquiry1 <- tail(as.character(strText.unigram),1)
# if (length(inquiry1)!=0){
# 
# match1 <- bi.freq2[grep(paste0("^",inquiry1),bi.freq2$feature),1:2]
# match1.prob <- na.omit(match1) %>% mutate(frequency=(frequency/sum(frequency))*0.4^3)
# pred1 <- head(match1.prob,10)
# }else{pred1 <- NULL}
# 
# pred.all <- rbind(pred4, pred3, pred2, pred1)
# pred.top20 <- head(arrange(pred.all,desc(frequency)),20)
# pred.split <- strsplit(pred.top20$feature, "\\_")
#         
# n = 1:length(pred.split)
# predict = NULL
# for (i in n) {
#         predict <- c(predict, tail(pred.split[[i]],1))
# }
# predict10 <- head(unique(predict),10)
# return(predict10)
# }
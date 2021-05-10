rm(list = ls())
setwd("/home/stathis/Desktop/Scripts/")
require("readr")
require("dplyr")
require("stringr")
require("stringi")
source("./data.preparation.functions.R")
#preparation of training, test data 


#import of training data
training.data <- read.csv("./training.dataset.9.may.1558.csv")
names(training.data)[2] <- c("Text")
names(training.data)[3] <- c("ambiguity")
#basic NLP
training.data$Text <- gsub('http.*\\s*', '', training.data$Text)
training.data$Text <- as.character(training.data$Text)
training.data$Text <- tolower(training.data$Text)
training.data <- subset(training.data, training.data$Text != "")

#positive training and test data
training.data.positive <- subset(training.data, training.data$ambiguity == 2)
training.data.positive <- training.data.positive[1:1428, ]
training.data.positive <- as.character(training.data.positive$Text)
train.pos <- training.data.positive[1:714]
test.pos <- training.data.positive[715:1428]


#negative training and test data
training.data.negative <- subset(training.data, training.data$ambiguity == 1)
training.data.negative <- training.data.negative[1:556, ]
training.data.negative <- as.character(training.data.negative$Text)
train.neg <- training.data.negative[1:278]
test.neg <- training.data.negative[279:556]

#creation of vectors
#train positive
for (i in 1:NROW(train.pos)){
  path = paste0("./rnn.model/train/pos/", i,".txt")
  text <- train.pos[i]
  sink(path)
  cat(as.character(train.pos[i]))
  sink()
}

#train negative
for (i in 1:NROW(train.neg)){
  path = paste0("./rnn.model/train/neg/", i,".txt")
  text <- train.neg[i]
  sink(path)
  cat(as.character(train.neg[i]))
  sink()
}


#test positive
for (i in 1:NROW(test.pos)){
  path = paste0("./rnn.model/test/pos/", i,".txt")
  text <- test.pos[i]
  sink(path)
  cat(as.character(test.pos[i]))
  sink()
}

#test negative
for (i in 1:NROW(test.neg)){
  path = paste0("./rnn.model/test/neg/", i,".txt")
  text <- test.neg[i]
  sink(path)
  cat(as.character(test.neg[i]))
  sink()
}


#list vectors
negative_train_list <- list.files("./rnn.model/train/neg", full.names = T)
positive_train_list <- list.files("./rnn.model/train/pos", full.names = T)


negative_test_list <- list.files("./rnn.model/test/neg", full.names = T)
positive_test_list <- list.files("./rnn.model/test/pos", full.names = T)

#read vectors
negative_train_raw <- file_import(negative_train_list)
positive_train_raw <- file_import(positive_train_list)
negative_test_raw <- file_import(negative_test_list)
positive_test_raw <- file_import(positive_test_list)

#creation of train vector, test vector
train_raw <- c(negative_train_raw, positive_train_raw)
test_raw <- c(negative_test_raw, positive_test_raw)


#creation of procesed traincorpus
corpus_preprocessed_train <- text_pre_process(corpus = train_raw, count_threshold = 10, 
                                              dic = NULL)

#creation of processed test corpus
corpus_preprocessed_test <- text_pre_process(corpus = test_raw, dic = corpus_preprocessed_train$dic)

seq_length_dist <- unlist(lapply(corpus_preprocessed_train$word_vec_list, length))
quantile(seq_length_dist, 0:20/20)

#bucket data, train
corpus_bucketed_train <- make_bucket_data(word_vec_list = corpus_preprocessed_train$word_vec_list, 
                                          labels = rep(0:1, c(278,714)), 
                                          dic = corpus_preprocessed_train$dic, 
                                          seq_len = c(5,10,20), 
                                          right_pad = T)

#bucket data, test
corpus_bucketed_test <- make_bucket_data(word_vec_list = corpus_preprocessed_test$word_vec_list, 
                                         labels = rep(0:1, c(278,714)), 
                                         dic = corpus_preprocessed_test$dic, 
                                         seq_len = c(5,10,20), 
                                         right_pad = T)

#export of corpus
saveRDS(corpus_bucketed_train, file = "./rnn.model/corpus_bucketed_train.improved_9May.1726.rds")
saveRDS(corpus_bucketed_test, file = "./rnn.model/corpus_bucketed_test.improved_9May.1726.rds")




############
#import of data to be predicted
data = read.csv("/home/stathis/Desktop/SeismicIntensityArticle-master/tweets.initial.classification.csv")
data$Text = as.character(data$Text)
for (i in 1:NROW(data)){
  path = paste0("./rnn.model/data/", i,".txt")
  text <- data$Text[i]
  sink(path)
  cat(as.character(data$Text[i]))
  sink()
}
data_list <- list.files("./rnn.model/data/", full.names = T)
data_raw <- file_import(data_list)
corpus_preprocessed_data <- text_pre_process(corpus = data_raw, count_threshold = 10, 
                                             dic = NULL)
seq_length_dist <- unlist(lapply(corpus_preprocessed_data$word_vec_list, length))
quantile(seq_length_dist, 0:20/20)

corpus_bucketed_data <- make_bucket_data(word_vec_list = corpus_preprocessed_data$word_vec_list, 
                                         labels = rep(0:1, each = 11609), 
                                         dic = corpus_preprocessed_data$dic, 
                                         seq_len = c(5,15,25), 
                                         right_pad = T)
saveRDS(corpus_bucketed_data, file = "./rnn.model/corpus_bucketed_data.rds")

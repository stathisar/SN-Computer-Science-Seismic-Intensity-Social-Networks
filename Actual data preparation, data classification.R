#preparation of data to be predicted
require("readr")
require("dplyr")
require("stringr")
require("stringi")
source("./data.preparation.functions.R")
setwd("/home/stathis/Desktop/testdata/")

#import of data to be predicted
data <- read.csv("/home/stathis/Desktop/SeismicIntensityArticle-master/tweets.initial.classification.csv")


#creation of vectors
for (i in 1:NROW(data)){
  path = paste0("/home/stathis/Desktop/SeismicIntensityArticle-master/initial/", i,".txt")
  text <- data$Text[i]
  sink(path)
  cat(as.character(data$Text[i]))
  sink()
}

#list, read vectors
initial_list <- list.files("/home/stathis/Desktop/SeismicIntensityArticle-master/initial/", full.names = T)
initial_raw <- file_import(initial_list)


#creation of corpus of data to be predicted
corpus_preprocessed_initial <- text_pre_process(corpus = initial_raw, count_threshold = 10, 
                                                dic = NULL)
seq_length_dist <- unlist(lapply(corpus_preprocessed_initial$word_vec_list, length))
quantile(seq_length_dist, 0:20/20)

#creation of bucket of data to be predicted
corpus_bucketed_initial <- make_bucket_data(word_vec_list = corpus_preprocessed_initial$word_vec_list, 
                                            labels = rep(0:1, each = 11609), 
                                            dic = corpus_preprocessed_initial$dic, 
                                            seq_len = c(5,15,25), 
                                            right_pad = T)

#classification

#set parameters
vocab <- length(corpus_bucketed_initial$dic)

batch.size <- 10

num.round <- 2

initial.data <- mx.io.bucket.iter(buckets = corpus_bucketed_initial$buckets, batch.size = batch.size, 
                                  data.mask.element = 0, shuffle = FALSE)


#import of rnn model
model <- mx.model.load("/home/stathis/Desktop/testdata/model_ambiguity_lstm", iteration = num.round)

#data prediction
pred2 <- mx.infer.rnn(infer.data = initial.data, model = model, ctx = mx.cpu())

#transform and merge with rest of data
pred2.transform = as.data.frame(matrix(nrow = 11609, ncol = 2))

for(i in 1:11609){
  pred2.transform[i, 1] <- as.numeric(pred2[1 ,i])
  pred2.transform[i, 2] <- as.numeric(pred2[2 ,i])
}
NROW(pred2.transform)
names(pred2.transform) = c("p", "n")
pred2.transform$id = 1:11609
tweets.with.int$id = 1:11609
data.final = merge(tweets.with.int, pred2.transform, by = "id")
write.csv(data.final, "/home/stathis/Desktop/SeismicIntensityArticle-master/data.final.rnn.27040049.csv")

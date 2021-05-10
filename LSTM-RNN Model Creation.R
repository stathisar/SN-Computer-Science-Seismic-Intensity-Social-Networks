
setwd("/home/stathis/Desktop/Scripts/")
require("mxnet")
corpus_bucketed_train <- readRDS(file = "./rnn.model/corpus_bucketed_train.improved__.rds")
corpus_bucketed_test <- readRDS(file = "./rnn.model/corpus_bucketed_test.improved__.rds")

vocab <- length(corpus_bucketed_train$d ic)

### Create iterators
batch.size <- 10
num.round <- 2

train.data <- mx.io.bucket.iter(buckets = corpus_bucketed_train$buckets, batch.size = batch.size, 
                                data.mask.element = 0, shuffle = TRUE)
eval.data <- mx.io.bucket.iter(buckets = corpus_bucketed_test$buckets, batch.size = batch.size, 
                               data.mask.element = 0, shuffle = FALSE)

mx.set.seed(0)
optimizer <- mx.opt.create("adadelta", rho = 0.92, epsilon = 1e-06, wd = 2e-04, clip_gradient = NULL, 
                           rescale.grad = 1/batch.size)

bucket_list <- unique(c(train.data$bucket.names, eval.data$bucket.names))

symbol_buckets <- sapply(bucket_list, function(seq) {
  rnn.graph(config = "seq-to-one",
            cell_type = "lstm",
            num_rnn_layer = 1,
            num_embed = 2,
            num_hidden = 10,
            num_decode = 2,
            input_size = vocab,
            dropout = 0.2,
            ignore_label = -1,
            loss_output = "softmax",
            output_last_state = F,
            masking = T)
})

t <-   rnn.graph(config = "seq-to-one",
                 cell_type = "lstm",
                 num_rnn_layer = 1,
                 num_embed = 2,
                 num_hidden = 10,
                 num_decode = 2,
                 input_size = vocab,
                 dropout = 0.2,
                 ignore_label = -1,
                 loss_output = "softmax",
                 output_last_state = F,
                 masking = T)


graph.viz(t, type = "graph", direction = "LR", 
          graph.height.px = 50, graph.width.px = 800, shape=c(64, 5))

# Accuracy on Training Data = 0.92
model_clearance_lstm <- mx.model.buckets(symbol = symbol_buckets,
                                         train.data = train.data,
                                         eval.data = eval.data,
                                         num.round = num.round,
                                         ctx = mx.cpu(),
                                         verbose = FALSE,
                                         metric = mx.metric.accuracy,
                                         optimizer = optimizer,
                                         initializer = mx.init.Xavier(rnd_type = "uniform",
                                                                      factor_type = "in",
                                                                      magnitude = 2),
                                         batch.end.callback = mx.callback.log.train.metric(period = 10),
                                         epoch.end.callback = NULL)

mx.model.save(model_clearance_lstm, prefix = "model_clearance_lstm_improved_but_530", iteration = num.round)
model <- mx.model.load("model_clearance_lstm_improved_but_530", iteration = num.round)

#the model needs to be parsed

pred <- mx.infer.rnn(infer.data = eval.data, model = model, ctx = mx.cpu())

ypred <- max.col(t(as.array(pred)), tie = "first") - 1

packer <- mxnet:::mx.nd.arraypacker()

eval.data$reset()

while (eval.data$iter.next()) {
  packer$push(eval.data$value()$label)
}

ylabel <- as.array(packer$get())
# Accuracy on Test Data = 0.92

acc <- sum(ylabel == ypred)/length(ylabel)

message(paste("Acc:", acc))


rm(list=ls())
library(keras)
library(tensorflow)
library(dplyr)
set.seed(8675309)
train=read.csv('train.csv')
tensorflow::tf$compat$v1$disable_eager_execution()

train$Description=iconv(train$Description,from='UTF-8',to='ASCII')


train1 = train %>% 
  select(-Name,-RescuerID,-Description,-PetID,-State)

sample_train = sample(nrow(train1))[1:round(.7*nrow(train1))]
sample_test = sample(nrow(train1))[((round(.7*nrow(train1)))+1):nrow(train1)]



train_df = train1[sample_train,]
test_df = train1[sample_test,]

train_labels = train_df$AdoptionSpeed
test_labels = test_df$AdoptionSpeed

train_df = train_df %>% select(-AdoptionSpeed)

train_df=as.matrix(train_df)
dimnames(train_df)=NULL


vectorize_sequences <- function(sequences, dimension = 8996) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}

# train_df=vectorize_sequences(train_df)


train_labels=to_categorical(train_labels)


model <- keras_model_sequential() %>% 
  layer_dense(units = 64, activation = "relu", input_shape = c(18)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 5, activation = "softmax")


model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)



history <- model %>% fit(
  (train_df),
  (train_labels),
  epochs = 50,
  batch_size=5,
  validation_split=.2
)








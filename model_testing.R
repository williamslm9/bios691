source('data_prep.R')
library(Metrics)

model <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "selu",input_shape = c(ncol(train_df))) %>%
  layer_dropout(rate=.5) %>%
  # layer_dense(units = 256, activation = "relu") %>%
  # layer_dropout(rate=.5) %>%
  # layer_dense(units = 128, activation = "relu") %>%
  # layer_dropout(rate=.5) %>%
  # layer_dense(units = 64, activation = "relu") %>%
  # layer_dropout(rate=.3) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate=.1) %>%
  layer_dense(units = 5, activation = "softmax")

summary(model)

model %>% compile(
  optimizer = 'adam',
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  (train_df),
  (train_labels),
  epochs = 100,
  batch_size=256,
  validation_split=.2
)


model %>%  evaluate(test_df,test_labels)

#######################################################################

predictions <- model %>% predict(test_df)


for(i in 1:nrow(predictions)){
  
  predictions[i,which.max(predictions[i,])]=1
  # c(1:5)[-which.max(predictions[1,])]
  predictions[i,c(1:5)[-which.max(predictions[i,])]]=0
  
}

pred=NULL
true=NULL

for(i in 1:nrow(predictions)){
  
  pred[i]=which(predictions[i,]==1)
  true[i]=which(test_labels[i,]==1)
}

pred=factor(pred,levels = c('1','2','3','4','5'))
# levels(pred)=c('1','2','3','4','5')
true=factor(true)

conf_table = table(true,pred)

ScoreQuadraticWeightedKappa(true,pred,1,5)

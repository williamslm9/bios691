rm(list=ls())
library(keras)
library(tensorflow)
library(dplyr)
# set.seed(23)
train=read.csv('train.csv')
# tensorflow::tf$compat$v1$disable_eager_execution()

# train$Age=train$Age/12
train$mixed_breed = ifelse(train$Breed2==0,0,1)
train$fee = ifelse(train$Fee==0,0,1)
train$video = ifelse(train$VideoAmt==0,0,1)
train$photo = ifelse(train$PhotoAmt==0,0,1)
train$quantity = ifelse(train$Quantity==1,0,1)
train$color = ifelse(train$Color3==0 & train$Color2 == 0,0,
                     ifelse(train$Color1!=0 & train$Color2!=0 &train$Color3==0,1,2))
# train1 = train %>%
#   select(-Name,-RescuerID,-Description,-PetID,-State,-Dewormed)


train1 = train %>%
  select(-Name,-RescuerID,-Description,-PetID,-State,-Breed1,-Breed2,-Fee,-VideoAmt,-PhotoAmt,-Quantity,-Dewormed,
         -Color1,-Color2,-Color3)
corrplot(cor(train1))


set.seed(8675409)
sample_rows =sample(2,nrow(train1),replace=TRUE,prob=c(.7,.3))

train_df = train1[sample_rows==1,]
test_df = train1[sample_rows==2,]

train_labels = train_df$AdoptionSpeed
test_labels = test_df$AdoptionSpeed

train_df = train_df %>% select(-AdoptionSpeed)
test_df = test_df %>%  select(-AdoptionSpeed)

train_df=as.matrix(train_df)
dimnames(train_df)=NULL
test_df=as.matrix(test_df)
dimnames(test_df)=NULL

train_df=keras::normalize(train_df)
test_df=keras::normalize(test_df)

train_labels = to_categorical(train_labels)
test_labels = to_categorical(test_labels)

model <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "relu",input_shape = c(ncol(train_df))) %>%
  layer_dropout(rate=.5) %>%
  layer_dense(units = 1024, activation = "relu") %>%
  layer_dropout(rate=.4) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dropout(rate=.3) %>%
  # layer_dense(units = 128, activation = "relu") %>%
  # layer_dropout(rate=.3) %>%
  layer_dense(units = 5, activation = "softmax")

summary(model)

model %>% compile(
  optimizer = optimizer_adam(lr=.0001),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  (train_df),
  (train_labels),
  epochs = 100,
  batch_size=128,
  validation_split=.2
)


model %>%  evaluate(test_df,test_labels)




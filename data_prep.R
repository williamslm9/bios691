rm(list=ls())
library(keras)
library(tensorflow)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(fastDummies)

# set.seed(23)
train=read.csv('train.csv')
# tensorflow::tf$compat$v1$disable_eager_execution()


train$Age=ifelse((train$Age)<6,0,1)

train$mixed_breed = ifelse(train$Breed2==0,0,1)

train$fee = ifelse(train$Fee==0,0,1)

# train$video = ifelse(train$VideoAmt==0,0,1)


train$photo = ifelse(train$PhotoAmt>0 & train$PhotoAmt<=3,0,1)
# hist(train$photo)

# train$quantity = ifelse(train$Quantity<=1,0,1)
# hist(train$quantity)


train$color = ifelse(train$Color3==0 & train$Color2 == 0,0,
                     ifelse(train$Color1!=0 & train$Color2!=0 &train$Color3==0,1,2))

# train1 = train %>%
#   select(-Name,-RescuerID,-Description,-PetID,-State,-Dewormed)


train1 = train %>%
  select(-Name,-RescuerID,-Description,-PetID,-State,-Breed1,-Breed2,-Fee,-VideoAmt,-PhotoAmt,-Quantity,-Dewormed,
         -Color1,-Color2,-Color3,-Vaccinated,-Sterilized)

set.seed(8675309)
sample_rows =sample(2,nrow(train1),replace=TRUE,prob=c(.8,.2))

train_df = train1[sample_rows==1,]
test_df = train1[sample_rows==2,]

train_labels = train_df$AdoptionSpeed
test_labels = test_df$AdoptionSpeed
test_labels_copy=test_labels

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
# corrplot(cor(train1))




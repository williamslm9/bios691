rm(list=ls())
library(keras)
library(tensorflow)
library(dplyr)

# set.seed(23)
train=read.csv('train.csv')
# tensorflow::tf$compat$v1$disable_eager_execution()
train$Type=train$Type-1
train$Gender=train$Gender-1
train$Vaccinated=train$Vaccinated-1
train$Dewormed=train$Dewormed-1
train$Sterilized=train$Sterilized-1

# train$Name=tolower(train$Name)
# 
# other = c('noname','no name','not given','not name','not yet','no name yet','girl','boy','female','male','rescue','kitty','kitten',
#           'cat','dog','doggy','doggie','puppy','pup','kitties','puppies','doggies','kittens')
# 
# other_dig = paste0('[[:digit:]]+[[:blank:]]?',other)
# nums=paste0(c('one','two','three','four','five','six','seven','eight','nine'),collapse='|')
# 
# other=paste0('^',paste0(other,collapse = '$|^'),'$')
# 
# name_empty_num = unique(c(grep('[[:digit:]]',train$Name),grep('^$',train$Name),
#                           grep(other,train$Name),grep(nums,train$Name),
#                           grep(paste0(other_dig,collapse='|'),train$Name)))
# train$new_name = train$Name
# train$new_name[name_empty_num]=0
# train$new_name[!rownames(train) %in% name_empty_num]=1
# train$new_name=as.numeric(train$new_name)


train$Age=ifelse(train$Age<=6,0,1)
# hist(train$Age)

train$mixed_breed = ifelse(train$Breed2==0,0,1)

train$fee = ifelse(train$Fee==0,0,1)

train$video = ifelse(train$VideoAmt==0,0,1)


train$photo = ifelse(train$PhotoAmt==0,0,ifelse(train$PhotoAmt>0 & train$PhotoAmt<=5,1,2))
# hist(train$photo)

train$Quantity = ifelse(train$Quantity<=1,0,1)
# hist(train$quantity)


train$color = ifelse(train$Color3==0 & train$Color2 == 0,0,
                     ifelse(train$Color1!=0 & train$Color2!=0 &train$Color3==0,1,2))

train1 = train %>%
  select(-Name,-RescuerID,-Description,-PetID,-State,-Breed1,-Breed2,-Fee,-VideoAmt,-PhotoAmt,
         -Color1,-Color2,-Color3,-Dewormed)



# corrplot(cor(train1))


# train1 = train %>%
#   select(-Name,-RescuerID,-Description,-PetID,-State,-Breed1,-Breed2,-Fee,-VideoAmt,-PhotoAmt,-Quantity,-Dewormed,
#          -Color1,-Color2,-Color3,-Vaccinated,-Sterilized)


train1=as.matrix(train1)
dimnames(train1)=NULL

train1[,c(3:9,14:15)]=keras::normalize(train1[,c(3:9,14:15)])


set.seed(8675309)
sample_rows =sample(2,nrow(train1),replace=TRUE,prob=c(.8,.2))

train_df = train1[sample_rows==1,]
test_df = train1[sample_rows==2,]


train_labels = train_df[,10]
test_labels = test_df[,10]
# test_labels_copy=test_labels

train_df =train_df[,-10]
test_df = test_df[,-10]



train_labels = to_categorical(train_labels)
test_labels = to_categorical(test_labels)
# corrplot(cor(train1))




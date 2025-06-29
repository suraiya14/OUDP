library(mlbench)
library(caret)

library(caTools)



#file<-read.csv("D:\\Research Work\\Disertation Project 1\\backup\\Raw Data\\featureExtraction/validation_merged_file.csv",header = TRUE)
file<-read.csv("D:\\Research_Work\\research on new idae\\idea2\\Feature_Extraction\\balanced_dataset.csv",header = TRUE)

nrow(file)
ncol(file)
sum(file$UD5OPIANY==1)
sum(file$UD5OPIANY==0)
#file = file[!(file$Output==1) >=235,]
#sum(file$Output==1)

library(dplyr)
#file %>%
#  summarise_all(funs(n_distinct(.))) %>%
#  select_if(. != 1)
#sum(file$Output==1)
#sum(file$Output==-1)
#set.seed(123)
#ind<-sample(2,nrow(file),replace = TRUE,prob = c(0.8,0.2))
#gur_tdm_train<-file[ind==1,]
#gur_tdm_test<-file[ind==2,]
#nrow(gur_tdm_train)
#sum(gur_tdm_train$Output==1)
#nrow(gur_tdm_test)

#sd<-123
#set.seed(sd)
#ind<-sample(2,nrow(file),replace = TRUE,prob = c(0.75,0.25))
##ind
#gur_tdm_train<-file[ind==1,]
#gur_tdm_test<-file[ind==2,]
#gur_tdm_train
#gur_tdm_test
#nrow(gur_tdm_train)
#nrow(gur_tdm_test)

library(caret)
train.index <- createDataPartition(file$UD5OPIANY, p = .8, list = FALSE)
train <- file[ train.index,]

train
test  <- file[-train.index,]


nrow(train)
nrow(test)
sum(train$UD5OPIANY==1)
sum(train$UD5OPIANY==0)

sum(test$UD5OPIANY==1)
sum(test$UD5OPIANY==0)


#set.seed(123)    # make the results reproducible

#inx <- sample.split(seq_len(nrow(file)), 0.8)
#train <- file[inx, ]
#test <-  file[!inx, ]
#nrow(train)
#nrow(test)


write.csv(train, "D:\\Research_Work\\research on new idae\\idea2\\Feature_Extraction\\dataSplit\\validate_train_data.csv", row.names = FALSE)

write.csv(test, "D:\\Research_Work\\research on new idae\\idea2\\Feature_Extraction\\dataSplit\\validate_test_data.csv", row.names = FALSE)


file<-read.csv("D:\\Research_Work\\research on new idae\\idea2\\Feature_Extraction\\dataSplit\\validate_train_data.csv",header = TRUE)
nrow(file)

library(dplyr)
file %>%
  summarise_all(funs(n_distinct(.))) %>%
  select_if(. != 1)
sum(file$UD5OPIANY==0)
sum(file$UD5OPIANY==1)


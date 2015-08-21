
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(parallel)
library(doParallel)

trainingRaw <- read.csv(file="pml-training.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))
testingRaw <- read.csv(file="pml-testing.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))

## Cleaning

trainingRaw <- trainingRaw[, colSums(is.na(trainingRaw)) == 0] 
testingRaw <- testingRaw[, colSums(is.na(testingRaw)) == 0] 

# Dropping reference columns raw_timestamp_part_1, raw_timestamp_part_2
# cvtd_timestamp, new_window, num_window
drops <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp",
           "new_window", "num_window")
trainingRaw <- trainingRaw[,!(names(trainingRaw) %in% drops)]
testingRaw <- testingRaw[,!(names(testingRaw) %in% drops)]

# Convert classe to factor
trainingRaw$classe <- as.factor(trainingRaw$classe)

## Pre processing

# Set train-test files

set.seed(123456)

fortrain <- createDataPartition(trainingRaw$classe, p = 0.6, list=FALSE)
training <- trainingRaw[fortrain,]
validation <- trainingRaw[-fortrain,]
test <- testingRaw[,-53]  # Removes problem id column

# Train model
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

        modFit <- train(classe ~., method="rf", data=training, 
                       trControl=trainControl(method="cv", 5,
                                              allowParallel=TRUE))
stopCluster(cl)

modFit
varImp(modFit)

# Accuracy validation set
ValPred <- predict(modFit, validation)
confusionMatrix(ValPred, validation$classe)

# # # Predictions on the testing set
finalPred <- predict(modFit, test)
finalPred





answ <- as.character(finalPred)



save(modFit, file="modFit-final.RData")

setwd("C:/Users/zgomez/Documents/ml-project/answers")

pml_write_files = function(x){
  n = length(x)
  path <- "C:/Users/zgomez/Documents/ml-project/answers"
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=file.path(path, filename),quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answ)

dataExercise <- read.csv(file = ".//data//pml-training.csv", header = T)

# First seven columns can be removed (observation number, user name,
# timestamps, and window counters).
dataExercise <- dataExercise[,-(1:7)]

# find fraction of NAs in each variable and eliminate variables
# with mostly NAs.
fracNA <- apply(dataExercise, MARGIN = 2, 
                function(x) sum(is.na(x))/length(x))
removeFracNA <- which(fracNA > 0.5)
dataExercise <- dataExercise[,-removeFracNA]
# find fraction of missing values in each variable and eliminate
# variables with mostly missing values.
fracMissing <- apply(dataExercise, MARGIN = 2, 
                function(x) sum(x=="")/length(x))
removeFracMissing <- which(fracMissing > 0.5)
dataExercise <- dataExercise[,-removeFracMissing]

# Check for missing data
sum(complete.cases(dataExercise))/nrow(dataExercise)

library(caret)

# Use random partitioning for cross validation.
set.seed(1222015)
inTrain <- createDataPartition(dataExercise$classe, p=0.7, list=F)
training <- dataExercise[inTrain,]
validation <- dataExercise[-inTrain,]

# create random forest model
model <- train(classe ~ ., data = training, method = "rf", prox = T, importance = T)
# display the results
model
# number of nodes in final model
finalTree <- getTree(model$finalModel, k=27, labelVar=T)
finalTree
nrow(finalTree)
# number of ternminal nodes
length(which(finalTree[,5]==-1))
# variables used in the tree
sort(unique(finalTree[,3]))
# variable importance -- train() must have importance=T attribute
importance(model)
varImpPlot(model)

# estimate out of bag error rate using the validation set
checkVal <- predict(model, validation[,-53])
confusionMatrix(checkVal, validation[,53])

####################################################################
### Predicting with test set.
dataTest <- read.csv(file = ".//data//pml-testing.csv", header = T)
dataTest <- dataTest[,-(1:7)]
dataTest <- dataTest[,-removeFracNA]
dataTest <- dataTest[,-removeFracMissing]

answers <- predict(model, newdata = dataTest[,-53])
answers <- as.character(answers)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(answers)

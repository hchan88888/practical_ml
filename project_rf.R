project_rf <- function(){
    
    #load packages and set random seed (assuming caret and e1071 installed)
    library(caret)
    library(e1071)
    set.seed(1234)
    
    #load data set
    train <- read.csv("pml-training.csv")

    # take a look at data
    summary(train)
    # replace empty cells with NA
    train1 <- apply(train, 2, function(x) ifelse(x =="", NA, x))
    # sum number of NAs in each column
    train2 <- apply(train1, 2, function(x)sum(is.na(x)))
    #extract columns with NAs
    names_to_remove<- names(train2[which(train2>0)])
    #remove columns with NAs
    train3 <- train1[, !(colnames(train1) %in% names_to_remove)]
    # convert matrix train3 to data.frame train4
    train4 <- as.data.frame(train3)
    # define other columns to remove
    names_to_remove2 <- c("X","user_name","raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp","new_window")
    # remove those columns
    train6 <- train4[, !(colnames(train4) %in% names_to_remove2)]
    # put the response variable in y
    y <- train6$classe
    # delete classe column
    train6$classe <-NULL
    # convert the data set into numeric
    train6 <- apply(train6, 2, as.numeric)
    #combine y with train6
    train7 <- as.data.frame(cbind(train6,y))
    # change column "y" to "classe"
    names(train7)[54] <- "classe"
    # convert column classe to factor
    train7$classe <- as.factor(train7$classe)
    
    # divide data in to training and testing sets
    inTrain <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
    training <- train7[inTrain,]
    testing <- train7[-inTrain,] 
    control <- trainControl(method = "cv", number=10, savePred=TRUE)
    #training model, predict and check accuracy
    model_rf <- train(classe ~., data=training, method="rf",trControl = control)
    print(model_rf)
    pred <-predict(model_rf, newdata=testing)
    confusionMatrix(pred, testing$classe)
    
    test <- read.csv("pml-testing.csv")
    test1 <- apply(test, 2, function(x) ifelse(x =="", NA, x))
    test3 <- test1[, !(colnames(test1) %in% names_to_remove)]
    test4 <- as.data.frame(test3)
    names_to_remove2 <- c("X","raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp")
    test5 <- test4[, !(colnames(test4) %in% names_to_remove2)]
    test6 <- test5[, 3:dim(test5)[2]]
    test6$problem_id <-NULL
    # convert the data set into numeric
    test6 <- apply(test6, 2, as.numeric)
    pred <-predict(model_rf, newdata=test6)    
    
}
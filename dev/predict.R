### Machine Learning algorithm for Walmart Kaggle competition

## Prepare

library(tidyr)
library(dplyr)
library(caret)



## Import

setwd("~/git/kaggle_walmart/data")
df.train.raw <- read.csv("train.csv", colClasses=c("factor", "integer", "factor", "factor", "integer", "factor", "factor"))
df.test.raw <- read.csv("test.csv", colClasses=c("integer", "factor", "factor", "integer", "factor", "factor"))



# ## Clean
# 
# # 1. Split in tidier data sets: df.inventory, df.train and df.test
# 
# # Build df.inventory with columns Upc, DepartmentDescription, FinelineNumber from both df.train and df.test
# df.inventory <- unique(
#     rbind(
#         subset(df.train.raw, select=c(Upc, DepartmentDescription, FinelineNumber)),
#         subset(df.test.raw, select=c(Upc, DepartmentDescription, FinelineNumber))
#     )
# )
# 
# # Drop DepartmentDescription and FinelineNumber from main data set since they belong in a separate data set df.inventory
# df.train <- subset(df.train.raw, select=-c(DepartmentDescription, FinelineNumber))
# df.test <- subset(df.test.raw, select=-c(DepartmentDescription, FinelineNumber))
# 
# 
# # 2. Check for NAs
# # Only Upc and FinelineNumber columns contain NA
# 
# 
# # 3. We consider each VisitNumber as a single observation (instead of Visitnumber-Upc combination)
# # and convert the data to exactly one observation per line
# 
# # In case of multiple lines with same VisitNumber-Upc combination, join together and take sum 
# df.train <- aggregate(ScanCount ~ ., data=df.train, FUN=sum) %>% arrange(VisitNumber, Upc)
# df.test <- aggregate(ScanCount ~ ., data=df.test, FUN=sum) %>% arrange(VisitNumber, Upc)
# 
# 
# # TODO: check NAs again
# # DEBUG
# head(df.train)
# names(df.train)
# lapply(df.train, class)
# df.tmp <- spread(data=df.train, key=Upc, value=ScanCount, fill=0)
# 


## Apply Machine Learning to raw data sets ('simple' method: predict TripType from single purchase, not single trip)

# Divide into training, validation and testing set. And drop VisitNumber column
set.seed(1234)
m.train.simple <- createDataPartition(df.train.raw$TripType, p=.1, list = FALSE)
df.train.simple <- subset(df.train.raw, select=-c(VisitNumber))[m.train.simple,]
df.validate.simple <- subset(df.train.raw, select=-c(VisitNumber))[-m.train.simple,]
df.test.simple <- subset(df.test.raw, select=-c(VisitNumber))

# Train models
model.simple.rf <- train(factor(TripType) ~ ., data=df.train.simple, method="rf")

# DEBUG
model.simple <- train(TripType ~ DepartmentDescription, data=df.train.simple, method="rpart")



# # Model details
# model.simple.rf
# model.simple.rf$finalModel
# # Tree model visualization
# library(rattle)
# fancyRpartPlot(model.simple.rf$finalModel)
# # Predict
# test.predictions.rf <- predict(model.simple.rf, newdata=df.validate.simple)
# # Confusion table and accuracy for testing set
# list.testing.results <- predict(model.best, newdata=df.testing)
# table(df.testing$classe, list.testing.results)
# sum(df.testing$classe == list.testing.results) / length(df.testing$classe)
# # Most important variables
# varImp(model.best)

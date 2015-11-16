### Machine Learning algorithm for Walmart Kaggle competition

## Prepare

library(tidyr)
library(dplyr)
library(caret)



## Import

setwd("~/git/kaggle_walmart")
df.train.raw <- read.csv("./data/train.csv", colClasses=c("factor", "integer", "factor", "factor", "integer", "factor", "factor"))
df.test.raw <- read.csv("./data/test.csv", colClasses=c("integer", "factor", "factor", "integer", "factor", "factor"))



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
# PROBLEM: not enough memory -> make validation set 10 times smaller
m.validate.simple <- createDataPartition(df.train.simple$TripType, p=.1, list = FALSE)
df.validate.simple <- df.validate.simple[m.validate.simple,]

# Train rpart model
# PROBLEM: not enough memory -> factors Upc and FinelineNumber have too many levels
# model.simple <- train(TripType ~ ., data=df.train.simple, method="rf")
set.seed(1234)
model.simple.rpart.1 <- train(TripType ~ Weekday + ScanCount + DepartmentDescription, data=df.train.simple, method="rpart")

# Model details
model.simple.rpart.1

# Save/load model to/from file
saveRDS(model.simple.rpart.1, "./results/model.simple.rpart.1.rds")
#model.simple.rpart.1 <- readRDS("./results/model.simple.rpart.1.rds")

# Confusion table and accuracy for validation set
results.validation.simple.rpart.1 <- predict(model.simple.rpart.1, newdata=df.validate.simple)
table(df.validate.simple$TripType, results.validation.simple.rpart.1)
sum(df.validate.simple$TripType == results.validation.simple.rpart.1) / length(df.validate.simple$TripType)

# Most important variables
varImp(model.simple.rpart.1)


## Idem for Random Forest model

set.seed(1234)
m.train.simple <- createDataPartition(df.train.raw$TripType, p=.05, list = FALSE)
df.train.simple <- subset(df.train.raw, select=-c(VisitNumber))[m.train.simple,]
df.validate.simple <- subset(df.train.raw, select=-c(VisitNumber))[-m.train.simple,]
df.test.simple <- subset(df.test.raw, select=-c(VisitNumber))
# PROBLEM: not enough memory -> make validation set 10 times smaller
m.validate.simple <- createDataPartition(df.train.simple$TripType, p=.1, list = FALSE)
df.validate.simple <- df.validate.simple[m.validate.simple,]

set.seed(1234)
# PROBLEM: not enough memory -> make training set small -> still issue
model.simple.rf.1 <- train(TripType ~ Weekday + ScanCount + DepartmentDescription, data=df.train.simple, method="rf")
model.simple.rf.1
saveRDS(model.simple.rf.1, "./results/model.simple.rf.1.rds")
results.validation.simple.rf.1 <- predict(model.simple.rf.1, newdata=df.validate.simple)
table(df.validate.simple$TripType, results.validation.simple.rf.1)
sum(df.validate.simple$TripType == results.validation.simple.rf.1) / length(df.validate.simple$TripType)
varImp(model.simple.rf.1)

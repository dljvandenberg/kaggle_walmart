### Machine Learning algorithm for Walmart Kaggle competition

## Prepare

library(tidyr)
library(dplyr)



## Import

setwd("~/git/kaggle_walmart/data")
df.train <- read.csv("train.csv", colClasses=c("factor", "integer", "factor", "factor", "integer", "factor", "factor"))
df.test <- read.csv("test.csv", colClasses=c("integer", "factor", "factor", "integer", "factor", "factor"))



## Clean

# 1. Split in tidier data sets: df.inventory, df.train and df.test

# Build df.inventory with columns Upc, DepartmentDescription, FinelineNumber from both df.train and df.test
df.inventory <- unique(
    rbind(
        subset(df.train, select=c(Upc, DepartmentDescription, FinelineNumber)),
        subset(df.test, select=c(Upc, DepartmentDescription, FinelineNumber))
    )
)

# Drop DepartmentDescription and FinelineNumber from main data set since they belong in a separate data set df.inventory
df.train <- subset(df.train, select=-c(DepartmentDescription, FinelineNumber))
df.test <- subset(df.test, select=-c(DepartmentDescription, FinelineNumber))


# 2. Check for NAs
# Only Upc and FinelineNumber columns contain NA


# 3. We consider each VisitNumber as a single observation (instead of Visitnumber-Upc combination)
# and convert the data to exactly one observation per line

# In case of multiple lines with same VisitNumber-Upc combination, join together and take sum 
df.train <- aggregate(ScanCount ~ ., data=df.train, FUN=sum) %>% arrange(VisitNumber, Upc)
df.test <- aggregate(ScanCount ~ ., data=df.test, FUN=sum) %>% arrange(VisitNumber, Upc)


# TODO: set correct class at import
# DEBUG
head(df.train)
names(df.train)
lapply(df.train, class)
df.tmp <- spread(data=df.train, key=Upc, value=ScanCount, fill=0)

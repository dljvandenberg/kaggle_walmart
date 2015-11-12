### Machine Learning algorithm for Walmart Kaggle competition

## Prepare

library(tidyr)
library(dplyr)



## Import

setwd("~/git/kaggle_walmart/data")
df.train.raw <- read.csv("train.csv")
df.test.raw <- read.csv("test.csv")



## Clean

# 1. Split in tidier data sets: df.inventory, df.train and df.test

# Build df.inventory with columns Upc, DepartmentDescription, FinelineNumber from both df.train.raw and df.test.raw
df.inventory <- unique(
    rbind(
        subset(df.train.raw, select=c(Upc, DepartmentDescription, FinelineNumber)),
        subset(df.test.raw, select=c(Upc, DepartmentDescription, FinelineNumber))
    )
)

# Drop DepartmentDescription and FinelineNumber from main data set since they belong in a separate data set df.inventory
df.train <- subset(df.train.raw, select=-c(DepartmentDescription, FinelineNumber))
df.test <- subset(df.test.raw, select=-c(DepartmentDescription, FinelineNumber))


# 2. Check for NAs
# Only Upc and FinelineNumber columns contain NA


# TODO: 3. We consider each VisitNumber as a single observation (instead of Visitnumber-Upc combination)
# and convert the data to exactly one observation per line

#mutate(df.train, sum = sum(ScanCount), by=c(VisitNumber,Upc)) -> incorrect
# See http://www3.nd.edu/~steve/computing_with_data/24_dplyr/dplyr.html

df.out <- spread(data=df.train, key=Upc, value=ScanCount, fill=0)

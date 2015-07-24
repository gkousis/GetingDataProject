## run_analysis.R for Getting Data course project
## Actions performed in this script
## 1
## Merge the training and testing data sets into one
## 2
## Drops uneccessary columns - keep only mean and std measurements
## 3
## Replace the activity with a descriptive names.  This is achieve using factors
## 4
## Create an independent tidy data set with the average of each variable by activity,subject


#####################################################################
## STEP 0 - general preparation
#####################################################################

## I use data.table extensively
library(data.table)

## Read the features (measurement names) in a data table.  This will be used to appropriately name the columns
features <- read.table("./data/UCI HAR Dataset/features.txt", header=FALSE, col.names=c("feature_id", "feature"))

features <- as.data.table(features)

## remove the () from the feature names to make the name more friendly
features[, feature := gsub("()", "", feature, fixed=TRUE)]


## Read the activity names 
activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header=FALSE, col.names=c("activity_id", "activity"))



#####################################################################
## STEP 1.1 - load the training part
#####################################################################

## Load the y_train set
y_train <- read.csv("./data/UCI HAR Dataset/train/y_train.txt", header=FALSE, col.names=c("activity_id"))


## This was the only code that worked on the x_train.  Even though the file appeared fwf, read.fwf does not work
x_train <- read.table("./data/UCI HAR Dataset/train/x_train.txt", header=FALSE, col.names=features[,feature])

## subject train
subject_train <- read.csv("./data/UCI HAR Dataset/train/subject_train.txt", header=FALSE, col.names=c("subject_id"))

## merge the training data sets
merged_train <- cbind(subject_train, y_train, x_train)


#####################################################################
## STEP 1.2 - load the test part
#####################################################################

## Load the y_train set
y_test <- read.csv("./data/UCI HAR Dataset/test/y_test.txt", header=FALSE, col.names=c("activity_id"))


## This was the only code that worked on the x_train
x_test <- read.table("./data/UCI HAR Dataset/test/x_test.txt", header=FALSE, col.names=features[,feature])

## subject train
subject_test <- read.csv("./data/UCI HAR Dataset/test/subject_test.txt", header=FALSE, col.names=c("subject_id"))

## merge the training data sets
merged_test <- cbind(subject_test, y_test, x_test)


#######################################################################
## STEP 1.3 - merge training and test data
#######################################################################

## merge the rows using rbind and convert to data table directly
accel <- as.data.table(rbind(merged_train, merged_test))


## now I can use byref operations and use more efficient calcs using the by clause
## but first clear the memory
rm(list=c("merged_test", "subject_test", "x_test", "y_test", "merged_train", "subject_train", "x_train", "y_train"))
gc()


#######################################################################
## STEP 2 - remove the unecessary columns.  
#######################################################################

## this builds a vector (via a datatable) from the colnames of the datatable and then uses a filter using the %like% operator
cols_remove <- data.table(colname=colnames(accel))[!(colname %like% "\\.mean\\.|\\.std\\.|subject_id|activity_id")][,colname]

## remove the columns by reference - very fast
accel[,(cols_remove) := NULL]

#######################################################################
## STEP 3 - use a descriptive name for acivity using the factor function
#######################################################################

## nice article (took a while to find) at http://www.r-bloggers.com/data-types-part-3-factors/
## activities is a data frame, not a data table
accel[,activity_id:=factor(activity_id, labels=activities$activity)]


#######################################################################
## STEP 4 - create final tidy data set mean by subject,activity
#######################################################################

## get the names of the columns I want to aggregate in a vector.  This will be used
## to dynamically build the expression instead of writing it by hand
aggr_col_names <- data.table(colname=colnames(accel))[(colname %like% "\\.mean\\.|\\.std\\.")][,colname]

## now build an expression for the mean expression instead of writing by hand
expr <- parse(text=paste0(".(", paste0(aggr_col_names, "=mean(", sep=aggr_col_names, collapse="),"), "))", collapse=""))

## final clean tidy set
tidy <- accel[,eval(expr), by=.(subject_id,activity_id)]

## export to text
write.table(tidy, file = "./tidy.txt", append=FALSE, row.names = FALSE )

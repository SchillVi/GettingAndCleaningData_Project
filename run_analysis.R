
# Getting and Cleaning Data Course Project


########
##### 1
########
# goal: merge training and test sets

# features (column names)
features <- read.table("features.txt", header = FALSE, sep = "")
features <- subset(features, select = V2)
feature <- t(features)

# test
subject_test <- read.delim("test/subject_test.txt", header = FALSE) 
x_test <- read.table("test/x_test.txt", header = FALSE, sep = "", col.names = feature)
y_test <- read.delim("test/y_test.txt", header = FALSE)

# train
subject_train <- read.delim("train/subject_train.txt", header = FALSE)
x_train <- read.table("train/x_train.txt", header = FALSE, sep = "", col.names = feature)
y_train <- read.delim("train/y_train.txt", header = FALSE)

# look at dimensions
dim(subject_test)
dim(x_test)
dim(y_test)

dim(subject_train)
dim(x_train)
dim(y_train)

## combine all data frames into one table

# row bind subject
subject <- rbind(subject_test, subject_train)
# row bind y
y <- rbind(y_test, y_train)

# column bind subject and y, 
# creating the first two columns of the new data frame
data_col <- cbind(subject, y)
# name the columns
colnames(data_col) <- c("subjectid", "activity")

# rowbind test and train measurements
x <- rbind(x_test, x_train)

# column bind the measurements to the subject ID and the activity
data <- cbind(data_col, x)


########
##### 2
########
# goal: 
# extract measurements on the mean and standard deviation for each measurement

name <- names(data)
# all variable names containing "mean"
means <- grep("mean", name, value = TRUE)
# all variable names containing "std"
stds <- grep("std", name, value = TRUE)

# vector of variables to extract
# include the first two columns (subjectid and activity)!
variables <- c("subjectid", "activity")
# and add all the names which contain either "mean" or "std"
variables <- append(variables, c(means, stds))

# extract the correct variables and save them in a new data set called data2
data2 <- data[, variables]
# check whether the names all contain either "mean" or "std"
grep("mean", names(data2))
grep("std", names(data2))


########
##### 3
########
# goal: create descriptive activity names

# names before
names(data2)

# change to lower case characters
names(data2) <- tolower(names(data2))

# erase dots
names(data2) <- gsub("\\.", "", names(data2))

# change the time indicator "t" to "time"
names(data2) <- gsub("^t", "time", names(data2))

# change the frequency indicator "f" to "freq"
names(data2) <- gsub("^f", "freq", names(data2))

# final names
names(data2)


########
##### 4
########
# goal: replace activity numbers with their labels

data_length <- length(data2$activity)
for (i in 1:data_length) {
      if(data2[i,2] == 1) data2[i,2] = "WALKING"
      if(data2[i,2] == 2) data2[i,2] = "WALKING_UPSTAIRS"
      if(data2[i,2] == 3) data2[i,2] = "WALKING_DOWNSTAIRS"
      if(data2[i,2] == 4) data2[i,2] = "SITTING"
      if(data2[i,2] == 5) data2[i,2] = "STANDING"
      if(data2[i,2] == 6) data2[i,2] = "LAYING"
}

########
##### 5
########
# goal: 
# create a second data set which contains the average of each variable for 
# each subject and each activity
# group by the subject ID and the activity, 
# then apply the mean function on the grouping
library(dplyr)
tidy_data <- data2 %>% group_by(subjectid, activity) %>% summarise_all(mean)


########
##### 6 
########
# save the data set from point 5 appropriately (as a .txt file)
write.table(tidy_data, file = "Tidy_data.txt", row.names = FALSE, sep = " ")

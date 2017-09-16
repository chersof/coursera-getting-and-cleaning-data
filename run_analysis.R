
##############################################################################
#                               Read data                                    #
##############################################################################

# first of all, we should download the the data files from the link below :
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# and then unzip the file in the R work directory.
# dplyr package is required for this script.

dataPath <- "UCI HAR Dataset"
# read training data
trainSubj <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainVal <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainActiv <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
testSubj <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testVal <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActiv <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features, don't convert text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

##############################################################################
#  Merge the training and the test sets to create one data set               #
##############################################################################

# concatenate individual data tables to make single data table
allActivity <- rbind(
    cbind(trainSubj, trainVal, trainActiv),
    cbind(testSubj, testVal, testActiv)
)

# remove individual data tables to save memory
rm(trainSubj, trainVal, trainActiv, 
   testSubj, testVal, testActiv)

# assign column names
colnames(allActivity) <- c("subject", features[, 2], "activity")

##############################################################################
#  Extract only the measurements on the mean and standard deviation for each #
#                               measurement                                  #
##############################################################################

# determine columns of data set to keep based on column name...
Keepcol <- grepl("subject|activity|mean|std", colnames(allActivity))

# ... and keep data in these columns only
allActivity <- allActivity[, Keepcol]

##############################################################################
#  Use descriptive activity names to name the activities in the data set     #
##############################################################################

# replace activity values with named factor levels
allActivity$activity <- factor(allActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

##############################################################################
#      Appropriately label the data set with descriptive variable names      #
##############################################################################

# get column names and remove special characters
allActivityCols <- colnames(allActivity)
allActivityCols <- gsub("[\\(\\)-]", "", allActivityCols)

# expand abbreviations and clean up names
allActivityCols <- gsub("^f", "frequencyDomain", allActivityCols)
allActivityCols <- gsub("^t", "timeDomain", allActivityCols)
allActivityCols <- gsub("Acc", "Accelerometer", allActivityCols)
allActivityCols <- gsub("Gyro", "Gyroscope", allActivityCols)
allActivityCols <- gsub("Mag", "Magnitude", allActivityCols)
allActivityCols <- gsub("Freq", "Frequency", allActivityCols)
allActivityCols <- gsub("mean", "Mean", allActivityCols)
allActivityCols <- gsub("std", "StandardDeviation", allActivityCols)

# correct typo
allActivityCols <- gsub("BodyBody", "Body", allActivityCols)

# use new labels as column names
colnames(allActivity) <- allActivityCols

##############################################################################
#      Create a second, independent tidy set with the average of each        #
#          variable for each activity and each subject                       #
##############################################################################

# group by subject and activity and summarise using mean
allActivityMeans <- allActivity %>% 
    group_by(subject, activity) %>%
    summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(allActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

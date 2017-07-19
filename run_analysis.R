################################################################################
# Course: Getting and Cleaning Data
# Title:  Programming Assignment
# Author: Patrick O'Malley
# Date:   7/18/17
################################################################################

# This is the script used to create the tidy data set required for this class
# The script will complete the following actions:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

# load packages
library(dplyr) # Data manipulation

# 1. Merge Data ----------------------------------------------------------------
x_train <- read.table("UCI HAR Dataset\\train\\X_train.txt")
str(x_train)
?read.table
features <- read.table("UCI HAR Dataset\\features.txt", stringsAsFactors = FALSE)
head(features)
str(features)
names(x_train) <- features$V2
names(x_train)
str(x_train)
y_train <- read.table("UCI HAR Dataset\\train\\Y_train.txt")
head(y_train)
summary(y_train)
names(y_train) <- "activity"
subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt")
names(subject_train) <- "subject"
train <- cbind(x_train, y_train, subject_train)

x_test <- read.table("UCI HAR Dataset\\test\\X_test.txt")
y_test <- read.table("UCI HAR Dataset\\test\\y_test.txt")
summary(y_test)
subject_test <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
names(subject_test) <- "subject"
test <- cbind(x_test, y_test, subject_test)
names(test) <- names(train)
names(test)

merged_data <- rbind(train, test)
paste(object.size(merged_data) * 1E-6, "Mb")

# there a several duplicates but they will be removed from the final data set
var_nam <- features %>% 
        group_by(V2) %>% 
        summarise(n = n()) %>% 
        filter(n > 1) %>% 
        View()

# 2. Extract Mean and SD -------------------------------------------------------

# fix column name issues
valid_names <- make.names(names = names(merged_data), unique = TRUE, allow_ = TRUE)
names(merged_data) <- valid_names
names(merged_data)
?regex
data <- select(merged_data, subject, activity, matches("(\\b.mean..\\b|\\b.std..\\b)"))
names(data)

# 3. Name activities -----------------------------------------------------------
# activity names are provided in activity labels file
# turn existing activity variable into a factor to include names
summary(data$activity)
head(data$activity)
data$activity <- factor(data$activity, labels = c("WALKING", 
                                                     "WALKING_UPSTAIRS",
                                                     "WALKING_DOWNSTAIRS",
                                                     "SITTING",
                                                     "STANDING",
                                                     "LAYING"))
str(data$activity)

# 4. Label variable names ------------------------------------------------------
names(data)
# labels already imported from features.txt

# 5. Avg variables for subject/activity combos ---------------------------------
tidy <- data %>% 
        group_by(subject, activity) %>% 
        summarise_all(funs(mean)) 
        
View(tidy)

# Save tidy data to new file
write.table(tidy, "tidy_data.txt")


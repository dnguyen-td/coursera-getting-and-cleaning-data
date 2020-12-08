#-------------------------------------------------------------
#Merges the training and the test sets to create one data set.
#-------------------------------------------------------------
mypath= "/Users/home/Google Drive_UC3M/17-UC3M STUDY/000_Research/R/Coursera_R programming/03_Tidy data/Week04/coursera-getting-and-cleaning-data"
setwd(mypath)

# Reading trainings tables:
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Reading testing tables:
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Reading feature vector:
features <- read.table('./UCI HAR Dataset/features.txt')

# Reading activity labels:
activityLabels = read.table('./UCI HAR Dataset/activity_labels.txt')

colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

merged_train <- cbind(y_train, subject_train, x_train)
merged_test <- cbind(y_test, subject_test, x_test)
merged <- rbind(merged_train, merged_test)
#-------------------------------------------------------------
#Extracts only the measurements on the mean and standard deviation for each measurement.
#-------------------------------------------------------------
colnames(merged)
mean_and_std <- (grepl("activityId" , colnames(merged)) | 
                         grepl("subjectId" , colnames(merged)) | 
                         grepl(".*mean.*" , colnames(merged)) | 
                         grepl(".*std.*" , colnames(merged) ) 
)
setForMeanAndStd <- merged[ , mean_and_std == TRUE]

#-------------------------------------------------------------
#Uses descriptive activity names to name the activities in the data set
#-------------------------------------------------------------
setForMeanAndStdWithNames <- merge(setForMeanAndStd, activityLabels, 
                                   by.y = "activityId",
                                   all.x=TRUE)


#-------------------------------------------------------------
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#-------------------------------------------------------------

secTidyDataset <- aggregate(. ~subjectId + activityType, 
                            data = setForMeanAndStdWithNames,
                            mean)
secTidyDataset <- secTidyDataset[order(secTidyDataset$subjectId, secTidyDataset$activityId),]

write.table(secTidyDataset, "secTidySet.txt", row.name=FALSE)


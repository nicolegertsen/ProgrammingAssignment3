library(dplyr)

# read test data
X.test <- read.table("./datascience/UCI HAR Dataset/test/X_test.txt", col)
y.test <- read.table("./datascience/UCI HAR Dataset/test/y_test.txt")
sub.test <- read.table("./datascience/UCI HAR Dataset/test/subject_test.txt")

# read train data
X.train<- read.table("./datascience/UCI HAR Dataset/train/X_train.txt")
y.train <- read.table("./datascience/UCI HAR Dataset/train/y_train.txt")
sub.train <- read.table("./datascience/UCI HAR Dataset/train/subject_train.txt")

# read data description
var.names <- read.table("./datascience/UCI HAR Dataset/features.txt")

# read activity labels
act.labels <- read.table("./datascience/UCI HAR Dataset/activity_labels.txt")

# 1. Merges the test and training datasets to create one dataset
X.total <- rbind(X.test, X.train)
y.total <- rbind(y.test, y.train)
sub.total <- rbind(sub.test, sub.train)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
selected.var <- var.names[grep("mean\\(\\)|std\\(\\)",var.names[,2]),]
X.total <- X.total[,selected.var[,1]]

# 3. Uses descriptive activity names to name the activities in de data set.
colnames(y.total) <- "activity"
y.total$act.label <- factor(y.total$activity, labels = as.character(act.labels[,2]))
activity.label <- y.total[,-1]

# 4. Appropriately labels the data set with descriptive variable names
colnames(X.total) <- var.names[selected.var[,1],2]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
colnames(sub.total) <- "subject"
total.dataset <- cbind(X.total, y.total, sub.total)
total.mean <- total.dataset %>% group_by(act.label, subject) %>% summarise_each(funs(mean))
write.table(total.mean, file = "./datascience/UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)



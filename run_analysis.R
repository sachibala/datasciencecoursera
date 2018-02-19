library(dplyr)

##### Load data from files #########
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

body_acc_x_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt")
body_acc_x_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt")
body_acc_y_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt")
body_acc_y_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt")
body_acc_z_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt")
body_acc_z_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt")

body_gyro_x_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt")
body_gyro_x_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt")
body_gyro_y_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt")
body_gyro_y_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt")
body_gyro_z_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt")
body_gyro_z_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt")

total_acc_x_train <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt")
total_acc_x_test <- read.table("UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt")
total_acc_y_train <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt")
total_acc_y_test <- read.table("UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt")
total_acc_z_train <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt")
total_acc_z_test <- read.table("UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
features <- read.table("UCI HAR Dataset/features.txt")

##### 1. Combine training and test data #######
X_train_test <- rbind(X_train, X_test)
Y_train_test <- rbind(Y_train, Y_test)
subject_train_test <- rbind(subject_train, subject_test)
body_acc_x_train_test <- rbind(body_acc_x_train, body_acc_x_test)
body_acc_y_train_test <- rbind(body_acc_y_train, body_acc_y_test)
body_acc_z_train_test <- rbind(body_acc_z_train, body_acc_z_test)
body_gyro_x_train_test <- rbind(body_gyro_x_train, body_gyro_x_test)
body_gyro_y_train_test <- rbind(body_gyro_y_train, body_gyro_y_test)
body_gyro_z_train_test <- rbind(body_gyro_z_train, body_gyro_z_test)
total_acc_x_train_test <- rbind(total_acc_x_train, total_acc_x_test)
total_acc_y_train_test <- rbind(total_acc_y_train, total_acc_y_test)
total_acc_z_train_test <- rbind(total_acc_z_train, total_acc_z_test)

##### 2. Extract only the measurements on the mean and standard deviation 
#####    for each measurement ######
colnames(X_train_test) <- make.names(features[['V2']], unique = TRUE)
mean_std_df <- X_train_test[,grepl("mean", colnames(X_train_test)) | grepl("std", colnames(X_train_test))]

##### 3. Descriptive activity names to name the activities in the data set
colnames(subject_train_test) <- 'Subject'
colnames(Y_train_test) <- 'Activity'
Y_train_test$Activity <- unlist(lapply(Y_train_test$Activity, function(x) activity_labels[activity_labels$V1 == x,2]))
#X_train_test_activities <- cbind(Y_train_test, X_train_test)


##### 4. Appropriate labels for data set with descriptive variable names
colnames(body_acc_x_train_test) <- lapply(rep(1:128, length(128)), function(x) paste0("Acceleration X-Axis Vector ", x, " (in g)"))
colnames(body_acc_y_train_test) <- lapply(rep(1:128, length(128)), function(x) paste0("Acceleration Y-Axis Vector ", x, " (in g)"))
colnames(body_acc_z_train_test) <- lapply(rep(1:128, length(128)), function(x) paste0("Acceleration Z-Axis Vector ", x, " (in g)"))
colnames(body_gyro_x_train_test) <- lapply(rep(1:128, length(128)), function(x) paste0("Angular Velocity X-Axis Vector ", x, " (in rad/sec)"))
colnames(body_gyro_y_train_test) <- lapply(rep(1:128, length(128)), function(x) paste0("Angular Velocity Y-Axis Vector ", x, " (in rad/sec)"))
colnames(body_gyro_z_train_test) <- lapply(rep(1:128, length(128)), function(x) paste0("Angular Velocity Z-Axis Vector ", x, " (in rad/sec)"))
colnames(total_acc_x_train_test) <- lapply(rep(1:128, length(128)), function(x) paste0("Body Acceleration X-Axis Vector ", x, " (Gravity-Total Acc.)"))
colnames(total_acc_y_train_test) <- lapply(rep(1:128, length(128)), function(x) paste0("Body Acceleration Y-Axis Vector ", x, " (Gravity-Total Acc.)"))
colnames(total_acc_z_train_test) <- lapply(rep(1:128, length(128)), function(x) paste0("Body Acceleration Z-Axis Vector ", x, " (Gravity-Total Acc.)"))

X_train_test_activities <- cbind(Y_train_test, subject_train_test, X_train_test, 
                                 body_acc_x_train_test, body_acc_y_train_test, body_acc_z_train_test, 
                                 body_gyro_x_train_test, body_gyro_y_train_test, body_gyro_z_train_test,
                                 total_acc_x_train_test, total_acc_y_train_test, total_acc_z_train_test
)

##### 5. Tidy data set with average of each variable for each activity and subject
tidy_df <- X_train_test_activities %>% group_by(Activity, Subject) %>% summarize_all(funs(mean))

write.table(tidy_df, file="assignment_output.txt", row.names = FALSE)

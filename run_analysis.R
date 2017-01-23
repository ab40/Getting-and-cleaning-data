#1. Sets your working directory to where you store the data
setwd('/Users/ab40/Desktop/UCI HAR Dataset/')

#2. Loads the features and activityLabels with read.table
features=read.table('/Users/ab40/Desktop/UCI HAR Dataset/features.txt', header=FALSE)
activityLabels=read.table('/Users/ab40/Desktop/UCI HAR Dataset/activity_labels.txt', header=FALSE)

#3. Loads the training data set
subjectTrain=read.table('/Users/ab40/Desktop/UCI HAR Dataset/train/subject_train.txt', header=FALSE)
xTrain=read.table('/Users/ab40/Desktop/UCI HAR Dataset/train/x_train.txt', header=FALSE)
yTrain=read.table('/Users/ab40/Desktop/UCI HAR Dataset/train/y_train.txt', header=FALSE)

#4. Gives descriptive activity names to name the activities in the training data set
colnames(activityLabels)=c('activityLabel','activityType')
colnames(subjectTrain)='subjectLabel'
colnames(xTrain)=features[,2]
colnames(yTrain)='activityLabel'

#5. Combines the training data set into one
train=cbind(subjectTrain,yTrain,xTrain)

#6. Loads the test data set
subjectTest=read.table('/Users/ab40/Desktop/UCI HAR Dataset/test/subject_test.txt', header=FALSE)
xTest=read.table('/Users/ab40/Desktop/UCI HAR Dataset/test/x_test.txt', header=FALSE)
yTest=read.table('/Users/ab40/Desktop/UCI HAR Dataset/test/y_test.txt', header=FALSE)

#7. Gives descriptive activity names to name the activities in the test data set
colnames(subjectTest)='subjectLabel'
colnames(xTest)=features[,2]
colnames(yTest)='activityLabel'

#8. Combines the test data set into one
test=cbind(subjectTest, yTest, xTest)

#9. Merges the training and the test sets to create one data set
data=rbind(train,test)

#10. Extracts only the measurements on the mean and standard deviation for each measurement
colNames=colnames(data)
measurments= (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
data=data[measurments==TRUE]
finalData= merge(data,activityLabels,by='activityLabel',all.x=TRUE)
colNames=colnames(finalData)

#11. Appropriately labels the data set with descriptive variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(finalData) = colNames

#12. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
finalData <- melt(finalData, id = c("subjectLabel", "activityType"))
finalData <- dcast(finalData, subjectLabel + activityType ~ variable, mean)

write.table(finalData, "tidy.txt", row.names = FALSE, quote = FALSE)

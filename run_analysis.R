#project
library(tidyverse)
library(plyr)

activityLabels = read.delim("./OneDrive/Coding/Coursera/dataClean/data/UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")
features = read.delim("./OneDrive/Coding/Coursera/dataClean/data/UCI HAR Dataset/features.txt", header = FALSE, sep = "")
names = features$V2

testDF = read.delim("./OneDrive/Coding/Coursera/dataClean/data/UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
testSubjects = read.delim("./OneDrive/Coding/Coursera/dataClean/data/UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "")
testLabels = read.delim("./OneDrive/Coding/Coursera/dataClean/data/UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")
colnames(testDF) = names
testDFAdd = mutate(testDF, Subject = testSubjects$V1, Activity = testLabels$V1)


trainDF = read.delim("./OneDrive/Coding/Coursera/dataClean/data/UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
trainSubjects = read.delim("./OneDrive/Coding/Coursera/dataClean/data/UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "")
trainLabels = read.delim("./OneDrive/Coding/Coursera/dataClean/data/UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "")
colnames(trainDF) = names
trainDFAdd = mutate(trainDF, Subject = trainSubjects$V1, Activity = trainLabels$V1)



allMatch = isTRUE(names(trainDFAdd) %in% names(testDFAdd))
testDF = union(trainDFAdd,testDFAdd) %>% arrange(Subject) 

dict = c(
  "1" = "WALKING",
  "2" = "WALKING_UPSTAIRS",
  "3" = "WALKING_DOWNSTAIRS",
  "4" = "SITTING",
  "5" = "STANDING",
  "6" = "LAYING"
)

testLabels = as.character(testDF$Activity) 
newLabels = c()

for (i in 1:length(testLabels)) {
  # print(dict[testLabels[i]])
  newLabels = append(newLabels, dict[testLabels[i]])
}

testDF = mutate(testDF, Activity = newLabels)

columns = names(testDF)
alltheguys = grep("std|Subject|mean|Activity", columns)
finalTable = testDF[alltheguys]
finalTable = select(finalTable, c(80, 81, 1:79))

columns = names(finalTable)
groupedMeansTable = finalTable %>% group_by(Subject, Activity) %>% summarise_at(vars(columns[3:81]), list(name = mean))
rm(features,testDF,testDFAdd,testSubjects,trainDF,trainDFAdd,trainLabels,trainSubjects,testLabels,activityLabels,allMatch,alltheguys,columns,dict,i,names,newLabels)

write.csv(finalTable,"./OneDrive/Coding/Coursera/dataClean/data/finalTable.csv", row.names = FALSE)
write.csv(groupedMeansTable,"./OneDrive/Coding/Coursera/dataClean/data/groupedMeansTable.csv", row.names = FALSE)

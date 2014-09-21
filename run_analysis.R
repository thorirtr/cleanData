# runAnalysis.r
# Description:
# Goal: Prepare tidy data that can be used for later analysis. 

# Get the data set and unzip it. 
setwd("/Users/thorir/Dropbox/Coursera/datafiles/getclean/")

if (!file.exists("runAnalysis")) {
  dir.create("runAnalysis")
}

# If the file doesn't exist... get it
if(!file.exists("./runAnalysis/dataset.zip")){
  # get the file
  file.url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  file.dest="./runAnalysis/dataset.zip"
  download.file(file.url,file.dest,method="curl")  
}
if (!file.exists("./runAnalysis/UCI HAR Dataset")) {
  file.unzip="./runAnalysis/"
  unzip(file.dest,exdir = file.unzip)
}

# Merge the training and the test sets to create one data set.

# change working directory to the location where the dataset was unzipped
setwd('/Users/thorir/Dropbox/Coursera/datafiles/getclean/runAnalysis/UCI HAR Dataset/')

# Load data from training files into variables, assume that if features variable exists then other will exist and skip reading
# the data into the variables. Shorter run time during testing.
if (!exists("features")) {
features = read.table('./features.txt',header=FALSE)
activityLabels = read.table('./activity_labels.txt',header=FALSE)
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain = read.table('./train/x_train.txt',header=FALSE)
yTrain = read.table('./train/y_train.txt',header=FALSE)
# Load data from test files
subjectTest = read.table('./test/subject_test.txt',header=FALSE)
xTest = read.table('./test/x_test.txt',header=FALSE)
yTest = read.table('./test/y_test.txt',header=FALSE)
}
# Assigin names to columns of the data loaded
colnames(activityLabels)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain) = features[,2]; 
colnames(yTrain) = "activityId";
colnames(yTest) = "activityId";
colnames(xTest) = features[,2]; 
colnames(subjectTest) = "subjectId";

# Merge data 
trainingData = cbind(yTrain,xTrain,subjectTrain);
testData = cbind(yTest,xTest,subjectTest);

# Combine the data sets
finalData = rbind(trainingData,testData);

# Create a vector from the column names of the finalData
colNames  = colnames(finalData); 

# Create a logicalVector from column names to capture ID, mean() & stddev()
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
finalData = finalData[logicalVector==TRUE];
finalData = merge(finalData,activityLabels,by='activityId',all.x=TRUE);

# Put new column headings on the finalData variable after merge.
colNames  = colnames(finalData); 

# Label the columns in accordance with the codebook
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
};
colnames(finalData) = colNames;


# Create a new table, finalNoActType without the activityType column
finalNoActType = finalData[,names(finalData) != 'activityType']

# Summarizing the finalNoActType table to contain the mean of each variable for each activity and each subject
cleanData = aggregate(finalNoActType[,names(finalNoActType) != c('activityId','subjectId')],by=list(activityId=finalNoActType$activityId,subjectId = finalNoActType$subjectId),mean)

# Merging the clean data set with activityLabels data to include descriptive acitvity names
cleanerData = merge(cleanData,activityLabels,by='activityId',all.x=TRUE)

# Export the cleaned Data set 
write.table(cleanerData, './cleanerData.txt',row.names=FALSE,sep='\t')
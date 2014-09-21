### codeBook.MD ###
Getting and Cleaning Data Project

## Description
Information about variables, data and transformation of the data that is used in this project. 

## Data Set Information
Informaiton about the data set can be found <a href="http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones">here</a>. The following is an excerpt from the web page:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information

For each record in the dataset it is provided:

Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
Triaxial Angular velocity from the gyroscope.
A 561-feature vector with time and frequency domain variables.
Its activity label.
An identifier of the subject who carried out the experiment.


### Section 1. 
Merge the training and the test sets to create one data set.

Set the source directory on lines 6 and 27 in the script. Then run the script to download the data and read the table data into the following variables (variable <- text file):

features <- features.txt
activityLabels <- activity_labels.txt
subjectTrain <- subject_train.txt
xTrain <- x_train.txt
yTrain <- y_train.txt
subjectTest <- subject_test.txt
xTest <- x_test.txt
yTest <- y_test.txt

Then assign column names to the data. After naming then merge to create one data set with cbind and rbind.

### Section 2. 
Extract only the measurements on the mean and standard deviation for each measurement.

Create a logcal vector that has only TRUE values for the IDs that have mean and stddev columns and FALSE for any other column. Subset the data to keep only these columns. 

### Section 3. 
Use descriptive activity names to name the activities in the data set

Merge the data that was subset in previous step with activityLabels table to include activity name with proper description. 

### Section 4. 
Appropriately label the data set with descriptive activity names.

By using gsub function for pattern replacement, clean up the data labels.

### Section 5. 
Create a second, independent tidy data set with the average of each variable for each activity and each subject.

As per instructions, create a second data set with the average of each variable for each activity and subject. Finally merge them and export all the cleaned data into a txt file. 

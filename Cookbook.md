Data process procedure includes five main steps: download (raw data), merge (training and test data sets), extract (variables of interest), summarize (by subject and by activity), and output (processed files). Raw data were downloaded from
  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
The unzipped raw data files contain some common files, which include a file that contains variable names (see "features.txt"), and a file that describs the activities (see "activity_labels.txt"), and two separate components: a training set and a test set. Each group of data underwent processes of assembling a data set that contain a subject ID column (obtained from "subject_train.txt" or "subject_test.txt"), an activity column (obtained from "y_train.txt" or "y_test.txt"), an identifier column, "whichSet", which specified whether the subject was partitioned in the training set or test set, and feature columns (obtained from "X_train.txt" or "X_test.txt"). The assembled training set was then merged with the assembled test set. Initially, feature labels in "features.txt" were used to name the feature variables. 

The above preliminary "merged_data.csv" was used to extract variables containing means and standard deviations, in additional to subject ID, activity, "whichSet" columns, to generated a smaller subset named "merged_data2". The variable names were simplified/cleaned in this step (see "merged_data2.csv").

Data "merged_data2" underwent further process to generate a summary data set, which gives the average of each variable for each activity and each subject, regardless whether the subject was used for trainging or testing.

R packages used for this project, as well as other references, can be found in "run_analysis.R". Note that the script was written in such a style that it was for one-time-use only, for learning and exploring the data, and for documenting the thought process.

For detailed descriptions of features, please see the link about the experiment in "GettingCleaningDataCourseProject_Coursera.pdf" and in "features_info.txt" in folder "RawDataDescriptions".  

# run_analysis.R

# 3/8/2015
# Coursera Data Science certification
# Course#3. Getting and Cleaning Data
# Course project

# Getting and Cleaning Data Course Project 
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following.
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Good luck!

# clear workspace
rm(list=ls())

current_dir = getwd()

########################################################################
# get data
########################################################################

# install.packages("downloader")
library(downloader)

# create data depository folder
if (!file.exists("ClassProj")) {dir.create("ClassProj")}
if (!file.exists("ClassProj/ClassProjRawData")) {dir.create("data/ClassProjRawData")}

setwd("ClassProj")

# download and unzip dataset
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile="./ProjData.zip")
unzip("./ProjData.zip",exdir="./ClassProjRawData")
list.files("./ClassProjRawData") # UCI HAR Dataset
file.remove("./ProjData.zip") # remove zip file when unzipped

rm(list=c("fileURL"))

########################################################################
# Requirement#1
# Merges the training and the test sets to create one data set.
########################################################################

# metadata
mydir <- "./ClassProjRawData/UCI HAR Dataset"
setwd(mydir)
getwd()

# this is a tricky part - convert factor to character before
#  it can be used for column headers
tt <- read.table("features.txt",fill = TRUE)
featureLabels <- as.character(t(tt[,2]))
# featureLabels
activityLabels <- read.table("activity_labels.txt",fill = TRUE)
# activityLabels

# leave it the way it is for now; do more after merge

###

# training set
subject <- read.table("train/subject_train.txt") #7352x1 # subjectID
names(subject) <- "SubjectID"
# unique(subject)
# #         SubjectID
# # 1            1
# # 348          3
# # 689          5
# # 991          6
# # 1316         7
# # 1624         8
# # 1905        11
# # 2221        14
# # 2544        15
# # 2872        16
# # 3238        17
# # 3606        19
# # 3966        21
# # 4374        22
# # 4695        23
# # 5067        25
# # 5476        26
# # 5868        27
# # 6244        28
# # 6626        29
# # 6970        30
activity <- read.table("train/y_train.txt") #7352x1 # activityID
names(activity) <- "Activity"

data <- read.table("train/X_train.txt") #7352x561

# check for missing data
sum(is.na(data)) # 0 (no missing data)

# add header row to data
merged_data <- data
names(merged_data) <- featureLabels
# str(merged_data)

# add "WhichSet" to identify whether the data are used for training or testing
whichSet <- matrix(rep("train",each=nrow(subject)),
                    nrow=nrow(subject))

# add SubjectID, activity, and Partition columns
merged_data <- cbind(subject, activity, whichSet, merged_data)
names(merged_data)
# head(merged_data)
# str(merged_data)

########################
# done with training set
########################

###

# now do the same thing to testing set
subject <- read.table("test/subject_test.txt") #7352x1 # subjectID
names(subject) <- "SubjectID"
# unique(subject)
# #         SubjectID
# # 1            2
# # 303          4
# # 620          9
# # 908         10
# # 1202        12
# # 1522        13
# # 1849        18
# # 2213        20
# # 2567        24
activity <- read.table("test/y_test.txt") #2947x1 # activityID
names(activity) <- "Activity"

data <- read.table("test/X_test.txt") #2947x561

# check for missing data
sum(is.na(data)) # 0 (no missing data)

# add "WhichSet" to identify whether the data come from train or test
whichSet <- matrix(rep("test",each=nrow(subject)),
                   nrow=nrow(subject))
##########################################################################
# Copied from:
# http://www.r-bloggers.com/a-quick-way-to-do-row-repeat-and-col-repeat-rep-row-rep-col/
# Accessed: 3/9/2016
# rep.row<-function(x,n){
#   matrix(rep(x,each=n),nrow=n)
# }
# rep.col<-function(x,n){
#   matrix(rep(x,each=n), ncol=n, byrow=TRUE)
# }
##########################################################################


# add header row to data
merged_data2 <- data
names(merged_data2) <- featureLabels
# str(merged_data2)

# add subjectID and activity columns
merged_data2 <- cbind(subject, activity, whichSet, merged_data2) 
# head(merged_data2)
# str(merged_data2)

########################
# done with testing set
########################

###
# now merge training and testing sets
merged_data <- rbind(merged_data, merged_data2) # 10222x89
# merged_data[7352:7358, 1:5]

########################
# done with Assignment#1
########################

########################################################################
# Requirement#2
# Extracts only the measurements on the mean and standard deviation
# for each measurement.
########################################################################

# Note:
# Technically speaking there are only six quantities were measured
# (Accel_XYZ and Gyro_XYZ) and the rest are derived values.
# I will play along pretending all variables are measured, though.

###

# subset mean and std columns
idx1 <- grep(("[mM][eE][aA][nN]"), names(merged_data))
idx2 <- grep("[sS][tT][dD]", names(merged_data))
merged_data2 <- merged_data[,c(1:3,idx1,idx2)]
# head(merged_data2)
# str(merged_data2)

########################
# done with Assignment#2
########################

########################################################################
# Requirement#3
# Uses descriptive activity names to name the activities in the data set.
########################################################################

# Note:
# While the unit of the measured quantities is "g", it is not clear
# about the units of those features. As a result, units were not
# Added to the final set to avoid misinterpretation.


# str(activityLabels)
# # 'data.frame':	6 obs. of  2 variables:
# #   $ V1: int  1 2 3 4 5 6
# # $ V2: Factor w/ 6 levels "LAYING","SITTING",..: 4 6 5 2 3 1
t <- merged_data2[,2]
# str(t) # int
idx <- match(t,activityLabels[,1])
# idx[1:10]
ActivityStr <- activityLabels[idx,2]
# str(ActivityStr)
# # Factor w/ 6 levels "LAYING","SITTING",..: 3 3 3 3 3 3 3 3 3 3 ...

merged_data2$Activity <- ActivityStr
# names(merged_data2)
# merged_data2$Activity[1:10]

########################
# done with Assignment#3
########################

########################################################################
# Requirement#4
# Appropriately labels the data set with descriptive variable names.
########################################################################

# Note:
# () in column names will be removed

names(merged_data2) <- gsub("[()]","",names(merged_data2))
# names(merged_data2)

########################
# done with Assignment#4
########################

########################################################################
# Requirement#5
# From the data set in step 4, creates a second, independent tidy data
# set with the average of each variable for each activity and each subject.
########################################################################

# install.packages("reshape2")
library(reshape2)

t <- merged_data2[,c(1:2,4:ncol(merged_data2))] # remove WhichSet
t_molten <- melt(t, id = c("SubjectID", "Activity"), na.rm = TRUE)
# str(t_molten)
# # 'data.frame':	813621 obs. of  4 variables:
# #   $ SubjectID: int  1 1 1 1 1 1 1 1 1 1 ...
# # $ Activity : Factor w/ 6 levels "LAYING","SITTING",..: 3 3 3 3 3 3 3 3 3 3 ...
# # $ variable : Factor w/ 79 levels "tBodyAcc-mean-X",..: 1 1 1 1 1 1 1 1 1 1 ...
# # $ value    : num  0.289 0.278 0.28 0.279 0.277 ...

# group mean
meanSet <- dcast(t_molten, formula = SubjectID + Activity ~ variable, mean) # 180x88
# head(meanSet)
# str(meanSet)

########################
# done with Assignment#5
########################a

########################################################################
# Final step:
# Output the data sets generated after Step 4 and 5, respectively
########################################################################

# create merged data folder for processed data
if (!file.exists("../../Output")) {dir.create("../../Output")}

# merged raw data
write.csv(file = "../../Output/merged_data.csv", x = merged_data, row.names = FALSE)
# processed data after Step#4
write.csv(file = "../../Output/merged_data2.csv", x = merged_data2, row.names = FALSE)
# summary data after Step#5
write.csv(file = "../../Output/meanSet.csv", x = meanSet, row.names = FALSE)

setwd(current_dir)

print("finished")

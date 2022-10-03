# Getting and Cleaning Data Course Project

## Loading library used in the project
library(dplyr)
library(expss) # for label the variable names

## 1. Merges the training and the test sets to create one data set.
### the path of files
vars_path <- "./data/gcdset/features.txt"
testx_path <- "./data/gcdset/test/X_test.txt"
trainx_path <- "./data/gcdset/train/X_train.txt"
### read the variable name list
varsname <- read.table(vars_path)
### extract the column of variable name
colnames <- varsname$V2
### read the test and train table and merge to one table with extracted variable names
testdata <- read.table(testx_path, col.names = colnames)
traindata <- read.table(trainx_path, col.names = colnames)
ttdata <- rbind(testdata, traindata) ### the table "ttdata" for requirement 1

## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
### select names of columns with ".mean." and ".std."
ttsele <- tbl_df(ttdata) %>%
        select(contains(".mean.")|contains(".std.")) ### the table "ttsele" for requirement 2

## 3.Uses descriptive activity names to name the activities in the data set
### file path 
testl_path <- "./data/gcdset/test/y_test.txt"
trainl_path <- "./data/gcdset/train/y_train.txt"
label_path <- "./data/gcdset/activity_labels.txt"
### read and merge test and train activity list 
testl <- read.table(testl_path)
trainl <- read.table(trainl_path)
activityl <- rbind(testl, trainl)
### connect with activity label to name the activities
label_name <- read.table(label_path)
act_name <- left_join(activityl, label_name, by = "V1")
### adding activity variable column to table 
tta_data <- bind_cols(ttsele, activity = act_name$V2) ### the table "tta_data" for requirement3

## 4.Appropriately labels the data set with descriptive variable names. 
### generate descriptive label name by editing  variables names, stored in the "var_lables"
var_names <- names(tta_data)
var_labels <- vector()
for (i in 1:length(var_names)) {
        gen_lable <- sub("BodyBody", "Body", var_names[i])
        gen_lable <- sub("^t", "TIME DOMAIN SIGNALS OF ", gen_lable)
        gen_lable <- sub("^f", "FREQENCY DOMAIN SIGNALS OF ", gen_lable)
        gen_lable <- sub("BodyAcc", "BODY ACCELERATION ", gen_lable)
        gen_lable <- sub("BodyGyro", "AUGULAR VELOCITY ", gen_lable)
        gen_lable <- sub("GravityAcc", "GRAVITY ACCELERATION ", gen_lable)
        gen_lable <- sub("Jerk", "JERK ", gen_lable)
        gen_lable <- sub("Mag", "MAGNITUDE ", gen_lable)
        gen_lable <- sub(".mean..", "ON MEAN", gen_lable)
        gen_lable <- sub("..std..", "ON STANDARD DEVIATION", gen_lable)
        gen_lable <- sub(".X$", "(X AXIS)", gen_lable)
        gen_lable <- sub(".Y$", "(Y AXIS)", gen_lable)
        gen_lable <- sub(".Z$", "(Z AXIS)", gen_lable)
        var_labels <- append(var_labels, gen_lable)
        ### label the variable names of "tta_data"   
        var_lab(tta_data[i]) = var_lables[i] ### the labelled "tta_data" for requirement4
}

## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
### binding the subject variable from test and train set
tests_path <- "./data/gcdset/test/subject_test.txt"
trains_path <- "./data/gcdset/train/subject_train.txt"
tests <- read.table(tests_path)
trains <- read.table(trains_path)
subject <- rbind(tests, trains)
ttas_data <- bind_cols(tta_data, subject = subject$V1)
### create second tidy data set with the average of each variable for each activity and subject
### dataset in step 4 "tta_data" was grouped by "activity" and "subject" columns 
ttas_group <- ttas_data %>% group_by(activity, subject) 
### calculating average of each variable for each activity and subject as variable named by "Average" plus origin variable name for new table "ttas_sum"
for (i in 1:66) {
        temp_name <- paste("Average", var_names[i], sep = ".")
        if(i == 1){
                
                ttas_sum <- summarise(ttas_group, mean(ttas_group[i]))
        }else{
                
                ttas_temp <- summarise(ttas_group, mean(ttas_group[i]))
                ttas_sum <- cbind(ttas_sum, ttas_temp[3])
        }
        n <- i + 2
        names(ttas_sum)[n] <- temp_name
}
### the second independently table "ttas_sum" for requirement 5.

#### END
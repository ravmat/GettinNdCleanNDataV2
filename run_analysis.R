
## I. Merges the training and the test sets to create one data set.
# #Actual merging is done oafter subsetting to the required data
## 1st read the test data set
test_tabl <- read.table( "./test/X_test.txt", head=F, sep="")

## and read the training data set; this is the main chunk of data collected in the mobile based movement/activity experiment
train_tabl <- read.table("./train/X_train.txt", head=F, sep="")

## II. Extracts only the measurements on the mean and standard deviation for each measurement. 
## drop the columns not required
#  Specify the columns of interest: ONLY mean and standard deviation columns retained
#  get the list of names for features from modified features.txt file: Subset_features.txt
# Actually using a version of the features file with DESCRIPTIVE names for the to-be extracted variables
sub_features_Col <- read.table( "./Subset_features_Descriptive.txt", head=T, sep="")
sub_train_tabl<- train_tabl[sub_features_Col[,1]]
# process test_table
sub_test_tabl<- test_tabl[sub_features_Col[,1]]
# Now merge the two sets row-wise; Result: only the measurements on the mean and standard deviation
trainAndtest_Tabl <- rbind(sub_train_tabl, sub_test_tabl)

## IV. Appropriately label the data set with descriptive variable names. 
#  Set DESCRIPTIVE Column Names for the mean and Std subset; use of Subset_features_Descriptive.txt above
colnames(trainAndtest_Tabl)<-sub_features_Col[,2]

## Bring in the Activity Description and position it as Col1
#  for this use activity_labels.txt for the activity descriptor
#  create a vector with activity labels (Walking, Sitting etc.) before adding it to the table
y_trainVector <- read.table("./train/y_train.txt", head=F, sep="")
y_testVector <- read.table("./test/y_test.txt", head=F, sep="")
combinedActivity <- rbind(y_trainVector, y_testVector)
#now prepare the  activity description/type vector
activityLabelsTabl <- read.table("./activity_labels.txt", head=F, sep="")
ActLen <- length(combinedActivity[,1])
for(i in 1:ActLen){
        #grab and insert label into this data frame for use later
        combinedActivity[i,2]=activityLabelsTabl[combinedActivity[i,1],2]
}
## Set descriptive activity names to name the activities in the data set
#   Add the activity type to main table as Column-1
trainAndtest_Tabl <- cbind(combinedActivity[,2],trainAndtest_Tabl)
# Add column name "Activity"
names(trainAndtest_Tabl)[1]<-paste("Activity")

# Add the Subject (who is doing the activity) info as  column-1 in trainAndtest_Tabl Table
trainSubjVector <- read.table("./train/subject_train.txt", head=F, sep="")
testSubjVector <- read.table("./test/subject_test.txt", head=F, sep="")
combinedSubjectList <- rbind(trainSubjVector, testSubjVector)

#   Add the subject to main combined/merged table as Column-1
trainAndtest_Tabl <- cbind(combinedSubjectList[,1],trainAndtest_Tabl)
# Add column name "Subject"
names(trainAndtest_Tabl)[1]<-paste("Subject")

## V. From the data set in step 4, to create a second,
## independent tidy data set with the average of each variable
## for each activity and each subject.
## TidyDataSetdf is the data frame that holds the final tidy data with averages
#Initialize the New Table
TidyDataSetdf <- trainAndtest_Tabl[0,]
numericCols = length(TidyDataSetdf[0,])
row <-0
nSubjects <-30
for (Si in 1:nSubjects){ ## Filter on Subject
        S<- trainAndtest_Tabl$Subject==Si
        SubjRowSlice <- trainAndtest_Tabl[S,]
        for (Ai in activityLabelsTabl[,2]){
                row <- row+1
                A <- SubjRowSlice$Activity==Ai
                ActvRowSlice <- SubjRowSlice[A,]
                TidyDataSetdf[row,1] <- Si # set the Subject in the row
                TidyDataSetdf[row,2] <- Ai # set the Activity in the row
                # Compue the mean of each of the numeric values columns and add to the row
                for(n in 3:numericCols){
                        TidyDataSetdf[row,n] <-  mean(ActvRowSlice[,n])        
                }
        }
}
## Order per Activity instead of Subject
TidyDataSetdf <- TidyDataSetdf[with(TidyDataSetdf, order(Activity)), ]

## Export tiday data as a text fule
write.table(TidyDataSetdf, 'TidyData.txt', row.name=FALSE)

 

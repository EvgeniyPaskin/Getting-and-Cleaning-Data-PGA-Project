########### Getting & Cleaning data Peer Graded Asignment script ##############
###############################################################################


## Setting prerequisites & Loading required packages
path <- "~/Dropbox/_MyProjects/Data Science/Cleaning Data/PGA"
setwd(path)
library(dplyr)
library(tidyr)


## Reading files

        # reading subject data
        subtrain <- read.table(paste(path, "/train/","subject_train.txt", sep=""))
        subtest  <- read.table(paste(path, "/test/","subject_test.txt", sep=""))

        # reading activity data
        acttrain <- read.table(paste(path, "/train/","Y_train.txt", sep=""))
        acttest  <- read.table(paste(path, "/test/","Y_test.txt", sep=""))

        # reading data
        dtrain <- read.table(paste(path, "/train/","X_train.txt", sep=""))
        dtest  <- read.table(paste(path, "/test/","X_test.txt", sep=""))
        
        #reading features names
        dfeat <- read.table(paste(path,"/","features.txt", sep=""))
        
        #reading activity names
        actlabels <- read.table(paste(path,"/","activity_labels.txt", sep=""))
        
# 1. Merging the training and the test sets to create one data set.        
        
        DT <- rbind(dtrain, dtest)  ## combining train and testing data
        SB <- rbind(subtrain, subtest) ## combining subject data
        AT <- rbind(acttrain, acttest ) ## combining activity data

        # Renaming variables       
        names(AT) <- "activity" # renaming variable in Activity DF
        names(SB) <- "subject"  # renaming variable in Subject DF
        names(DT) <- dfeat$V2
        names(actlabels) <- c("activity", "ActivityName")
        
        # Merging datasets to one
        DT<-cbind(SB, AT, DT)
        
        
# 2. Extracts only the measurements on the mean and standard deviation for each measurement
        
        # Extracting required veriables with mean and std
        DTmeanstd <- grep(".*mean.*|.*std.*", names(DT),value=TRUE) 
       
        # Adding activity and subject variables to the vector of required variables
        DTmeanstd <- union(c("subject","activity"), DTmeanstd)
        
        # Taking only measurements for the mean and standard deviation plus subject & activity
        DT2 <- subset(DT,select=DTmeanstd) 
        
# 3. Using descriptive activity names to name the activities in the DT
      
        # Adding name of activity into data frame
        DT3 <- full_join(DT2, actlabels, by = "activity")
        DT3$ActivityName <- as.character(DT3$ActivityName)
        
        
# 4. Appropriately labels the data set with descriptive variable names.
  
        
        names(DT3)<-gsub("std()", "SD", names(DT3))
        names(DT3)<-gsub("mean()", "MEAN", names(DT3))
        names(DT3)<-gsub("^t", "time", names(DT3))
        names(DT3)<-gsub("^f", "frequency", names(DT3))
        names(DT3)<-gsub("Acc", "Accelerometer", names(DT3))
        names(DT3)<-gsub("Gyro", "Gyroscope", names(DT3))
        names(DT3)<-gsub("Mag", "Magnitude", names(DT3))
        names(DT3)<-gsub("BodyBody", "Body", names(DT3))
       

# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.        
        
        
        
        ## calculate average for each activity and each subject
        DT4<- aggregate(. ~ subject - ActivityName, data = DT3, mean) 
        DT5<- tbl_df(arrange(DT4,subject,ActivityName))
        
        ## create a second, independent tidy data set 
        write.table(DT5, "independent_tidy_data_set.txt", row.name=FALSE)
        
        ## remove intermediate variables
        rm("AT", "DT", "DT2", "DT3", "DT4", "DTmeanstd", "SB",       
        "actlabels", "acttest", "acttrain", "dfeat",     
        "dtest", "dtrain", "path", "subtest", "subtrain")
        
        
        
run_analysis = function()
{
    setwd("C:\\home\\ap\\Tech\\Coursera\\Clean\\data\\Project")
    
    # Read the activities
    activities = read.table("activity_labels.txt")
    
    # Read the features
    features = read.table("features.txt", stringsAsFactors=FALSE)
    
    #===================================#
    print('Clean and merge Test data')  
    #===================================#
    
    dataTest = read.table("test/X_test.txt")
    
    activityIdTest = read.table("test/y_test.txt")
    
    # Set descriptive activity names
    activityTest = factor(activityIdTest$V1, 
                          levels = activities$V1,
                          labels = activities$V2)
    
    subjectTest = read.table("test/subject_test.txt")
    
    mergeTest = data.frame(subjectId=subjectTest$V1, 
                           activity=activityTest, 
                           dataTest[,c(1:ncol(dataTest))])
    
    #===================================#
    print('Clean and merge Training data')
    #===================================#
    dataTrain = read.table("train/X_train.txt")
    
    activityIdTrain = read.table("train/y_train.txt")
    
    # Set descriptive activity names
    activityTrain = factor(activityIdTrain$V1, 
                           levels = activities$V1,
                           labels = activities$V2)
    
    subjectTrain = read.table("train/subject_train.txt")
    
    mergeTrain = data.frame(subjectId=subjectTrain$V1, 
                            activity=activityTrain, 
                            dataTrain[,c(1:ncol(dataTrain))])
    
    #===================================#
    print('Merge Test and Training data')
    #===================================#
    allData = rbind(mergeTest, mergeTrain)
    
    #===================================#
    print('Extract std and mean columns')
    #===================================#
    stdMeanCols = grep("*mean\\(\\)*|*std*", features$V2)
    
    # add 2 to the col index because subject and activity columns were inserted
    stdMeanData = allData[,c(1,2,stdMeanCols+2)]
    orderdData = stdMeanData[order(stdMeanData$subjectId),]
    
    finalData = aggregate(orderdData[,3:68], 
                          by=list(orderdData$subjectId, 
                                  orderdData$activity), 
                          FUN=mean)
    
    #===================================#
    print('Set column names')
    #===================================#
    stdMeanNames = grep("*mean\\(\\)*|*std*", features$V2, value=TRUE)
    stdMeanNames = sub("-std", "Std", stdMeanNames)
    stdMeanNames = sub("-mean", "Mean", stdMeanNames)
    stdMeanNames = sub("\\(\\)", "", stdMeanNames)
    stdMeanNames = sub("-", "", stdMeanNames)
    #stdMeanNames = sub("^t", "Time", stdMeanNames)
    #stdMeanNames = sub("^f", "Freq", stdMeanNames)
    
    colnames(finalData) = c("subjectId", 
                            "activity", 
                            stdMeanNames[1:length(stdMeanNames)])

    #===================================#
    print('Write as a CSV file')
    #===================================#
    write.csv(finalData, "ap-osd-final-data.csv")
}



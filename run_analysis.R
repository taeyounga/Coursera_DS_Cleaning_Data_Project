#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("data.table")
#install.packages("reshape2")
library(dplyr)
library(tidyr)
library(reshape2)
library(plyr)

features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")
xtest <- read.table("test/X_test.txt")
ytest <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")
names(subject_test) <- "Subject"

selected_features_labeled <- grep(".*mean.*|.*std.*",features[,2] , value = TRUE)
selected_features <- grep(".*mean.*|.*std.*",features[,2])

xtest2 <- xtest[selected_features]
names(xtest2) <- selected_features_labeled

ytest2 <- merge(x = ytest, y = activity_labels, by.x = "V1", sort = FALSE)
names(ytest2) <- c("ActivityCode", "Activity")
test_set <- cbind(subject_test, ytest2[2], xtest2)

xtrain <- read.table("train/X_train.txt")
ytrain <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")
names(subject_train) <- "Subject"

xtrain2 <- xtrain[selected_features]
names(xtrain2) <- selected_features_labeled

ytrain2 <- merge(x = ytrain, y = activity_labels, by.x = "V1", sort = FALSE)
names(ytrain2) <- c("ActivityCode", "Activity")
train_set <- cbind(subject_train, ytrain2[2], xtrain2)

#Combine train_set and test_set
combined_set <- rbind(train_set, test_set)

#Solution using DPLYR package
tbl_combined_set <- tbl_df(combined_set)
by_sbj <- group_by(tbl_combined_set, Subject)
by_sbj_by_act <- group_by(by_sbj, Activity)
by_sbj_by_act
by_activity <- group_by(tbl_combined_set, Activity)

#(Stacking data)Solution using TIDYR package 
gathered <- gather(data = tbl_combined_set, key = Feature, Value, -Subject, -Activity)

#(Stacking data)Solution using RESHApE2 package
melted <- melt(tbl_combined_set, id=c("Subject", "Activity"))

#Solution using AGGREGATE:
aggr_melt <- aggregate(value ~ Subject+Activity+variable, mean, data=melted)
aggr_melt <- aggr_melt[order(aggr_melt$Subject),]
rownames(aggr_melt) <- seq(length=nrow(aggr_melt))
names(aggr_melt) <- c("Subject", "Activity", "Feature", "Mean") #Melt - Solution

aggr_gather <- aggregate(Value ~ Subject+Activity+Feature, mean, data=gathered)
aggr_gather <- aggr_gather[order(aggr_gather$Subject),]
names(aggr_gather) <- c("Subject", "Activity", "Feature", "Mean")
names(aggr_gather) #Gather - Solution
##I can see the result is same (order for Variable is different, but the values are the same)

#Solution using PLYR package:
means <- ddply(melted, .(Subject, Activity, variable), summarize, mean=mean(value))
names(means) <- c("Subject", "Activity", "Feature", "Mean")
names(means)

#Export results
write.table(means, "./Means_plyr.txt", row.names=FALSE)
write.table(aggr_gather, "./Means_aggr_gather.txt", row.names=FALSE)
write.table(aggr_melt, "./Means_aggr_melt.txt", row.names=FALSE)

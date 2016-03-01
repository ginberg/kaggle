# Telstra Kaggle competition

library(ggplot2)
library(randomForest)
library(xgboost)
source("multiLogLoss.R")

#load data
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)
#severity type of a warning message coming from the log
severity_type <- read.csv("severity_type.csv", stringsAsFactors=FALSE)
#type of resource related to the main dataset
resource <- read.csv("resource_type.csv", stringsAsFactors=FALSE)
colnames(resource) <- c("id", "resource")
#event type related to the main dataset
event <- read.csv("event_type.csv", stringsAsFactors=FALSE)
#features extracted from log files
log_feature <- read.csv("log_feature.csv", stringsAsFactors=FALSE)
sample_submission <- read.csv("sample_submission.csv", stringsAsFactors=FALSE)

# merge data to 1 dataframe
mergeDF <- function(x, y){
  y_train <- y[y$id %in% x$id,]
  y_train <- aggregate(y_train[-1], by=list(y_train$id), c)
  colnames(y_train) <- c("id")
  df <- merge(x, y_train, by= "id")
  return(df)
}
df <- Reduce(mergeDF, list(train, severity_type, resource, event, log_feature))
colnames(df) <- c("id", "location", "fault_severity", "severity_type", "resource", "event", "log_features", "volumes")
df$fault_severity <- as.factor(df$fault_severity)
df_test <- Reduce(mergeDF, list(test, severity_type, resource, event, log_feature))
colnames(df_test) <- c("id", "location", "severity_type", "resource", "event", "log_features", "volumes")

addResourceColumns = function(df){
  resource_levels <- levels(factor(unlist(df$resource)))
  df_res_types <- as.data.frame(do.call(rbind, lapply(lapply(df$resource, factor, resource_levels), table)))
  colnames(df_res_types) <- gsub('resource_type ', 'res', colnames(df_res_types))
  event_levels <- levels(factor(unlist(df$event)))
  df_event_types <- as.data.frame(do.call(rbind, lapply(lapply(df$event, factor, event_levels), table)))
  colnames(df_event_types) <- gsub('event_type ', 'evt', colnames(df_event_types))
  df <- cbind(df, df_res_types)
  df <- cbind(df, df_event_types)
  return(df)
}
#df <- addResourceColumns(df)
#df_test <- addResourceColumns(df_test)

addResourceLengthColumn = function(df){
  length <- lapply(df$resource, length)
  length_df <- data.frame(Reduce(rbind, length))
  colnames(length_df) <- c("res.size")
  df <- cbind(df, length_df)
}
df <- addResourceLengthColumn(df)
df_test <- addResourceLengthColumn(df_test)

addEventLengthColumn = function(df){
  length <- lapply(df$event, length)
  length_df <- data.frame(Reduce(rbind, length))
  colnames(length_df) <- c("event.size")
  df <- cbind(df, length_df)
}
df <- addEventLengthColumn(df)
df_test <- addEventLengthColumn(df_test)

addLogfeaturesColumn = function(df){
  length <- lapply(df$log_features, length)
  length_df <- data.frame(Reduce(rbind, length))
  colnames(length_df) <- c("feat.size")
  df <- cbind(df, length_df)
}
df <- addLogfeaturesColumn(df)
df_test <- addLogfeaturesColumn(df_test)

addTotalVolColumn = function(df){
  length <- lapply(df$volumes, sum)
  length_df <- data.frame(Reduce(rbind, length))
  colnames(length_df) <- c("tot.vol")
  df <- cbind(df, length_df)
}
df <- addTotalVolColumn(df)
df_test <- addTotalVolColumn(df_test)

addLocationClassColumn = function(df){
  loc <- round(strtoi(gsub("location ", "", df$location))/100)
  locclass_df <- data.frame(Reduce(rbind, loc))  
  colnames(locclass_df) <- c("loc")  
  df <- cbind(df, locclass_df)
}
df <- addLocationClassColumn(df)
df_test <- addLocationClassColumn(df_test)

df_save <- df[,c("id", "location", "fault_severity", "severity_type", "res.size", "event.size", "feat.size", "tot.vol", "loc")]
write.csv(df_save, "train_merged.csv", row.names = FALSE)

#fault_severity = output variable
#ggplot(data = df) + geom_histogram(aes(x=fault_severity))

#severity_type (from logs)
#ggplot(data = df) + geom_histogram(aes(x=severity_type))
#ggplot(data = resource_type) + geom_histogram(aes(x=resource_type))

ggplot(data=df, aes(x=as.factor(res.size))) + 
  geom_histogram() 

ggplot(data = df, aes(x=factor(fault_severity), y=feat.size)) + 
  geom_jitter() + geom_boxplot()

set.seed(1)

extractFeatures <- function(data) {
  #features <- c("severity_type", "location", "event.size", "feat.size", "tot.vol", "loc")
  features <- c("event.size", "feat.size", "tot.vol", "loc")
  fea <- data[,features]
  fea$severity_type <- as.factor(fea$severity_type)
  fea$location <- strtoi(gsub('location ', '', fea$location))
  return(fea)
}

feat_train <- extractFeatures(df)
rf <- randomForest(feat_train, df$fault_severity)

submission <- as.data.frame(df_test[, c("id")])
colnames(submission) <- c("id")
predictions <- as.data.frame(predict(rf, extractFeatures(df_test), type="prob"))
submission$predict_0 <- predictions$"0"
submission$predict_1 <- predictions$"1"
submission$predict_2 <- predictions$"2"

#create output
write.csv(submission, file = "submissions/random_forest_r_submission6.csv", row.names=FALSE)


#xgboost
feat_train2 <- feat_train[,c("location", "event.size","feat.size","tot.vol","loc")]
labels <- as.numeric(levels(df$fault_severity)) [df$fault_severity]
train2<-as.matrix(feat_train2)
train2<- matrix(as.numeric(train2),nrow(train2),ncol(train2))

xgb <- xgboost(data = train2,  
               label = labels, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3)

y_pred <- predict(xgb, extractFeatures(df_test), type="prob"))
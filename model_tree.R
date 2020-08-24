## MODEL

# * training data -----------------------------------------
# Normalization not required as numbers are "well-behaved" and no algorithm performance increase is detected
trainingData = data_df %>% dplyr::select(conversion,channel,device,event_count,session_duration_seconds) 

trainingLabel = rep(0,nrow(trainingData))

trainingLabel[trainingData$conversion] = 1

trainingData = trainingData %>% dplyr::select(-conversion)

# make factors for encoding
features = colnames(trainingData)
for (f in features){
  if( (class(trainingData[[f]]) == "character") || (class(trainingData[[f]]) == "factor"))
  {
    levels = unique(trainingData[[f]])
    trainingData[[f]] = factor(trainingData[[f]], level = levels)
  }
}

dummies <- dummyVars( ~ ., data = trainingData)
data_encoded = predict(dummies, newdata = trainingData) 

# * split into test and train ----------------------------
# use 75/25 split
test_indx = sample(1:nrow(data_encoded), floor(nrow(data_encoded)*0.25), replace=FALSE)

training_df = data_encoded[which(! (1:nrow(data_encoded) %in% test_indx)),]
test_df     = data_encoded[test_indx,]

training_label = trainingLabel[-test_indx]
test_label     = trainingLabel[test_indx]

if (!(nrow(test_df) + nrow(training_df) == nrow(trainingData))){
  stop("training data split not correct")
}
if (!(length(test_label) + length(training_label) == length(trainingLabel))){
  stop("training label split not correct")
}

negative_cases = sum(training_label == 0)
postive_cases = sum(training_label == 1)

# todo if time: use cross-validation here 

modeltree <- xgboost(data = training_df, label = training_label, 
                     max.depth = 8, nthread = 2,
                     nrounds = 200, 
                     objective = "binary:logistic",#eval_metric = "auc",
                     early_stopping_rounds = 5,
                     scale_pos_weight = negative_cases/postive_cases)


# * predictions ------------------------
pred = predict(modeltree,test_df)


c_threshold = 0.5

# quick prints
result = sum(as.numeric(pred>c_threshold)==test_label)/length(pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))

err <- mean(as.numeric(pred >c_threshold) != test_label)
print(paste("test-error=", err))

#confusion matrix
confusionMatrix( table(ifelse(pred >= c_threshold, 1, 0), test_label))

# feature importance
importance_matrix = xgb.importance(dimnames(training_df)[[2]],model = modeltree)
xgb.plot.importance(importance_matrix)

# roc 
pred_ROCR = prediction(pred, test_label)
perf_ROCR = performance(pred_ROCR, "tpr", "fpr")
# xgb.perf = performance(xgb.pred, "prec", "rec")

# plotdf = data.frame(tpr = xgb.perf@x.values[[1]], fpr = xgb.perf@y.values[[1]])
# ggplot() + geom_line(data=plotdf,aes(x=tpr,y=fpr))

auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
print(paste("auc=", auc_ROCR))

plot(perf_ROCR,
     avg="threshold",
     colorize=TRUE,
     lwd=1,
     main="ROC Curve w/ Thresholds",
     print.cutoffs.at=seq(0, 1, by=0.05),
     text.adj=c(-0.5, 0.5),
     text.cex=0.5)
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")




## ---- Load_Libraries ----
# Load libraries 
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(ROCR)
library(DMwR)
library(randomForest)

# =============================
# Function - Model Evaluation
#
# This Function will take a training and Testing set from a Logistic run
# and calculate the threshold, area under curve 
# and output plots
#
# =============================
model_evaluation = function(training = training, testing = testing, 
                            model = glm_fit,
                            target = "target",
                            plot_results = TRUE) {
  
  # create prediction object on the testing data
  testing_prediction = prediction(testing$probability, testing[, target])
  # create prediction object on the training data
  training_prediction = prediction(training$probability, training[, target])
  
  
  # Create model performance objects on test data set - uses ROCR
  test_tpr_fpr = performance(testing_prediction, "tpr","fpr")
  
  data.test.auc = performance(testing_prediction, "auc")
  data.test.lift = performance(testing_prediction, "lift", "rpp")
  # Area under the ROC curve
  auc = unlist(slot(data.test.auc, "y.values"))
  cat("\nAUC on test data is ", round(100 * auc, 2), "%", sep = "")
  # Create model performance objects on training data set - uses ROCR
  train_tpr_fpr <- performance(training_prediction, "tpr","fpr")
  
  
  # Sensitivity / Specificity charts
  
  test_sens_spec = performance(testing_prediction, "sens","spec")
  
  sens = performance(testing_prediction, "sens")
  spec = performance(testing_prediction, "spec")
  
  # Plot the tpr and fpr gains chart ROC for both testing and training data
  if (plot_results) {
    plot(test_tpr_fpr, main = "Gains Chart ROC", type = "l", col = "red", lwd = 2, 
         xlim = c(0,1), ylim = c(0,1))
    plot(train_tpr_fpr, add = T, col = "blue", lwd = 2, lty = 2, xlim = c(0,1), ylim = c(0,1))
    legend("bottomright", legend = c("Training","Testing"), col = c("blue","red"), lty = 1, lwd = 2)
    abline(0,1, col = "darkgray")
    grid()
    
    
    plot(sens, 
         main = "Sensitivity Specificity Chart", type = "l", col = "red", lwd = 2, 
         xlim = c(0,1), ylim = c(0,1), 
         ylab = "Values")
    axis(side = 1, at = seq(0, 1, 0.1))
    axis(side = 2, at = seq(0, 1, 0.1))
    plot(spec, add = T, col = "blue", lwd = 2, 
         xlim = c(0,1), ylim = c(0,1)
    )
    legend("bottomright", legend = c("Sensitivity","Specificity"), col = c("red", "blue"), lty = 1, lwd = 2)
    abline(h = seq(0, 1, 0.1), v = seq(0, 1, 0.1), col="gray", lty=3)
    
  }
  
  # Probability Threshold calculation
  threshold.df = data.frame(cut = test_sens_spec@alpha.values[[1]], 
                            sens = test_sens_spec@x.values[[1]],
                            spec = test_sens_spec@y.values[[1]])
  threshold = threshold.df[which.max(threshold.df$sens + threshold.df$spec), "cut"]
  cat("\nProbability threshold is", threshold)
  cat("\n")
  return(data.frame(threshold, auc))
}


# =================================
# Initial Load of data and cleaning
# =================================
# Set working directory to read transactions.csv
setwd("/Users/mmrpr1/Documents/Uni/DAM/Assignment_2/AT2_STUDENTS/")
transactions <- read.csv("AT2_credit_train_STUDENT.csv", stringsAsFactors = FALSE)

# ==============
# EDA
# ==============
# Look at percentage of defaults
prop.table(table(transactions$default))

# EXplore Education, Limit Bal, Sex, Marriage and Age variables
attach(transactions)

# Produce contingencey table and bar chart for default and LIMIT_BAL
tab=table(default,LIMIT_BAL)

ggplot(transactions,aes(x=factor(LIMIT_BAL),fill=default))+
  geom_bar()+theme(axis.text.x=element_text(angle=90))+
  xlab("Limit Balance")

# A proportion table (with condition on columns)and proportion bar chart provide further insight.
prop.table (tab,2)
ggplot(transactions,aes(x=factor(LIMIT_BAL),fill=default))+
  geom_bar(position="fill")+theme(axis.text.x=element_text(angle=90))+
  ylab("Proportions")

# Repeat the above EDA in relation to default and gender.
tab=table(default,SEX)

ggplot(transactions,aes(x=factor(SEX),fill=default))+
  geom_bar(position="dodge")

prop.table (tab,2)
ggplot(transactions,aes(x=factor(SEX),fill=default))+
  geom_bar(position="fill")+
  ylab("Proportions")

# Education
tab=table(default,EDUCATION)
ggplot(transactions,aes(x=factor(EDUCATION),fill=default))+
  geom_bar(position="dodge")

prop.table (tab,2)
ggplot(transactions,aes(x=factor(EDUCATION),fill=default))+
  geom_bar(position="fill")+
  ylab("Proportions") + xlab("Education")

# Marriage
tab=table(default,MARRIAGE)
tab
ggplot(transactions,aes(x=factor(MARRIAGE),fill=default))+
  geom_bar(position="dodge")

prop.table (tab,2)
ggplot(transactions,aes(x=factor(MARRIAGE),fill=default))+
  geom_bar(position="fill")+
  ylab("Proportions")

# Age
tab=table(default,AGE)

ggplot(transactions[transactions$AGE < 100,],aes(x=AGE,fill=default))+
  geom_bar()+theme(axis.text.x=element_text(angle=90)) +
  xlab("Age")

prop.table (tab,2)
ggplot(transactions,aes(x=AGE,fill=default))+
  geom_bar(position="fill")+
  ylab("Proportions")


# ====================
# Data Cleaning
# ====================
# Convert cat/dog/dolphin to category 0 for Sex
transactions$SEX <-  ifelse(transactions$SEX %in% c("1", "2"), transactions$SEX, "0")
# Any Age over 100, minus 100
transactions$AGE <-  ifelse(transactions$AGE > 100, transactions$AGE-100, transactions$AGE)

# combine Education 0, 5, 6 to one group
transactions$EDUCATION <-  ifelse(transactions$EDUCATION > 4, 0, transactions$EDUCATION)
# combine Marriage 0, 3 to one group
transactions$MARRIAGE <-  ifelse(transactions$MARRIAGE > 2, 0, transactions$MARRIAGE)

trainset_size <- floor(0.80 * nrow(transactions))

# ===================
# Model 1 - Random Forest
# ===================
# Set default as 0, 1 and convert to factor
transactions_rf <- transactions
# Convert default from Y/N to 1/0 and then make a factor
transactions_rf$default <- ifelse(transactions_rf$default %in% c("Y"), 1, 0)
transactions_rf$default <- as.factor(transactions_rf$default)

# Sex, Education and Marriage are categorical so convert to Factors
transactions_rf$SEX <- as.factor(transactions_rf$SEX)
transactions_rf$EDUCATION <- as.factor(transactions_rf$EDUCATION)
transactions_rf$MARRIAGE <- as.factor(transactions_rf$MARRIAGE)

# loop through 10 runs 
nruns <- 1

summaryStats <- data.frame()
trainset_size <- floor(0.80 * nrow(transactions_rf))
typeColNum <- grep("default",names(transactions_rf))
set.seed(1)

for (i in 1:nruns){

  trainset_indices <- sample(seq_len(nrow(transactions_rf)), size = trainset_size)
  
  # assign observations to training and testing sets
  trainset <- transactions_rf[trainset_indices, ]
  testset <- transactions_rf[-trainset_indices, ]
             
  #res <- tuneRF(x = subset(trainset, select = -default),
  #              y = trainset$default,
  #              ntreeTry = 500)
  
  # Run RandomForest and remove ID field as this is NOT a predictor
  tx.rf <- randomForest(default ~ . ,data = trainset[, !(colnames(trainset) %in% c("ID"))], 
                        importance=TRUE, ntree=1000, #xtest=testset[, !(colnames(testset) %in% c("ID", 'default'))],
                        keep.forest = T, mtry=8)
  testset$prediction <- tx.rf$test$predicted
  # Predict against Train and Test and get AUC/Threshold value
  trainset$probability = predict(tx.rf, newdata = trainset, type = "prob")[, 2]
  testset$probability = predict(tx.rf, newdata = testset, type = "prob")[, 2]
  Measures = model_evaluation(trainset, testset, rf_fit, target = "default", plot_results=FALSE)

  testset$prediction = 0
  testset[testset$probability >= Measures$threshold, "prediction"] = 1
  testset$prediction <- as.factor(testset$prediction)
  
  # Create a confusion matrix (along with other measures) using the table function
  cfm_lr <- table(predicted=testset$prediction,true=testset$default)

  tp <- cfm_lr[2,2]
  fp <- cfm_lr[2,1]
  tn <- cfm_lr[1,1]
  fn <- cfm_lr[1,2]
  
  # Precision = TP/(TP+FP)
  precision <-tp/(tp + fp)
  
  # Recall = TP/(TP+FN)
  recall <- tp/(tp + fn)
  
  # F1
  f1 <- round(2*(precision*recall/(precision+recall)) * 100, 2)
  precision <- round(precision * 100, 2)
  
  # Recall = TP/(TP+FN)
  recall <- round(recall * 100, 2)

  accuracy <- round(mean(testset$prediction==testset$default)*100, 2)
  prediction_rate <- round((tp+fp)/(tp+fp+tn+fn) * 100, 2)
  actual_rate <- round((tp+fn)/(tp+fp+tn+fn) * 100, 2)
  auc <- round(Measures$auc * 100, 2)
  print(mean(testset$prediction==testset$default))
  
  summaryStats <- rbind(summaryStats, data.frame(auc, tp, fp, tn, fn, precision, recall, f1, accuracy, actual_rate, prediction_rate))
}

summaryStats

# Get AUC plot for last run
Measures = model_evaluation(trainset, testset, rf_fit, target = "default", plot_results=TRUE)

# quantitative measure of variable importance for last run
importance(tx.rf)
# sorted plot of importance for last run
varImpPlot(tx.rf)

# Plot Partial Dependency plots
partialPlot(tx.rf, testset, x.var = "AGE", which.class = "1")
partialPlot(tx.rf, testset, x.var = "PAY_PC1", which.class = "1")
partialPlot(tx.rf, testset, x.var = "LIMIT_BAL", which.class = "1")
partialPlot(tx.rf, testset, x.var = "AMT_PC1", which.class = "1")

# ============================
#
# Rebuild Random Forest for Final Predictions
#
# ============================
# Set up Transactions_rf again
transactions_rf <- transactions
transactions_rf$default <- ifelse(transactions_rf$default %in% c("Y"), 1, 0)
transactions_rf$default <- as.factor(transactions_rf$default)
transactions_rf$SEX <- as.factor(transactions_rf$SEX)
transactions_rf$EDUCATION <- as.factor(transactions_rf$EDUCATION)
transactions_rf$MARRIAGE <- as.factor(transactions_rf$MARRIAGE)

# Read in Validation set and make sure all are numeric and Sex, Marriage, Education are Factors
transactions_val <- read.csv("AT2_credit_test_STUDENT.csv")
transactions_val$AGE <- as.numeric(transactions_val$AGE)
transactions_val$LIMIT_BAL <- as.numeric(transactions_val$LIMIT_BAL)
# Combine 0, 5 and 6 to 1 factor in Education
transactions_val$EDUCATION <-  ifelse(transactions_val$EDUCATION > 4, 0, transactions_val$EDUCATION)
# combine education 0, 5, 6 to one group
transactions_val$MARRIAGE <-  ifelse(transactions_val$MARRIAGE > 2, 0, transactions_val$MARRIAGE)

transactions_val$SEX <- as.factor(transactions_val$SEX)
transactions_val$EDUCATION <- as.factor(transactions_val$EDUCATION)
transactions_val$MARRIAGE <- as.factor(transactions_val$MARRIAGE)

# Ensure the factors for Sex have 0, 1, 2 in the final set
transactions_val$SEX <- factor(transactions_val$SEX, levels = levels(transactions_rf$SEX))

tx.rf = tuneRF (x=transactions_rf[, !(colnames(transactions_rf) %in% c("ID","default"))],y=transactions_rf$default,ntreeTry=1000,
              doBest=TRUE)

# Get Threshold for calculating Prediction
transactions_rf$probability = predict(tx.rf, newdata = transactions_rf, type = "prob")[, 2]
transactions_val$probability = predict(tx.rf, newdata = transactions_val, type = "prob")[, 2]
Measures = model_evaluation(transactions_rf, transactions_rf, rf_fit, target = "default", plot_results=FALSE)

transactions_val$prediction = 0
transactions_val[transactions_val$probability >= Measures$threshold, "prediction"] = 1

# Add Prediction to data frame
test_predictions_rf <- data.frame(transactions_val$ID,transactions_val$probability )
# Check rates of prediction are in line with Testing (about 25%)
prop.table(table(transactions_val$prediction))

# Finally, write out predictions
colnames(test_predictions_rf) <- c("ID", "default")
write.csv(test_predictions_rf, "AT2_credit_sample_UPLOAD_RF.csv", row.names = FALSE)


# ========================
# Model 2 - Support Vector Machine (SVM)
# ========================
library(e1071)

transactions_svm <- transactions
# Convert default to 1/0 but do not make a factor yet 
transactions_svm$default <- ifelse(transactions_svm$default %in% c("Y"), 1, 0)
# Make Sex, Education and Marriage Factors and then convert to dummy variables
# as SVM needs numeric data to work on
transactions_svm$SEX <- as.factor(transactions_svm$SEX)
transactions_svm$EDUCATION <- as.factor(transactions_svm$EDUCATION)
transactions_svm$MARRIAGE <- as.factor(transactions_svm$MARRIAGE)
dmy <- dummyVars(" ~ . ", data = transactions_svm,fullRank = F)
transactions_svm <- data.frame(predict(dmy, newdata = transactions_svm))
# Then make default a factor
transactions_svm$default <- as.factor(transactions_svm$default)

# loop through 10 runs again
nruns <- 5
str(transactions_svm)
summaryStats <- data.frame()
set.seed(1)
trainset_indices <- sample(seq_len(nrow(transactions_svm)), size = trainset_size)
for (i in 1:nruns){

  trainset_indices <- sample(seq_len(nrow(transactions_svm)), size = trainset_size)
  # assign observations to training and testing sets
  trainset <- transactions_svm[trainset_indices, ]
  testset <- transactions_svm[-trainset_indices, ]
  trainset$default <- as.factor(trainset$default)
  dn_train <- downSample(x = trainset[, !(colnames(trainset) %in% c("default"))],
                         y = trainset$default, yname="default")
 
  #Tune gamma..this may take some time
  tune_out <- tune.svm(x=dn_train[, !(colnames(dn_train) %in% c("ID", "default"))],y=dn_train[, (colnames(dn_train) %in% c("default"))],
                       gamma=c(0.01, 0.1),cost=c(1, 10),kernel="radial")

  # Run an SVM Model using the best Cost and gamma from tuning
  svm_model<- svm(default ~ ., data=dn_train[, !(colnames(dn_train) %in% c("ID"))], method="C-classification", 
                  kernel="radial",cost=tune_out$best.parameters$cost,gamma=tune_out$best.parameters$gamma, probability = TRUE)
  
   pred_train <- predict(svm_model,trainset, probability = TRUE)
   pred_test <- predict(svm_model,testset, probability = TRUE)
   # Get probabilities to calculate AUC and then use the threshold to set the prediction
   trainset$probability <- attr(pred_train, "prob")[, 2]
   testset$probability <- attr(pred_test, "prob")[, 2]
   Measures = model_evaluation(trainset, testset, svm_model, target = "default", plot_results=FALSE)

   testset$prediction = 0
   testset[testset$probability >= Measures$threshold, "prediction"] = 1

   # Create a confusion matrix (along with other measures) using the table function
   cfm_lr <- table(predicted=testset$prediction,true=testset$default)
   
   tp <- cfm_lr[2,2]
   fp <- cfm_lr[2,1]
   tn <- cfm_lr[1,1]
   fn <- cfm_lr[1,2]
   
   # Precision = TP/(TP+FP)
   precision <-tp/(tp + fp)
   
   # Recall = TP/(TP+FN)
   recall <- tp/(tp + fn)
   
   # F1
   f1 <- round(2*(precision*recall/(precision+recall)) * 100, 2)
   precision <- round(precision * 100, 2)
   
   # Recall = TP/(TP+FN)
   recall <- round(recall * 100, 2)
   
   accuracy <- round(mean(testset$prediction==testset$default)*100, 2)
   prediction_rate <- round((tp+fp)/(tp+fp+tn+fn) * 100, 2)
   actual_rate <- round((tp+fn)/(tp+fp+tn+fn) * 100, 2)
   auc <- round(Measures$auc * 100, 2)
   print(mean(testset$prediction==testset$default))
   
   summaryStats <- rbind(summaryStats, data.frame(auc, tp, fp, tn, fn, precision, recall, f1, accuracy, actual_rate, prediction_rate))
}

summaryStats

# ============================
#
# Rebuild SVM for Final Predictions
#
# ============================
# Reset transactions_svm for final run
transactions_svm <- transactions
# Convert default to 1/0 but do not make a factor yet 
transactions_svm$default <- ifelse(transactions_svm$default %in% c("Y"), 1, 0)
# Make Sex, Education and Marriage Factors and then convert to dummy variables
# as SVM needs numeric data to work on
transactions_svm$SEX <- as.factor(transactions_svm$SEX)
transactions_svm$EDUCATION <- as.factor(transactions_svm$EDUCATION)
transactions_svm$MARRIAGE <- as.factor(transactions_svm$MARRIAGE)
dmy <- dummyVars(" ~ . ", data = transactions_svm,fullRank = F)
transactions_svm <- data.frame(predict(dmy, newdata = transactions_svm))
# Then make default a factor
transactions_svm$default <- as.factor(transactions_svm$default)

#Tune gamma and cost on Training set
tune_out <- tune.svm(x=transactions_svm[, !(colnames(transactions_svm) %in% c("ID", "default"))],y=transactions_svm[, (colnames(transactions_svm) %in% c("default"))],
                      gamma=c(0.01, 0.1),cost=c(1, 10),kernel="radial")

# Run Against Validation set
transactions_val <- read.csv("AT2_credit_test_STUDENT.csv")
transactions_val$AGE <- as.numeric(transactions_val$AGE)
transactions_val$LIMIT_BAL <- as.numeric(transactions_val$LIMIT_BAL)
# Combine 0, 5 and 6 to 1 factor in Education
transactions_val$EDUCATION <-  ifelse(transactions_val$EDUCATION > 4, 0, transactions_val$EDUCATION)
# combine education 0, 5, 6 to one group
transactions_val$MARRIAGE <-  ifelse(transactions_val$MARRIAGE > 2, 0, transactions_val$MARRIAGE)

transactions_val$SEX <- as.factor(transactions_val$SEX)
transactions_val$EDUCATION <- as.factor(transactions_val$EDUCATION)
transactions_val$MARRIAGE <- as.factor(transactions_val$MARRIAGE)

dmy <- dummyVars(" ~ . ", data = transactions_val,fullRank = F)
transactions_val <- data.frame(predict(dmy, newdata = transactions_val))
# Set dummy variable for Sex=0 that is in training set
transactions_val$SEX.0 <- 0

# Run svm against full training set 
tune_out$best.parameters$cost 
tune_out$best.parameters$gamma 
svm_model<- svm(default ~ ., data=transactions_svm[, !(colnames(transactions_svm) %in% c("ID"))], method="C-classification", 
                kernel="radial",cost=tune_out$best.parameters$cost,gamma=tune_out$best.parameters$gamma, probability = TRUE)
pred_test = predict(svm_model, newdata = transactions_svm)
pred_train <- predict(svm_model,transactions_svm, probability = TRUE)
pred_test <- predict(svm_model,transactions_val, probability = TRUE)

# Get AUC and Threshold for calculating Prediction
transactions_svm$probability <- attr(pred_train, "prob")[, 2]
transactions_val$probability <- attr(pred_test, "prob")[, 2]
Measures = model_evaluation(transactions_svm, transactions_svm, svm_model, target = "default", plot_results=FALSE)

transactions_val$prediction = 0
# Modify the probability threshold to see if you can get a better accuracy
transactions_val[transactions_val$probability >= Measures$threshold, "prediction"] = 1

test_predictions_svm <- data.frame(transactions_val$ID,transactions_val$prediction )
# Check rates of prediction are in line with Testing (about 25%)
prop.table(table(transactions_val$prediction))

# Finally, write out predictions
colnames(test_predictions_svm) <- ColnameBat <- c("ID", "default")
write.csv(test_predictions_svm, "AT2_credit_sample_UPLOAD_SVM.csv", row.names = FALSE)

# ======================
# Model 3 - Gradient Boosting Machine (GBM)
# ======================
library(gbm)

transactions_gbm <- transactions
# Convert default to numeric 1 = yes, 0 = no
transactions_gbm$default <- ifelse(transactions_gbm$default %in% c("Y"), 1, 0)
# Convert Sex, Education and Marriage to dummy numeric variables for GBM
transactions_gbm$SEX <- as.factor(transactions_gbm$SEX)
transactions_gbm$EDUCATION <- as.factor(transactions_gbm$EDUCATION)
transactions_gbm$MARRIAGE <- as.factor(transactions_gbm$MARRIAGE)
dmy <- dummyVars(" ~ . ", data = transactions_gbm,fullRank = F)
transactions_gbm <- data.frame(predict(dmy, newdata = transactions_gbm))
# Then convert default to a factor
transactions_gbm$default <- as.factor(transactions_gbm$default)

# loop through 10 runs again
nruns <- 10

summaryStats <- data.frame()
set.seed(1)

# defining some parameters
gbm_depth = 10 #maximum nodes per tree
gbm_n.min = 15 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.001 #learning rate
cores_num = 8 #number of cores
gbm_cv.folds=5 #number of cross-validation folds to perform
num_trees = 1000

for (i in 1:nruns){
  
  trainset_indices <- sample(seq_len(nrow(transactions_gbm)), size = trainset_size)
  # assign observations to training and testing sets
  
  trainset <- transactions_gbm[trainset_indices, ]
  testset <- transactions_gbm[-trainset_indices, ]

  gbm_clf = gbm(trainset$default~.,
                data=trainset[, !(colnames(trainset) %in% c("ID", "default"))],
                distribution='bernoulli', #continuous response
                n.trees=num_trees, #the number of GBM interaction
                interaction.depth= gbm_depth,
                n.minobsinnode = gbm_n.min, 
                shrinkage=gbm_shrinkage, 
                train.fraction = 0.5,
                cv.folds=gbm_cv.folds,
                verbose = FALSE, #print the preliminary output
                n.cores = cores_num
  )
  
  # Get AUC and Threshold for determining Prediction
  testset$probability = predict(gbm_clf, testset, n.trees = best.iter, type = "response")
  trainset$probability = predict(gbm_clf, trainset, n.trees = best.iter, type = "response")
  Measures = model_evaluation(trainset, testset, gbm_clf, target = "default", plot_results=FALSE)

  testset$prediction = 0
  testset[testset$probability >= 0.5, "prediction"] = 1
  
  # Create a confusion matrix (along with other measures) using the table function
  cfm_lr <- table(predicted=testset$prediction,true=testset$default)
  
  tp <- cfm_lr[2,2]
  fp <- cfm_lr[2,1]
  tn <- cfm_lr[1,1]
  fn <- cfm_lr[1,2]
  
  # Precision = TP/(TP+FP)
  precision <-tp/(tp + fp)
  
  # Recall = TP/(TP+FN)
  recall <- tp/(tp + fn)
  
  # F1
  f1 <- round(2*(precision*recall/(precision+recall)) * 100, 2)
  precision <- round(precision * 100, 2)
  
  # Recall = TP/(TP+FN)
  recall <- round(recall * 100, 2)
  
  accuracy <- round(mean(testset$prediction==testset$default)*100, 2)
  prediction_rate <- round((tp+fp)/(tp+fp+tn+fn) * 100, 2)
  actual_rate <- round((tp+fn)/(tp+fp+tn+fn) * 100, 2)
  auc <- round(Measures$auc * 100, 2)
  print(mean(testset$prediction==testset$default))
  
  summaryStats <- rbind(summaryStats, data.frame(auc, tp, fp, tn, fn, precision, recall, f1, accuracy, actual_rate, prediction_rate))
}

summaryStats

best.iter = gbm.perf(gbm_clf, method = "cv")

# gives the variable importance in a graph
summary(gbm_clf,n.trees=best.iter, ylab = "Variable", main = "Variable Relative Importance")
#OR just as a table
summary(gbm_clf)

# ============================
#
# Rebuild GBM for Final Predictions
#
# ============================
# Get Training set again and convert to dummy variables
transactions_gbm <- transactions
# Convert default to numeric 1 = yes, 0 = no
transactions_gbm$default <- ifelse(transactions_gbm$default %in% c("Y"), 1, 0)
# Convert Sex, Education and Marriage to dummy numeric variables for GBM
transactions_gbm$SEX <- as.factor(transactions_gbm$SEX)
transactions_gbm$EDUCATION <- as.factor(transactions_gbm$EDUCATION)
transactions_gbm$MARRIAGE <- as.factor(transactions_gbm$MARRIAGE)
dmy <- dummyVars(" ~ . ", data = transactions_gbm,fullRank = F)
transactions_gbm <- data.frame(predict(dmy, newdata = transactions_gbm))

# Run Against Validation set
transactions_val <- read.csv("AT2_credit_test_STUDENT.csv")
transactions_val$AGE <- as.numeric(transactions_val$AGE)
transactions_val$LIMIT_BAL <- as.numeric(transactions_val$LIMIT_BAL)
# Combine 0, 5 and 6 to 1 factor in Education
transactions_val$EDUCATION <-  ifelse(transactions_val$EDUCATION > 4, 0, transactions_val$EDUCATION)
# combine education 0, 5, 6 to one group
transactions_val$MARRIAGE <-  ifelse(transactions_val$MARRIAGE > 2, 0, transactions_val$MARRIAGE)

transactions_val$SEX <- as.factor(transactions_val$SEX)
transactions_val$EDUCATION <- as.factor(transactions_val$EDUCATION)
transactions_val$MARRIAGE <- as.factor(transactions_val$MARRIAGE)

dmy <- dummyVars(" ~ . ", data = transactions_val,fullRank = F)
transactions_val <- data.frame(predict(dmy, newdata = transactions_val))

set.seed(1)

# defining some parameters
gbm_depth = 10 #maximum nodes per tree
gbm_n.min = 15 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.001 #learning rate
cores_num = 8 #number of cores
gbm_cv.folds=5 #number of cross-validation folds to perform
num_trees = 1000

# Run the GBM model
gbm_clf = gbm(transactions_gbm$default~.,
              data=transactions_gbm[, !(colnames(transactions_gbm) %in% c("ID", "default"))],
              distribution='bernoulli', #continuous response
              n.trees=num_trees, #the number of GBM interaction
              interaction.depth= gbm_depth,
              n.minobsinnode = gbm_n.min, 
              shrinkage=gbm_shrinkage, 
              train.fraction = 0.5,
              cv.folds=gbm_cv.folds,
              verbose = FALSE, #print the preliminary output
              n.cores = cores_num
)

# Get AUC and threshold for Predictions
transactions_val$probability = predict(gbm_clf, transactions_val, n.trees = best.iter, type = "response")
transactions_gbm$probability = predict(gbm_clf, transactions_gbm, n.trees = best.iter, type = "response")
Measures = model_evaluation(transactions_gbm, transactions_gbm, gbm_clf, target = "default", plot_results=FALSE)

transactions_val$prediction = 0
transactions_val[transactions_val$probability >= Measures$threshold, "prediction"] = 1

# Add Prediction and ID to data frame
test_predictions_gbm <- data.frame(transactions_val$ID,transactions_val$prediction )
# Check rates of prediction are in line with Testing (about 25%)
prop.table(table(transactions_val$prediction))

# Finally, write out predictions
colnames(test_predictions_gbm) <- ColnameBat <- c("ID", "default")
write.csv(test_predictions_gbm, "AT2_credit_sample_UPLOAD_GBM.csv", row.names = FALSE)


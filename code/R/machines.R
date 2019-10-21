library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(randomForest)
library(xgboost)
library(gbm)
library(caret)
library(ggcorrplot)

setwd("/GitDev/Footyapp/data/") # use here function instead

footy_match_play <- read.csv("FootyMatchPlayFull.csv", stringsAsFactors = FALSE)

# Column for joining to create percentages over the 
# 2 teams in a game
footy_match_play$joinColumn <- 1

# To work out a percentage between the 2 teams,
# processing duplicates each row by essentially doing
# a cross-join to the duplicate table.
# Then column_pcnt = column / sum(column)
# Processing will then filter out the duplicate and
# be left with the percentages.
joinColumn <- c(1, 1)
UsTeam <- c(1, 2)
duplicate <- data.frame(joinColumn, UsTeam)

# Reduce game date to just the date portion
footy_match_play$gameDate = substr(footy_match_play$gameDate, 1, 10)

# Steps to create percentages:
# 1. Summarise plays up to 1 row per team per game
# 2. Create columns such as velocity and ppm
# 3. Do cross-join to duplicate rows
# 4. Do windowing to work out percentage over the 2 teams in a game
# 5. Remove duplicate rows
footy_match <- footy_match_play %>%
  #  filter(gameID == "-LE6m4eLGNTqcr9JmgTQ") %>%
  group_by(competition, gameID, teamName, playByUs, joinColumn, matchName, gameDate,
           ourName, theirName) %>%
  summarise_at(c("distance","attackDistance", "midfieldDistance",
                 "defenceDistance", "isAttackIncursion", 
                 "isPenaltyIncursion", "isZone17Incursion", "isZone14Incursion",
                 "isZone11PassIncursion", "totalPasses", "totalBackPasses",
                 "totalFwdPasses", "totalSidePasses", "total10mPasses",
                 "total10_20mPasses", "total20mPasses","possessionDuration",
                 "AttackDuration", "MidfieldDuration", "DefenceDuration",
                 "AttackHalfDuration", "DefenceHalfDuration", "playDuration",
                 "attackPasses", "midfieldPasses", "defencePasses",
                 "crossfieldPlay", "goals", "shots", "backPassTurnover",
                 "fwdPassTurnover", "sidePassTurnover", "rightAttack",
                 "middleAttack", "leftAttack", "defenceOut",
                 "MAPlay", "AMPlay", "MDPlay",
                 "DMPlay", "backKeeperPasses", "clearances",
                 "passZero", "pass1", "pass2_3", "pass4_6",
                 "pass7Plus", "shots"), sum, na.rm=TRUE) %>%
  mutate(ppm = totalPasses/(possessionDuration/60),
         aveVelocity = distance/(possessionDuration),
         zone14_17 = isZone17Incursion + isZone14Incursion,
         goal_shots = goals + shots) %>%
  left_join(duplicate, c("joinColumn" = "joinColumn")) %>%
  ungroup () %>%
  group_by(competition, gameID, UsTeam) %>%
  # Work out who won the game based on number of goals each side scored
  mutate(result = case_when(sum(goals) == 0 ~ "Draw",
                            goals/sum(goals) == 0.5 ~ "Draw",
                            goals/sum(goals) < 0.5 ~ "Loss", 
                            TRUE ~ "Win"),
         default = ifelse(result == "Draw", 1,
                          ifelse(result == "Loss", 0, 2)),
         distance_pcnt = ifelse(distance==0, 0, distance / sum(distance)),
         attackDistance_pcnt = ifelse(attackDistance==0, 0, attackDistance / sum(attackDistance)),
         midfieldDistance_pcnt = ifelse(midfieldDistance==0, 0, midfieldDistance / sum(midfieldDistance)),
         defenceDistance_pcnt = ifelse(defenceDistance==0, 0, defenceDistance / sum(defenceDistance)),
         isAttackIncursion_pcnt = ifelse(isAttackIncursion==0, 0, isAttackIncursion / sum(isAttackIncursion)),
         isPenaltyIncursion_pcnt = ifelse(isPenaltyIncursion==0, 0, isPenaltyIncursion / sum(isPenaltyIncursion)),
         isZone17Incursion_pcnt = ifelse(isZone17Incursion==0, 0, isZone17Incursion / sum(isZone17Incursion)),
         isZone14Incursion_pcnt = ifelse(isZone14Incursion==0, 0, isZone14Incursion / sum(isZone14Incursion)),
         zone14_17_pcnt = ifelse(zone14_17==0, 0, zone14_17 / sum(zone14_17)),
         isZone11PassIncursion_pcnt = ifelse(isZone11PassIncursion==0, 0, isZone11PassIncursion / sum(isZone11PassIncursion)), 
         totalPasses_pcnt = ifelse(totalPasses==0, 0, totalPasses / sum(totalPasses)), 
         totalBackPasses_pcnt = ifelse(totalBackPasses==0, 0, totalBackPasses / sum(totalBackPasses)),
         totalFwdPasses_pcnt = ifelse(totalFwdPasses==0, 0, totalFwdPasses / sum(totalFwdPasses)), 
         totalSidePasses_pcnt = ifelse(totalSidePasses==0, 0, totalSidePasses / sum(totalSidePasses)), 
         total10mPasses_pcnt = ifelse(total10mPasses==0, 0, total10mPasses / sum(total10mPasses)),
         total10_20mPasses_pcnt = ifelse(total10_20mPasses==0, 0, total10_20mPasses / sum(total10_20mPasses)), 
         total20mPasses_pcnt = ifelse(total20mPasses==0, 0, total20mPasses / sum(total20mPasses)),
         possessionDuration_pcnt = ifelse(possessionDuration==0, 0, possessionDuration / sum(possessionDuration)),
         AttackDuration_pcnt = ifelse(AttackDuration==0, 0, AttackDuration / sum(AttackDuration)), 
         MidfieldDuration_pcnt = ifelse(MidfieldDuration==0, 0, MidfieldDuration / sum(MidfieldDuration)), 
         DefenceDuration_pcnt = ifelse(DefenceDuration==0, 0, DefenceDuration / sum(DefenceDuration)),
         AttackHalfDuration_pcnt = ifelse(AttackHalfDuration==0, 0, AttackHalfDuration / sum(AttackHalfDuration)), 
         DefenceHalfDuration_pcnt = ifelse(DefenceHalfDuration==0, 0, DefenceHalfDuration / sum(DefenceHalfDuration)), 
         playDuration_pcnt = ifelse(playDuration==0, 0, playDuration / sum(playDuration)),
         attackPasses_pcnt = ifelse(attackPasses==0, 0, attackPasses / sum(attackPasses)), 
         midfieldPasses_pcnt = ifelse(midfieldPasses==0, 0, midfieldPasses / sum(midfieldPasses)), 
         defencePasses_pcnt = ifelse(defencePasses==0, 0, defencePasses / sum(defencePasses)),
         crossfieldPlay_pcnt = ifelse(crossfieldPlay==0, 0, crossfieldPlay / sum(crossfieldPlay)), 
         goals_pcnt = ifelse(goals==0, 0, goals / sum(goals)), 
         shots_pcnt = ifelse(shots==0, 0, shots / sum(shots)), 
         goal_shots_pcnt = ifelse(goal_shots==0, 0, goal_shots / sum(goal_shots)), 
         backPassTurnover_pcnt = ifelse(backPassTurnover==0, 0, backPassTurnover / sum(backPassTurnover)),
         fwdPassTurnover_pcnt = ifelse(fwdPassTurnover==0, 0, fwdPassTurnover / sum(fwdPassTurnover)), 
         sidePassTurnover_pcnt = ifelse(sidePassTurnover==0, 0, sidePassTurnover / sum(sidePassTurnover)), 
         rightAttack_pcnt = ifelse(rightAttack==0, 0, rightAttack / sum(rightAttack)),
         middleAttack_pcnt = ifelse(middleAttack==0, 0, middleAttack / sum(middleAttack)), 
         leftAttack_pcnt = ifelse(leftAttack==0, 0, leftAttack / sum(leftAttack)), 
         defenceOut_pcnt = ifelse(defenceOut==0, 0, defenceOut / sum(defenceOut)),
         MAPlay_pcnt  = ifelse(MAPlay==0, 0, MAPlay / sum(MAPlay)),
         AMPlay_pcnt = ifelse(AMPlay==0, 0, AMPlay / sum(AMPlay)),
         MDPlay_pcnt = ifelse(MDPlay==0, 0, MDPlay / sum(MDPlay)),
         DMPlay_pcnt = ifelse(DMPlay==0, 0, DMPlay / sum(DMPlay)),
         backKeeperPasses_pcnt  = ifelse(backKeeperPasses==0, 0, backKeeperPasses / sum(backKeeperPasses)),
         clearances_pcnt = ifelse(clearances==0, 0, clearances / sum(clearances)),
         passZero_pcnt  = ifelse(passZero==0, 0, passZero / sum(passZero)),
         pass1_pcnt  = ifelse(pass1==0, 0, pass1 / sum(pass1)),
         pass2_3_pcnt  = ifelse(pass2_3==0, 0, pass2_3 / sum(pass2_3)),
         pass4_6_pcnt = ifelse(pass4_6==0, 0, pass4_6 / sum(pass4_6)),
         pass7Plus_pcnt  = ifelse(pass7Plus==0, 0, pass7Plus / sum(pass7Plus)),
         ppm_pcnt = ifelse(ppm==0, 0, ppm / sum(ppm)),
         aveVelocity_pcnt = ifelse(aveVelocity==0, 0, aveVelocity / sum(aveVelocity)) ) %>%
  filter(UsTeam == 1)

# Remove game with strange stats.. 
footy_match <- footy_match[!footy_match$competition == "2017 Mens Open", ]

# Filter to columns to be used in Machine Learning Algorithms
# Features are trying to be things a team can control themselves
footy_data <- footy_match[, c("default", 
                              "totalBackPasses_pcnt", "totalFwdPasses_pcnt", "totalSidePasses_pcnt", 
                              "total10mPasses_pcnt", "total10_20mPasses_pcnt", 
                              "total20mPasses_pcnt", "crossfieldPlay_pcnt", 
                              "backPassTurnover_pcnt", "fwdPassTurnover_pcnt", 
                              "sidePassTurnover_pcnt",  "rightAttack_pcnt",
                              "middleAttack_pcnt", "leftAttack_pcnt",
                              "backKeeperPasses_pcnt", "passZero_pcnt", 
                              "pass1_pcnt", "pass2_3_pcnt", 
                              "pass4_6_pcnt", "pass7Plus_pcnt", 
                              "ppm_pcnt", "aveVelocity_pcnt",
                              "isZone11PassIncursion_pcnt"
)]

# Make the default a factor..
footy_data$default <- factor(footy_data$default)

# Create partition index
index <- createDataPartition(footy_data$default, p = 0.8, list = FALSE)
# Subset into train and test data with index
fty_train_data <- footy_data[index, ]
fty_test_data  <- footy_data[-index, ]

# Define 3x5 folds repeated cross-validation
trainControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

# Define hyperparameter grid for gbm.
gbmHyperParams <- expand.grid(n.trees = seq(from = 500, to = 2000, by = 500), 
                              interaction.depth = c(5, 10), 
                              shrinkage = 0.1, 
                              n.minobsinnode = c(5, 10))

# Train GBM model (Gradient Boosting Machine)
gbm_model <- train(default ~ ., 
                   data = fty_train_data, 
                   method = "gbm", 
                   trControl = trainControl,
                   verbose = FALSE,
                   tuneGrid = gbmHyperParams)

# Define hyperparameter grid for RandomForest.
rfHyperParams <- expand.grid(mtry = seq(from = 2, to = 18, by = 4))

# Train Random Forest model
rf_model <- train(default ~ ., 
                  data = fty_train_data, 
                  method = "rf", 
                  ntree = 2000,
                  trControl = trainControl,
                  verbose = FALSE,
                  tuneGrid = rfHyperParams)

levels(fty_train_data$default) <- c("loss", "draw", "win")

# Train XGBoost Model
xgb_model <- train(default~., 
                   data = fty_train_data,
                   method = "xgbTree",
                   trControl = trainControl,
                   search="random")


# In the Test data, predict probability of loss, draw or win
pred_rf  = predict(rf_model, fty_test_data,type='prob')
pred_gbm = predict(gbm_model, fty_test_data,type='prob')
pred_xgb = predict(xgb_model, fty_test_data,type='prob')

# Assign loss, draw or win based on whichever has the highest probability
fty_test_data$pred_rf = apply(pred_rf,1,function(x) colnames(pred_rf)[which.max(x)])
fty_test_data$pred_gbm = apply(pred_gbm,1,function(x) colnames(pred_gbm)[which.max(x)])
fty_test_data$pred_xgb = apply(pred_xgb,1,function(x) colnames(pred_xgb)[which.max(x)])

# Show confusion matrix for each model
table(fty_test_data$pred_gbm , fty_test_data$default)
table(fty_test_data$pred_xgb , fty_test_data$default)
table(fty_test_data$pred_rf , fty_test_data$default)

# estimate variable importance
importance_xgb <- varImp(xgb_model, scale=FALSE)
importance_rf <- varImp(rf_model, scale=FALSE)
importance_gbm <- varImp(gbm_model, scale=FALSE)

# summarize importance
print(importance_xgb)
print(importance_rf)
print(importance_gbm)

# plot importance
plot(importance_xgb)
plot(importance_rf)
plot(importance_gbm)

# Plot Correlation of default versus the predictor columns
mcor <- cor(x = as.numeric(footy_data$default), y = footy_data[-1], use="complete.obs")
ggcorrplot(mcor, tl.srt=45)

## ==== Cleanup ====
rm(list = ls())

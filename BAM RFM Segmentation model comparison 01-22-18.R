brokers <- read.csv('activation_data_mm_cleaned.csv', header=TRUE, sep=",",
                    stringsAsFactors=FALSE)
 
########## Setup
options(scipen=10)
#forces R not to use scientific notation
 
library(dplyr)
library(randomForestSRC)
library(e1071)
library(mgcv)
library(ggplot2)
library(kknn)
library(Information)
library(ClustOfVar)
library(reshape2)
library(plyr)
 
############################################
#RFM Segmentation
############################################
 
###RECENT RAMBOS###
#variable is latest_ticket
brokers_R <- brokers %>% select(npn, latest_ticket)
brokers_R <- brokers_R %>% filter(!latest_ticket %in% 'Unknown')
brokers_R <- brokers_R %>% mutate(days_old=Sys.Date() - as.Date(latest_ticket))
brokers_R_1 <- brokers_R %>% filter(days_old < quantile(brokers_R$days_old, .33))
brokers_R_2 <- brokers_R %>% filter(between(brokers_R$days_old, quantile(brokers_R$days_old, .33), quantile(brokers_R$days_old, .66)))
brokers_R_3 <- brokers_R %>% filter(between(brokers_R$days_old, quantile(brokers_R$days_old, .66), quantile(brokers_R$days_old, 1)))
 
###FREQUENT FLYERS###
#variable is num_tickets
brokers_F <- brokers %>% select(npn, num_tickets)
brokers_F <- brokers_F %>% filter(!num_tickets %in% 'Unknown')
brokers_F <- brokers_F %>% mutate(num_tickets=as.numeric(num_tickets))
brokers_F_3 <- brokers_F %>% filter(num_tickets < quantile(brokers_F$num_tickets, .33))
brokers_F_2 <- brokers_F %>% filter(between(brokers_F$num_tickets, quantile(brokers_F$num_tickets, .33), quantile(brokers_F$num_tickets, .66)))
brokers_F_1 <- brokers_F %>% filter(between(brokers_F$num_tickets, quantile(brokers_F$num_tickets, .66), quantile(brokers_F$num_tickets, 1)))
 
###MONEY MAKERS (AVG PREMIUM NOT TOT PREMIUM)###
brokers_M <- brokers %>% select(npn, tot_premium, num_tickets)
brokers_M <- brokers_M %>% filter(!tot_premium %in% 'Unknown')
brokers_M <- brokers_M %>% filter(!num_tickets %in% 'Unknown')
brokers_M <- brokers_M %>% mutate(tot_premium=as.numeric(tot_premium))
brokers_M <- brokers_M %>% mutate(num_tickets=as.numeric(num_tickets))
brokers_M$avg_premium <- brokers_M$tot_premium/brokers_M$num_tickets
brokers_M_3 <- brokers_M %>% filter(avg_premium < quantile(brokers_M$avg_premium, .33))
brokers_M_2 <- brokers_M %>% filter(between(brokers_M$avg_premium, quantile(brokers_M$avg_premium, .33), quantile(brokers_M$avg_premium, .66)))
brokers_M_1 <- brokers_M %>% filter(between(brokers_M$avg_premium, quantile(brokers_M$avg_premium, .66), quantile(brokers_M$avg_premium, 1)))
 
#ggplot(brokers_M, aes(tot_premium)) +
#  geom_density() + labs(x = "Total Premium", y = 'Density',
#                        title = 'Total premium of brokers who have dropped
#                        a ticket since 09-30-2015') +
#  xlim(0, 4500000)
 
###ALL STARS (Top 3rd in all three segments (R, F, and M)###
brokers_R1_F1_M1 <- join_all(list(brokers_R_1, brokers_F_1, brokers_M_1),
                             by = 'npn', type = 'inner')
brokers_R1_F1_M2 <- join_all(list(brokers_R_1, brokers_F_1, brokers_M_2),
                             by = 'npn', type = 'inner')
brokers_R1_F2_M1 <- join_all(list(brokers_R_1, brokers_F_2, brokers_M_1),
                             by = 'npn', type = 'inner')
brokers_R2_F1_M1 <- join_all(list(brokers_R_2, brokers_F_1, brokers_M_1),
                             by = 'npn', type = 'inner')
 
#the data frames being row bound below all have at least two 1's
brokers_1_1 <- rbind(brokers_R1_F1_M1, brokers_R1_F1_M2,
                     brokers_R1_F2_M1, brokers_R2_F1_M1)
 
#remove duplicates
brokers_1_1 = brokers_1_1[order(brokers_1_1[,'npn'],-brokers_1_1[,'npn']),]
brokers_1_1 = brokers_1_1[!duplicated(brokers_1_1$npn),]
 
###BIG FISH (High Average Premium and High Number of Tickets)###
#big_fish <- filter(brokers_all, brokers_all$avg_premium > 100000 & num_tickets > 24)
 
rm(brokers_R1_F1_M1)
rm(brokers_R1_F1_M2)
rm(brokers_R1_F2_M1)
rm(brokers_R2_F1_M1)
 
brokers_1_1 <- join_all(list(brokers, brokers_1_1),
                        by = 'npn', type = 'inner')
brokers_1_1 <- brokers_1_1[,1:87]
 
brokers_axa <- join_all(list(brokers_R, brokers_F, brokers_M),
                        by = 'npn', type = 'inner')
 
brokers_axa <- brokers_axa[,c("npn", "days_old", "num_tickets", "avg_premium")]
brokers_axa$days_old <- as.integer(brokers_axa$days_old)
 
brokers_axa <- join_all(list(brokers_axa, brokers[,c("npn", "bdfirmname",
                                                          "firstname", "lastname",
                                                          "suffix", "email_businesstype",
                                                          "email_businesstypevalidationsupported",
                                                          "email_businesstypeupdate")]),
                             by = 'npn', type = 'left')
 
brokers_non_axa <- filter(brokers, target == 0)
brokers_non_axa <- brokers_non_axa[,c("npn", "bdfirmname",
                                             "firstname", "lastname",
                                             "suffix", "email_businesstype",
                                             "email_businesstypevalidationsupported",
                                             "email_businesstypeupdate")]
brokers_non_axa[,c("days_old", "num_tickets", "avg_premium")] <- NA
 
brokers_full <- rbind(brokers_axa, brokers_non_axa)
 
############################################
#Converting columns to appropriate data type
############################################
 
brokers$number_yearsanagent <- as.integer(brokers$number_yearsanagent)
brokers$securitiesregisteredagent <- as.factor(brokers$securitiesregisteredagent)
brokers$carrierappointments <- as.factor(brokers$carrierappointments)
brokers$gender <- as.factor(brokers$gender)
brokers$agentlicensetype_health <- as.factor(brokers$agentlicensetype_health)
brokers$number_statelicenses_health <- as.integer(brokers$number_statelicenses_health)
brokers$agentlicensetype_life <- as.factor(brokers$agentlicensetype_life)
brokers$number_statelicenses_life <- as.integer(brokers$number_statelicenses_life)
brokers$agentlicensetype_propertycasualty <- as.factor(brokers$agentlicensetype_propertycasualty)
brokers$number_statelicenses_propertycasualty <- as.integer(brokers$number_statelicenses_propertycasualty)
brokers$agentlicensetype_variableproducts <- as.factor(brokers$agentlicensetype_variableproducts)
brokers$number_statelicenses_variableproducts <- as.integer(brokers$number_statelicenses_variableproducts)
brokers$titlecategories <- as.factor(brokers$titlecategories)
brokers$bdfirmcrd <- as.factor(brokers$bdfirmcrd)
brokers$branch_zipcode <- as.factor(substr(brokers$branch_zipcode, 0, 3))
brokers$number_branchreps <- as.integer(brokers$number_branchreps)
brokers$datebecamerep_numberofyears <- as.integer(brokers$datebecamerep_numberofyears)
brokers$dateofhireatcurrentfirm_numberofyears <- as.integer(brokers$dateofhireatcurrentfirm_numberofyears)
brokers$nonproducer <- as.factor(brokers$nonproducer)
brokers$independentcontractor <- as.factor(brokers$independentcontractor)
brokers$duallylicensedbdriarep <- as.factor(brokers$duallylicensedbdriarep)
brokers$duallyregisteredbdriarep <- as.factor(brokers$duallyregisteredbdriarep)
brokers$possiblefundinvestingrep <- as.factor(brokers$possiblefundinvestingrep)
brokers$sellsretirementplanproducts <- as.factor(brokers$sellsretirementplanproducts)
brokers$number_registeredstates <- as.integer(brokers$number_registeredstates)
brokers$priorfirm1_firmcrd <- as.factor(brokers$priorfirm1_firmcrd)
brokers$priorfirm1_numberofyears <- as.integer(brokers$priorfirm1_numberofyears)
brokers$is_variable_licensed <- as.factor(brokers$is_variable_licensed)
brokers$title_advisor <- as.factor(brokers$title_advisor)
brokers$has_license_to_sell <- as.factor(brokers$has_license_to_sell)
brokers$channeltype <- as.factor(brokers$channeltype)
brokers$firmcode <- as.factor(brokers$firmcode)
brokers <- brokers %>% mutate(age = replace(age, age > 100, 'Unknown'))
brokers$age <- as.integer(brokers$age)
brokers$target <- ifelse(((brokers$counter %in% c(1,2))),1,0)
brokers$target <- as.numeric(brokers$target)
 
############################################
#Choose revelant covariates that could be
#possibly predictive
############################################
 
brokers_model <- select(brokers, npn, number_yearsanagent, carrierappointments,
                        gender, number_statelicenses_health,
                        number_statelicenses_life,
                        number_statelicenses_propertycasualty,
                       number_statelicenses_variableproducts,
                        bdfirmcrd, branch_zipcode, datebecamerep_numberofyears,
                        dateofhireatcurrentfirm_numberofyears, nonproducer,
                        independentcontractor, duallylicensedbdriarep,
                        duallyregisteredbdriarep, possiblefundinvestingrep,
                        number_registeredstates, priorfirm1_numberofyears, title_advisor,
                        age, firmfriendliness, target)
 
############################################
#Split Data into testing and training
############################################
 
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(brokers_model$target, p = .50,
                                  list = FALSE,
                                  times = 1)
 
brokers_model_train <- brokers_model[ splitIndex,]
brokers_model_test <- brokers_model[-splitIndex,]
 
rm(splitIndex)
 
prop.table(table(brokers_model_train$target))
prop.table(table(brokers_model_test$target))
 
#Convert all 'Unknown' values to NA values
brokers_model_train[brokers_model_train=='Unknown'] <- NA
brokers_model_test[brokers_model_test=='Unknown'] <- NA
 
#drop unused levels
brokers_model_test <- droplevels(brokers_model_test)
brokers_model_train <- droplevels(brokers_model_train)
 
#remove npn so it's not used as part of IV or clustering process
#brokers_model_test <- subset(brokers_model_test, select=-npn)
#brokers_model_train <- subset(brokers_model_train, select=-npn)
 
######### Kill the weakest variables with IV
IV <- Information::create_infotables(data=brokers_model_train[, !names(brokers_model_train) %in% c("npn")], NULL, "target", 10)
View(IV$Summary)
 
#remove variables with IV less than 0.04
brokers_model_train <- brokers_model_train[,c(subset(IV$Summary, IV>0.04)$Variable, "npn", "target")]
brokers_model_test <- brokers_model_test[,c(subset(IV$Summary, IV>0.04)$Variable, "npn", "target")]
 
#make all variables numeric for variable clustering
brokers_model_train_clustering <- brokers_model_train[, !names(brokers_model_train) %in% c("npn")]
brokers_model_train_clustering$number_yearsanagent <- as.numeric(brokers_model_train_clustering$number_yearsanagent)
brokers_model_train_clustering$carrierappointments <- as.numeric(brokers_model_train_clustering$carrierappointments)
brokers_model_train_clustering$gender <- as.numeric(brokers_model_train_clustering$gender)
brokers_model_train_clustering$number_statelicenses_life <- as.numeric(brokers_model_train_clustering$number_statelicenses_life)
brokers_model_train_clustering$bdfirmcrd <- as.numeric(brokers_model_train_clustering$bdfirmcrd)
brokers_model_train_clustering$branch_zipcode <- as.numeric(substr(brokers_model_train_clustering$branch_zipcode, 0, 3))
brokers_model_train_clustering$nonproducer <- as.numeric(brokers_model_train_clustering$nonproducer)
brokers_model_train_clustering$independentcontractor <- as.numeric(brokers_model_train_clustering$independentcontractor)
brokers_model_train_clustering$duallylicensedbdriarep <- as.numeric(brokers_model_train_clustering$duallylicensedbdriarep)
brokers_model_train_clustering$duallyregisteredbdriarep <- as.numeric(brokers_model_train_clustering$duallyregisteredbdriarep)
brokers_model_train_clustering$possiblefundinvestingrep <- as.numeric(brokers_model_train_clustering$possiblefundinvestingrep)
brokers_model_train_clustering$title_advisor <- as.numeric(brokers_model_train_clustering$title_advisor)
brokers_model_train_clustering$number_registeredstates <- as.numeric(brokers_model_train_clustering$number_registeredstates)
brokers_model_train_clustering$datebecamerep_numberofyears <- as.numeric(brokers_model_train_clustering$datebecamerep_numberofyears)
brokers_model_train_clustering$priorfirm1_numberofyears  <- as.numeric(brokers_model_train_clustering$priorfirm1_numberofyears)
brokers_model_train_clustering$number_statelicenses_variableproducts  <- as.numeric(brokers_model_train_clustering$number_statelicenses_variableproducts)
brokers_model_train_clustering$age <- as.numeric(brokers_model_train_clustering$age)
 
######## Variable clustering
tree <- hclustvar(brokers_model_train_clustering[,!(names(brokers_model_train_clustering)=="target")])
nclusters <- length(tree[tree$height<1.00])
part_init <- cutreevar(tree,nclusters)$cluster
kmeans <- kmeansvar(X.quanti=brokers_model_train_clustering[,!(names(brokers_model_train_clustering)=="target")],init=part_init)
clusters <- cbind.data.frame(melt(kmeans$cluster), row.names(melt(kmeans$cluster)))
names(clusters) <- c("Cluster", "Variable")
clusters <- join(clusters, IV$Summary, by="Variable", type="left")
clusters <- clusters[order(clusters$Cluster),]
clusters$Rank <- ave(-clusters$IV, clusters$Cluster, FUN=rank)
variables <- as.character(subset(clusters, Rank==1)$Variable)
rm(brokers_model_train_clustering)
 
############################################
#Use Fully Conditional Specification to
#impute missing values using mice library
############################################
 
library(mice)
 
#mice generates a dataset using a default of 5 iterations for each
system.time(mice_imputes_train <- mice(brokers_model_train, m=1))
system.time(mice_imputes_test <- mice(brokers_model_test, m=1))
 
mice_imputes_train$method
mice_imputes_test$method
 
#choosing the dataset
brokers_model_train_sample <- complete(mice_imputes_train,1)
brokers_model_test_sample <- complete(mice_imputes_test,1)
 
brokers_model_train_sample$npn <- brokers_model_train$npn
brokers_model_test_sample$npn <- brokers_model_test$npn
 
brokers_model_train_sample$target <- brokers_model_train$target
brokers_model_test_sample$target <- brokers_model_test$target
 
#Plotting and comparing values with xyplot()
#blue is observed, red is imputed
#xyplot(mice_imputes_train, number_registeredstates ~ number_yearsanagent | .imp, pch = 20, cex = 1.4)
#xyplot(mice_imputes_train, number_registeredstates ~ priorfirm1_numberofyears | .imp, pch = 20, cex = 1.4)
#xyplot(mice_imputes_train, number_yearsanagent ~ priorfirm1_numberofyears | .imp, pch = 20, cex = 1.4)
 
#make a density plot
#blue is observed, red is imputed
densityplot(mice_imputes_train)
densityplot(mice_imputes_test)
 
############################################
#use this chunk code to bypass ~3 hour
#imputing process
############################################
 
load("brokers_model_test_sample.Rda")
load("brokers_model_train_sample.Rda")
 
load("brokers_model_test_sample_age.Rda")
load("brokers_model_train_sample_age.Rda")
 
brokers_model_train_sample$age <- brokers_model_train_sample_age$age
brokers_model_test_sample$age <- brokers_model_test_sample_age$age
 
brokers_model_train_sample$npn <- brokers_model_train$npn
brokers_model_test_sample$npn <- brokers_model_test$npn
 
brokers_model_train_sample$target <- brokers_model_train$target
brokers_model_test_sample$target <- brokers_model_test$target
 
brokers_model_train_sample$dateofhireatcurrentfirm_numberofyears <- brokers_model_train_sample_age$dateofhireatcurrentfirm_numberofyears
brokers_model_test_sample$dateofhireatcurrentfirm_numberofyears <- brokers_model_test_sample_age$dateofhireatcurrentfirm_numberofyears
 
rm(brokers_model_test_sample_age)
rm(brokers_model_train_sample_age)
 
############################################
#Sample a subset of brokers who don't yet sell
#AXA product and proportionally
#append to R_1, F_1, and M_1 segments
############################################
 
#R_1
 
brokers_model_train_target_0 <- filter(brokers_model_train_sample, target==0)
brokers_model_train_target_1 <- filter(brokers_model_train_sample, target==1)
brokers_R_1_good <- filter(brokers_model_train_sample, npn %in% brokers_R_1$npn)
 
set.seed(1234)
rows <- sample(1:nrow(brokers_model_train_target_0),
               nrow(brokers_R_1_good)/(nrow(brokers_model_train_target_1))*nrow(brokers_model_train_target_0),
               replace=FALSE)
 
brokers_model_train_target_0_sample <- brokers_model_train_target_0[rows,]
 
brokers_model_train_R <- rbind(brokers_model_train_target_0_sample, brokers_R_1_good)
 
rm(brokers_model_train_target_1)
rm(brokers_R_1_good)
 
#F_1
 
brokers_model_train_target_0_sample_leftover <- brokers_model_train_target_0[-rows,]
smp_size <- floor(0.5 * nrow(brokers_model_train_target_0_sample_leftover))
set.seed(5678)
samples <- sample(seq_len(nrow(brokers_model_train_target_0_sample_leftover)), size = smp_size)
brokers_model_train_target_0_remain_F <- brokers_model_train_target_0_sample_leftover[samples, ]
brokers_model_train_target_0_remain_M <- brokers_model_train_target_0_sample_leftover[-samples, ]
 
rm(smp_size)
rm(samples)
rm(brokers_model_train_target_0_sample_leftover)
 
brokers_F_1_good <- filter(brokers_model_train_sample, npn %in% brokers_F_1$npn)
 
brokers_model_train_F <- rbind(brokers_model_train_target_0_remain_F, brokers_F_1_good)
 
rm(brokers_model_train_target_0)
rm(brokers_model_train_target_0_remain_F)
rm(brokers_F_1_good)
rm(rows)
 
#M_1
 
brokers_M_1_good <- filter(brokers_model_train_sample, npn %in% brokers_M_1$npn)
 
brokers_model_train_M <- rbind(brokers_model_train_target_0_remain_M, brokers_M_1_good)
 
rm(brokers_M_1_good)
rm(brokers_model_train_target_0_remain_M)
rm(brokers_model_train_target_0_sample)
 
prop.table(table(brokers_model_train_R$target))
prop.table(table(brokers_model_train_F$target))
prop.table(table(brokers_model_train_M$target))
 
############################################
####TOP 3rd RECENCY SEGMENT#################
############################################
 
############################################
#Upsample occurence of brokers who sell axa product.
#Since there are only around 7.5% of brokers who sell our product,
#this can improve predictive power of model
############################################
 
#factorize target
brokers_model_train_R$target <- as.factor(brokers_model_train_R$target)
 
set.seed(9560)
up_train_R <- upSample(x = brokers_model_train_R[, !names(brokers_model_train_R) %in% c("target")],
                     y = brokers_model_train_R$target)
table(up_train_R$Class)
 
#subset the dataset for modeling
train_R <- up_train_R
rm(up_train_R)
train_R$Class <- as.integer(train_R$Class) - 1
train_R_all <- train_R
train_R <- train_R[,c("Class", variables)]
train_R[,'target'] <- NULL
test <- brokers_model_test_sample[,c("target", variables)]
test$Class <- test$target
test[,'target'] <- NULL
 
library(useful)
 
theFormula <- Class ~ . -1
#it's a tree so we don't have to worry about mathematical stability, hence -1
#removing intercept
 
#onehot encoding categorical variables
xTrain_R <- build.x(theFormula, data=train_R, contrasts=FALSE, sparse=TRUE)
#xValidate <- build.x(theFormula, data=validate, contrasts=FALSE, sparse=TRUE)
xTest <- build.x(theFormula, data=test, contrasts=FALSE, sparse=TRUE)
 
yTrain_R <- build.y(theFormula, data=train_R)
#yValidate <- build.y(theFormula, data=validate)
yTest <- build.y(theFormula, data=test)
 
library(xgboost)
trainXG_R <- xgb.DMatrix(data=xTrain_R, label=yTrain_R)
#validateXG <- xgb.DMatrix(data=xValidate, label=yValidate)
#watchList <- list(train=trainXG, validate=validateXG)
 
#finding best tuning parameters for XGBoost using a for loop
 
best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0
 
for (iter in 1:10) {
  param <- list(objective = "binary:logistic",
                eval_metric = "logloss",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8),
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 100
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=trainXG_R, params = param, nthread=6,
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early_stopping_rounds=8, maximize=FALSE)
 
  min_logloss = min(mdcv$evaluation_log[, test_logloss_mean])
  min_logloss_index = which.min(mdcv$evaluation_log[, test_logloss_mean])
 
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
}
 
nround = best_logloss_index
set.seed(best_seednumber)
 
xg1_R <-xgb.train(
  data=trainXG_R,
  params=best_param,
  nrounds=nround,
  nthread=6#,
  #watchlist=watchList,
)
 
#importance <- xgb.importance(feature_names = trainXG@Dimnames[[2]],
#                             model = xg1 )
 
predXG_R <-predict(xg1_R, newdata=xTest)
 
pred_R <- as.vector(predXG_R)
 
paste0("XGBOOST: ", AUC(test[["Class"]], pred_R)[1])
 
pred_time_R <- predXG_R
test_xgboost_R <- test
test_xgboost_R$pred_time_R <- pred_time_R
 
test_xgboost_Class_0_R <- filter(test_xgboost_R, Class == 0)
test_xgboost_Class_1_R <- filter(test_xgboost_R, Class == 1)
 
test_xgboost_good_fit_R <- filter(test_xgboost_R, pred_time_R > .25 & Class == 1)
test_xgboost_prospects_R <- filter(test_xgboost_R, pred_time_R > .25 & Class == 0)
 
ggplot(test_xgboost_Class_1_R, aes(pred_time_R)) +
  geom_density() + xlim(0, 1)
 
ggplot(test_xgboost_Class_0_R, aes(pred_time_R)) +
  geom_density() + xlim(0, 1)
 
############################################
####TOP 3rd FREQUENCY SEGMENT###############
############################################
 
############################################
#Upsample occurence of brokers who sell axa product.
#Since there are only around 7.5% of brokers who sell our product,
#this can improve predictive power of model
############################################
 
#factorize target
brokers_model_train_F$target <- as.factor(brokers_model_train_F$target)
 
set.seed(4268)
up_train_F <- upSample(x = brokers_model_train_F[, !names(brokers_model_train_F) %in% c("target")],
                       y = brokers_model_train_F$target)
table(up_train_F$Class)
 
#subset the dataset for modeling
train_F <- up_train_F
rm(up_train_F)
train_F$Class <- as.integer(train_F$Class) - 1
train_F_all <- train_F
train_F <- train_F[,c("Class", variables)]
train_F[,'target'] <- NULL
 
library(useful)
 
theFormula <- Class ~ . -1
#it's a tree so we don't have to worry about mathematical stability, hence -1
#removing intercept
 
#onehot encoding categorical variables
xTrain_F <- build.x(theFormula, data=train_F, contrasts=FALSE, sparse=TRUE)
#xValidate <- build.x(theFormula, data=validate, contrasts=FALSE, sparse=TRUE)
xTest <- build.x(theFormula, data=test, contrasts=FALSE, sparse=TRUE)
 
yTrain_F <- build.y(theFormula, data=train_F)
#yValidate <- build.y(theFormula, data=validate)
yTest <- build.y(theFormula, data=test)
 
library(xgboost)
trainXG_F <- xgb.DMatrix(data=xTrain_F, label=yTrain_F)
#validateXG <- xgb.DMatrix(data=xValidate, label=yValidate)
#watchList <- list(train=trainXG, validate=validateXG)
 
#finding best tuning parameters for XGBoost using a for loop
 
best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0
 
for (iter in 1:10) {
  param <- list(objective = "binary:logistic",
                eval_metric = "logloss",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8),
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 100
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=trainXG_F, params = param, nthread=6,
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early_stopping_rounds=8, maximize=FALSE)
 
  min_logloss = min(mdcv$evaluation_log[, test_logloss_mean])
  min_logloss_index = which.min(mdcv$evaluation_log[, test_logloss_mean])
 
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
}
 
nround = best_logloss_index
set.seed(best_seednumber)
 
xg1_F <-xgb.train(
  data=trainXG_F,
  params=best_param,
  nrounds=nround,
  nthread=6#,
  #watchlist=watchList,
)
 
#importance <- xgb.importance(feature_names = trainXG@Dimnames[[2]],
#                             model = xg1 )
 
predXG_F <-predict(xg1_F, newdata=xTest)
 
pred_F <- as.vector(predXG_F)
 
paste0("XGBOOST: ", AUC(test[["Class"]], pred_F)[1])
 
pred_time_F <- predXG_F
test_xgboost_F <- test
test_xgboost_F$pred_time_F <- pred_time_F
 
test_xgboost_Class_0_F <- filter(test_xgboost_F, Class == 0)
test_xgboost_Class_1_F <- filter(test_xgboost_F, Class == 1)
 
test_xgboost_good_fit_F <- filter(test_xgboost_F, pred_time_F > .25 & Class == 1)
test_xgboost_prospects_F <- filter(test_xgboost_F, pred_time_F > .25 & Class == 0)
 
ggplot(test_xgboost_Class_1_F, aes(pred_time_F)) +
  geom_density() + xlim(0, 1)
 
ggplot(test_xgboost_Class_0_F, aes(pred_time_F)) +
  geom_density() + xlim(0, 1)
 
############################################
####TOP 3rd MONETARY SEGMENT###############
############################################
 
############################################
#Upsample occurence of brokers who sell axa product.
#Since there are only around 7.5% of brokers who sell our product,
#this can improve predictive power of model
############################################
 
#factorize target
brokers_model_train_M$target <- as.factor(brokers_model_train_M$target)
 
set.seed(1278)
up_train_M <- upSample(x = brokers_model_train_M[, !names(brokers_model_train_M) %in% c("target")],
                       y = brokers_model_train_M$target)
table(up_train_M$Class)
 
#subset the dataset for modeling
train_M <- up_train_M
rm(up_train_M)
train_M$Class <- as.integer(train_M$Class) - 1
train_M_all <- train_M
train_M <- train_M[,c("Class", variables)]
train_M[,'target'] <- NULL
 
library(useful)
 
theFormula <- Class ~ . -1
#it's a tree so we don't have to worry about mathematical stability, hence -1
#removing intercept
 
#onehot encoding categorical variables
xTrain_M <- build.x(theFormula, data=train_M, contrasts=FALSE, sparse=TRUE)
#xValidate <- build.x(theFormula, data=validate, contrasts=FALSE, sparse=TRUE)
xTest <- build.x(theFormula, data=test, contrasts=FALSE, sparse=TRUE)
 
yTrain_M <- build.y(theFormula, data=train_M)
#yValidate <- build.y(theFormula, data=validate)
yTest <- build.y(theFormula, data=test)
 
library(xgboost)
trainXG_M <- xgb.DMatrix(data=xTrain_M, label=yTrain_M)
#validateXG <- xgb.DMatrix(data=xValidate, label=yValidate)
#watchList <- list(train=trainXG, validate=validateXG)
 
#finding best tuning parameters for XGBoost using a for loop
 
best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0
 
for (iter in 1:10) {
  param <- list(objective = "binary:logistic",
                eval_metric = "logloss",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2),
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8),
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 100
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=trainXG_M, params = param, nthread=6,
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early_stopping_rounds=8, maximize=FALSE)
 
  min_logloss = min(mdcv$evaluation_log[, test_logloss_mean])
  min_logloss_index = which.min(mdcv$evaluation_log[, test_logloss_mean])
 
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
}
 
nround = best_logloss_index
set.seed(best_seednumber)
 
xg1_M <-xgb.train(
  data=trainXG_M,
  params=best_param,
  nrounds=nround,
  nthread=6#,
  #watchlist=watchList,
)
 
#importance <- xgb.importance(feature_names = trainXG@Dimnames[[2]],
#                             model = xg1 )
 
predXG_M <-predict(xg1_M, newdata=xTest)
 
pred_M <- as.vector(predXG_M)
 
paste0("XGBOOST: ", AUC(test[["Class"]], pred_M)[1])
 
pred_time_M <- predXG_M
test_xgboost_M <- test
test_xgboost_M$pred_time_M <- pred_time_M
 
test_xgboost_Class_0_M <- filter(test_xgboost_M, Class == 0)
test_xgboost_Class_1_M <- filter(test_xgboost_M, Class == 1)
 
test_xgboost_good_fit_M <- filter(test_xgboost_M, pred_time_M > .25 & Class == 1)
test_xgboost_prospects_M <- filter(test_xgboost_M, pred_time_M > .5 & Class == 0)
 
ggplot(test_xgboost_Class_1_M, aes(pred_time_M)) +
  geom_density() + xlim(0, 1)
 
ggplot(test_xgboost_Class_0_M, aes(pred_time_M)) +
  geom_density() + xlim(0, 1)
 
#############################################
####Generalized Additive Model (GAM)#########
#############################################
 
#R_1
 
#remove factor levels not in both test and train datasets
bdfirmcrd_shared_R <- intersect(train_R$bdfirmcrd,
                              test$bdfirmcrd)
 
branch_zipcode_shared_R <- intersect(train_R$branch_zipcode,
                                   test$branch_zipcode)
 
train_R_GAM <- filter(train_R, bdfirmcrd %in% bdfirmcrd_shared_R)
 
train_R_GAM <- filter(train_R_GAM, branch_zipcode %in% branch_zipcode_shared_R)
 
test_R_GAM <- filter(test, bdfirmcrd %in% bdfirmcrd_shared_R)
 
test_R_GAM <- filter(test_R_GAM, branch_zipcode %in% branch_zipcode_shared_R)
 
########## GAM where smoothing parameters are selected with REML.
f_R_GAM <- CreateGAMFormula(train_R_GAM[,variables], "Class", -1, "regspline")
f_R_GAM <- Class ~ s(bdfirmcrd, bs = "re") + s(firmfriendliness, bs = "ps", sp = -1) +
  s(branch_zipcode, bs = "re") + carrierappointments + s(number_registeredstates,
                                           bs = "ps", sp = -1) + s(number_yearsanagent, bs = "ps", sp = -1) +
  possiblefundinvestingrep + s(priorfirm1_numberofyears, bs = "ps",
                               sp = -1) + s(dateofhireatcurrentfirm_numberofyears, bs = "ps",
                                            sp = -1)
 
system.time(
  gam2.model_R_GAM <- mgcv::gam(f_R_GAM, data=train_R_GAM, family=binomial(link="logit"), method="REML")
)
 
### Predict the probabilities for the validation dataset.
system.time(
  gam2.predict_R_GAM <- 1/(1+exp(-mgcv::predict.gam(gam2.model_R_GAM, newdata=test_R_GAM)))
)
 
gam2.predict.auc_R_GAM <- as.vector(gam2.predict_R_GAM)
#brokers_model_test_no_na_sample$target <- as.integer(brokers_model_test_no_na_sample$target)
paste0("GAM2: ", AUC(test_R_GAM[["Class"]], gam2.predict.auc_R_GAM)[1])
 
pred_time2_R_GAM <- gam2.predict.auc_R_GAM
test_gam2_R <- test_R_GAM
test_gam2_R$pred_time2_R_GAM <- pred_time2_R_GAM
 
test_gam2_target_0_R_GAM <- filter(test_gam2_R, Class == 0)
test_gam2_target_1_R_GAM <- filter(test_gam2_R, Class == 1)
 
test_gam2_good_fit_R_GAM <- filter(test_gam2_R, pred_time2_R_GAM > .25 & Class == 1)
test_gam2_prospects_R_GAM <- filter(test_gam2_R, pred_time2_R_GAM > .25 & Class == 0)
 
ggplot(test_gam2_target_0_R_GAM, aes(pred_time2_R_GAM)) +
  geom_density() + xlim(0, 1)
 
ggplot(test_gam2_target_1_R_GAM, aes(pred_time2_R_GAM)) +
  geom_density() + xlim(0, 1)
 
#rejoin npn number with test_gam2_R and create test_gam2_R_final
 
test_R_GAM_final <- filter(brokers_model_test_sample, bdfirmcrd %in% bdfirmcrd_shared_R)
 
test_R_GAM_final <- filter(test_R_GAM_final, branch_zipcode %in% branch_zipcode_shared_R)
 
test_gam2_R_final <- test_R_GAM_final
test_gam2_R_final$pred_time2_R_GAM <- pred_time2_R_GAM
 
#F_1
 
#remove factor levels not in both test and train datasets
bdfirmcrd_shared_F <- intersect(train_F$bdfirmcrd,
                                test$bdfirmcrd)
 
branch_zipcode_shared_F <- intersect(train_F$branch_zipcode,
                                     test$branch_zipcode)
 
train_F_GAM <- filter(train_F, bdfirmcrd %in% bdfirmcrd_shared_F)
 
train_F_GAM <- filter(train_F_GAM, branch_zipcode %in% branch_zipcode_shared_F)
 
test_F_GAM <- filter(test, bdfirmcrd %in% bdfirmcrd_shared_F)
 
test_F_GAM <- filter(test_F_GAM, branch_zipcode %in% branch_zipcode_shared_F)
 
########## GAM where smoothing parameters are selected with REML.
f_F_GAM <- CreateGAMFormula(train_F_GAM[,variables], "Class", -1, "regspline")
f_F_GAM <- Class ~ s(bdfirmcrd, bs = "re") + s(firmfriendliness, bs = "ps", sp = -1) +
  s(branch_zipcode, bs = "re") + carrierappointments + s(number_registeredstates,
                                                         bs = "ps", sp = -1) + s(number_yearsanagent, bs = "ps", sp = -1) +
  possiblefundinvestingrep + s(priorfirm1_numberofyears, bs = "ps",
                               sp = -1) + s(dateofhireatcurrentfirm_numberofyears, bs = "ps",
                                            sp = -1)
 
system.time(
  gam2.model_F_GAM <- mgcv::gam(f_F_GAM, data=train_F_GAM, family=binomial(link="logit"), method="REML")
)
 
### Predict the probabilities for the validation dataset.
system.time(
  gam2.predict_F_GAM <- 1/(1+exp(-mgcv::predict.gam(gam2.model_F_GAM, newdata=test_F_GAM)))
)
 
gam2.predict.auc_F_GAM <- as.vector(gam2.predict_F_GAM)
#brokers_model_test_no_na_sample$target <- as.integer(brokers_model_test_no_na_sample$target)
paste0("GAM2: ", AUC(test_F_GAM[["Class"]], gam2.predict.auc_F_GAM)[1])
 
pred_time2_F_GAM <- gam2.predict.auc_F_GAM
test_gam2_F <- test_F_GAM
test_gam2_F$pred_time2_F_GAM <- pred_time2_F_GAM
 
test_gam2_target_0_F_GAM <- filter(test_gam2_F, Class == 0)
test_gam2_target_1_F_GAM <- filter(test_gam2_F, Class == 1)
 
test_gam2_good_fit_F_GAM <- filter(test_gam2_F, pred_time2_F_GAM > .25 & Class == 1)
test_gam2_prospects_F_GAM <- filter(test_gam2_F, pred_time2_F_GAM > .25 & Class == 0)
 
ggplot(test_gam2_target_0_F_GAM, aes(pred_time2_F_GAM)) +
  geom_density() + xlim(0, 1)
 
ggplot(test_gam2_target_1_F_GAM, aes(pred_time2_F_GAM)) +
  geom_density() + xlim(0, 1)
 
#rejoin npn number with test_gam2_F and create test_gam2_F_final
 
test_F_GAM_final <- filter(brokers_model_test_sample, bdfirmcrd %in% bdfirmcrd_shared_F)
 
test_F_GAM_final <- filter(test_F_GAM_final, branch_zipcode %in% branch_zipcode_shared_F)
 
test_gam2_F_final <- test_F_GAM_final
test_gam2_F_final$pred_time2_F_GAM <- pred_time2_F_GAM
 
#M_1
 
#remove factor levels not in both test and train datasets
bdfirmcrd_shared_M <- intersect(train_M$bdfirmcrd,
                                test$bdfirmcrd)
 
branch_zipcode_shared_M <- intersect(train_M$branch_zipcode,
                                     test$branch_zipcode)
 
train_M_GAM <- filter(train_M, bdfirmcrd %in% bdfirmcrd_shared_M)
 
train_M_GAM <- filter(train_M_GAM, branch_zipcode %in% branch_zipcode_shared_M)
 
test_M_GAM <- filter(test, bdfirmcrd %in% bdfirmcrd_shared_M)
 
test_M_GAM <- filter(test_M_GAM, branch_zipcode %in% branch_zipcode_shared_M)
 
########## GAM where smoothing parameters are selected with REML.
f_M_GAM <- CreateGAMFormula(train_M_GAM[,variables], "Class", -1, "regspline")
f_M_GAM <- Class ~ s(bdfirmcrd, bs = "re") + s(firmfriendliness, bs = "ps", sp = -1) +
  s(branch_zipcode, bs = "re") + carrierappointments + s(number_registeredstates,
                                                         bs = "ps", sp = -1) + s(number_yearsanagent, bs = "ps", sp = -1) +
  possiblefundinvestingrep + s(priorfirm1_numberofyears, bs = "ps",
                               sp = -1) + s(dateofhireatcurrentfirm_numberofyears, bs = "ps",
                                            sp = -1)
 
system.time(
  gam2.model_M_GAM <- mgcv::gam(f_M_GAM, data=train_M_GAM, family=binomial(link="logit"), method="REML")
)
 
### Predict the probabilities for the validation dataset.
system.time(
  gam2.predict_M_GAM <- 1/(1+exp(-mgcv::predict.gam(gam2.model_M_GAM, newdata=test_M_GAM)))
)
 
gam2.predict.auc_M_GAM <- as.vector(gam2.predict_M_GAM)
#brokers_model_test_no_na_sample$target <- as.integer(brokers_model_test_no_na_sample$target)
paste0("GAM2: ", AUC(test_M_GAM[["Class"]], gam2.predict.auc_M_GAM)[1])
 
pred_time2_M_GAM <- gam2.predict.auc_M_GAM
test_gam2_M <- test_M_GAM
test_gam2_M$pred_time2_M_GAM <- pred_time2_M_GAM
 
test_gam2_target_0_M_GAM <- filter(test_gam2_M, Class == 0)
test_gam2_target_1_M_GAM <- filter(test_gam2_M, Class == 1)
 
test_gam2_good_fit_M_GAM <- filter(test_gam2_M, pred_time2_M_GAM > .25 & Class == 1)
test_gam2_prospects_M_GAM <- filter(test_gam2_M, pred_time2_M_GAM > .25 & Class == 0)
 
ggplot(test_gam2_target_0_M_GAM, aes(pred_time2_M_GAM)) +
  geom_density() + xlim(0, 1)
 
ggplot(test_gam2_target_1_M_GAM, aes(pred_time2_M_GAM)) +
  geom_density() + xlim(0, 1)
 
#rejoin npn number with test_gam2_F and create test_gam2_F_final
 
test_M_GAM_final <- filter(brokers_model_test_sample, bdfirmcrd %in% bdfirmcrd_shared_M)
 
test_M_GAM_final <- filter(test_M_GAM_final, branch_zipcode %in% branch_zipcode_shared_M)
 
test_gam2_M_final <- test_M_GAM_final
test_gam2_M_final$pred_time2_M_GAM <- pred_time2_M_GAM
 
#find common brokers among test_gam2_R_final, test_gam2_F_final, test_gam2_M_final
 
test_gam2_shared <- intersect(test_gam2_R_final$npn,
                                test_gam2_F_final$npn)
 
test_gam2_shared <- intersect(test_gam2_shared,
                              test_gam2_M_final$npn)
 
test_gam2_R_final_filtered <- filter(test_gam2_R_final, npn %in% test_gam2_shared)
test_gam2_F_final_filtered <- filter(test_gam2_F_final, npn %in% test_gam2_shared)
test_gam2_M_final_filtered <- filter(test_gam2_M_final, npn %in% test_gam2_shared)
 
#combining prediction results into one dataframe
 
test_gam2_results <- test_gam2_R_final_filtered
test_gam2_results$pred_time2_F_GAM <- test_gam2_F_final_filtered$pred_time2_F_GAM
test_gam2_results$pred_time2_M_GAM <- test_gam2_M_final_filtered$pred_time2_M_GAM
 
test_gam2_results_best <- c("pred_time2_R_GAM", "pred_time2_F_GAM", "pred_time2_M_GAM")[apply(test_gam2_results[,c("pred_time2_R_GAM", "pred_time2_F_GAM", "pred_time2_M_GAM")],1,which.max)]
test_gam2_results$best <- test_gam2_results_best
 
test_gam2_results_R <- filter(test_gam2_results, best == "pred_time2_R_GAM")
test_gam2_results_R_final <- filter(test_gam2_results_R, target == 0)
test_gam2_results_F <- filter(test_gam2_results, best == "pred_time2_F_GAM")
test_gam2_results_F_final <- filter(test_gam2_results_F, target == 0)
test_gam2_results_M <- filter(test_gam2_results, best == "pred_time2_M_GAM")
test_gam2_results_M_final <- filter(test_gam2_results_M, target == 0)
 
ggplot(test_gam2_results_R_final, aes(pred_time2_R_GAM)) +
  geom_density() + xlim(0, 1)
 
ggplot(test_gam2_results_F_final, aes(pred_time2_F_GAM)) +
  geom_density() + xlim(0, 1)
 
ggplot(test_gam2_results_M_final, aes(pred_time2_M_GAM)) +
  geom_density() + xlim(0, 1)
 
#get deciles for each segment
 
test_gam2_results_R_deciles <- cut(test_gam2_results_R$pred_time2_R_GAM, breaks=quantile(test_gam2_results_R$pred_time2_R_GAM, probs=c(0:10/10)), labels=1:10,
                                         include.lowest=TRUE)
test_gam2_results_R$decile <- test_gam2_results_R_deciles
test_gam2_results_R <- test_gam2_results_R[order(-test_gam2_results_R$pred_time2_R_GAM),]
 
test_gam2_results_F_deciles <- cut(test_gam2_results_F$pred_time2_F_GAM, breaks=quantile(test_gam2_results_F$pred_time2_F_GAM, probs=c(0:10/10)), labels=1:10,
                                   include.lowest=TRUE)
test_gam2_results_F$decile <- test_gam2_results_F_deciles
test_gam2_results_F <- test_gam2_results_F[order(-test_gam2_results_F$pred_time2_F_GAM),]
 
test_gam2_results_M_deciles <- cut(test_gam2_results_M$pred_time2_M_GAM, breaks=quantile(test_gam2_results_M$pred_time2_M_GAM, probs=c(0:10/10)), labels=1:10,
                                   include.lowest=TRUE)
test_gam2_results_M$decile <- test_gam2_results_M_deciles
test_gam2_results_M <- test_gam2_results_M[order(-test_gam2_results_M$pred_time2_M_GAM),]
 
#ADDING THE FOLLOWING TO DATASET
#name, email, firm, avg recency, avg frequency, median monetary,
#avg number of registered states, avg number years as an agent,
#avg number of years at prior firm, avg number of years at current firm
 
#RECENT RAMBOS
 
test_gam2_results_R_marketing <- join_all(list(test_gam2_results_R, brokers_full),
                        by = 'npn', type = 'left')
 
test_gam2_results_R_marketing_results <- test_gam2_results_R_marketing %>%
  group_by(decile) %>%
  dplyr::mutate(avg_recency = mean(days_old, na.rm=TRUE)) %>%
  dplyr::mutate(avg_frequency = mean(num_tickets, na.rm=TRUE)) %>%
  dplyr::mutate(med_monetary = median(avg_premium, na.rm=TRUE)) %>%
  dplyr::mutate(avg_number_registeredstates = mean(number_registeredstates, na.rm=TRUE)) %>%
  dplyr::mutate(avg_number_yearsanagent = mean(number_yearsanagent, na.rm=TRUE)) %>%
  dplyr::mutate(avg_priorfirm1_numberofyears = mean(priorfirm1_numberofyears, na.rm=TRUE)) %>%
  dplyr::mutate(avg_age = mean(age, na.rm=TRUE)) %>%
  dplyr::mutate(pred_time2_R_GAM_min = min(pred_time2_R_GAM)) %>%
 dplyr::mutate(pred_time2_R_GAM_max = max(pred_time2_R_GAM))
 
#summary statistics
 
test_gam2_results_R_marketing %>%
  group_by(decile) %>%
  dplyr::summarise(pred_time2_R_GAM_min = min(pred_time2_R_GAM),
                   pred_time2_R_GAM_max = max(pred_time2_R_GAM))
 
test_gam2_results_R_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_recency = mean(days_old, na.rm=TRUE))
 
test_gam2_results_R_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_frequency = mean(num_tickets, na.rm=TRUE))
 
test_gam2_results_R_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(med_monetary = median(avg_premium, na.rm=TRUE))
 
test_gam2_results_R_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_number_registeredstates = mean(number_registeredstates, na.rm=TRUE))
 
test_gam2_results_R_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_number_yearsanagent = mean(number_yearsanagent, na.rm=TRUE))
 
test_gam2_results_R_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_priorfirm1_numberofyears = mean(priorfirm1_numberofyears, na.rm=TRUE))
 
test_gam2_results_R_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_age = mean(age, na.rm=TRUE))
 
#FREQUENT FLYERS
 
test_gam2_results_F_marketing <- join_all(list(test_gam2_results_F, brokers_full),
                                          by = 'npn', type = 'left')
 
test_gam2_results_F_marketing_results <- test_gam2_results_F_marketing %>%
  group_by(decile) %>%
  dplyr::mutate(avg_recency = mean(days_old, na.rm=TRUE)) %>%
  dplyr::mutate(avg_frequency = mean(num_tickets, na.rm=TRUE)) %>%
  dplyr::mutate(med_monetary = median(avg_premium, na.rm=TRUE)) %>%
  dplyr::mutate(avg_number_registeredstates = mean(number_registeredstates, na.rm=TRUE)) %>%
  dplyr::mutate(avg_number_yearsanagent = mean(number_yearsanagent, na.rm=TRUE)) %>%
  dplyr::mutate(avg_priorfirm1_numberofyears = mean(priorfirm1_numberofyears, na.rm=TRUE)) %>%
  dplyr::mutate(avg_age = mean(age, na.rm=TRUE))%>%
  dplyr::mutate(pred_time2_F_GAM_min = min(pred_time2_F_GAM)) %>%
  dplyr::mutate(pred_time2_F_GAM_max = max(pred_time2_F_GAM))
 
#summary statistics
 
test_gam2_results_F_marketing %>%
  group_by(decile) %>%
  dplyr::summarise(pred_time2_F_GAM_min = min(pred_time2_F_GAM),
                   pred_time2_F_GAM_max = max(pred_time2_F_GAM))
 
test_gam2_results_F_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_recency = mean(days_old, na.rm=TRUE))
 
test_gam2_results_F_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_frequency = mean(num_tickets, na.rm=TRUE))
 
test_gam2_results_F_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(med_monetary = median(avg_premium, na.rm=TRUE))
 
test_gam2_results_F_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_number_registeredstates = mean(number_registeredstates, na.rm=TRUE))
 
test_gam2_results_F_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_number_yearsanagent = mean(number_yearsanagent, na.rm=TRUE))
 
test_gam2_results_F_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_priorfirm1_numberofyears = mean(priorfirm1_numberofyears, na.rm=TRUE))
 
test_gam2_results_F_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_age = mean(age, na.rm=TRUE))
 
#MONEY MAKERS
 
test_gam2_results_M_marketing <- join_all(list(test_gam2_results_M, brokers_full),
                                          by = 'npn', type = 'left')
 
test_gam2_results_M_marketing_results <- test_gam2_results_M_marketing %>%
  group_by(decile) %>%
  dplyr::mutate(avg_recency = mean(days_old, na.rm=TRUE)) %>%
  dplyr::mutate(avg_frequency = mean(num_tickets, na.rm=TRUE)) %>%
  dplyr::mutate(med_monetary = median(avg_premium, na.rm=TRUE)) %>%
  dplyr::mutate(avg_number_registeredstates = mean(number_registeredstates, na.rm=TRUE)) %>%
  dplyr::mutate(avg_number_yearsanagent = mean(number_yearsanagent, na.rm=TRUE)) %>%
  dplyr::mutate(avg_priorfirm1_numberofyears = mean(priorfirm1_numberofyears, na.rm=TRUE)) %>%
  dplyr::mutate(avg_age = mean(age, na.rm=TRUE))%>%
  dplyr::mutate(pred_time2_M_GAM_min = min(pred_time2_M_GAM)) %>%
  dplyr::mutate(pred_time2_M_GAM_max = max(pred_time2_M_GAM))
 
#summary statistics
 
test_gam2_results_M_marketing %>%
  group_by(decile) %>%
  dplyr::summarise(pred_time2_M_GAM_min = min(pred_time2_M_GAM),
                   pred_time2_M_GAM_max = max(pred_time2_M_GAM))
 
test_gam2_results_M_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_recency = mean(days_old, na.rm=TRUE))
 
test_gam2_results_M_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_frequency = mean(num_tickets, na.rm=TRUE))
 
test_gam2_results_M_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(med_monetary = median(avg_premium, na.rm=TRUE))
 
test_gam2_results_M_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_number_registeredstates = mean(number_registeredstates, na.rm=TRUE))
 
test_gam2_results_M_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_number_yearsanagent = mean(number_yearsanagent, na.rm=TRUE))
 
test_gam2_results_M_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_priorfirm1_numberofyears = mean(priorfirm1_numberofyears, na.rm=TRUE))
 
test_gam2_results_M_marketing %>%
  group_by(decile) %>%
  dplyr::summarize(avg_age = mean(age, na.rm=TRUE))
 
#percent of prospective brokers with emails in discovery
 
test_gam2_results_R_marketing_results_email <- filter(test_gam2_results_R_marketing_results, target == 0)
test_gam2_results_R_marketing_results_email_final <- filter(test_gam2_results_R_marketing_results_email, email_businesstype != 'Unknown')
nrow(test_gam2_results_R_marketing_results_email_final)/nrow(test_gam2_results_R_marketing_results_email)
 
test_gam2_results_F_marketing_results_email <- filter(test_gam2_results_F_marketing_results, target == 0)
test_gam2_results_F_marketing_results_email_final <- filter(test_gam2_results_F_marketing_results_email, email_businesstype != 'Unknown')
nrow(test_gam2_results_F_marketing_results_email_final)/nrow(test_gam2_results_F_marketing_results_email)
 
test_gam2_results_M_marketing_results_email <- filter(test_gam2_results_M_marketing_results, target == 0)
test_gam2_results_M_marketing_results_email_final <- filter(test_gam2_results_M_marketing_results_email, email_businesstype != 'Unknown')
nrow(test_gam2_results_M_marketing_results_email_final)/nrow(test_gam2_results_M_marketing_results_email)
 
#summary statistics on predictions
 
nrow(filter(test_gam2_results_R_final, pred_time2_R_GAM > .75))
nrow(filter(test_gam2_results_F_final, pred_time2_F_GAM > .75))
nrow(filter(test_gam2_results_M_final, pred_time2_M_GAM > .75))
nrow(filter(test, Class == 0))
#30618 of 83329 (36.7%) of prospects have a score of .5 or higher
#12940 of 83329 (15.5%) of prospects have a score of .75 or higher
 
#ECDF curves
 
cdf_R <- ecdf(test_gam2_results_R_final$pred_time2_R_GAM)
 
ggplot(test_gam2_results_R_final, aes(x = pred_time2_R_GAM))+
  stat_ecdf(size=1)
 
cdf_F <- ecdf(test_gam2_results_F_final$pred_time2_F_GAM)
 
ggplot(test_gam2_results_F_final, aes(x = pred_time2_F_GAM))+
  stat_ecdf(size=1)
 
cdf_M <- ecdf(test_gam2_results_M_final$pred_time2_M_GAM)
 
ggplot(test_gam2_results_M_final, aes(x = pred_time2_M_GAM))+
  stat_ecdf(size=1)

rm(list = ls())

library(dplyr)
library(tree)
library(randomForest)
library(mde)

set.seed(1989)

# Read in Data
dataset <- read.table("MI.data", sep = ",", dec = ".")
dataset <- dataset %>%
  mutate_all(as.character) %>%
  mutate_all(~na_if(., "?")) %>% 
  mutate_all(as.integer)


# Rename data?
names(dataset) <- c("ID", "AGE", "SEX", "INF_ANAM", "STENOK_AN", "FK_STENOK", "IBS_POST",
                    "IBS_NASL", "GB", "SIM_GIPERT", "DLIT_AG", "ZSN_A", "nr11", "nr01", "nr02",
                    "nr03", "nr04", "nr07", "nr08", "np01", "np04", "np05", "np07", "np08",
                    "np09", "np10", "endocr_01", "endocr_02", "endocr_03", "zab_leg_01",
                    "zab_leg_02", "zab_leg_03", "zab_leg_04", "zab_leg_06", "S_AD_KBRIG",
                    "D_AD_KBRIG", "S_AD_ORIT", "D_AD_ORIT", "O_L_POST", "K_SH_POST", "MP_TP_POST",
                    "SVT_POST", "GT_POST", "FIB_G_POST", "ant_im", "lat_im", "inf_im", "post_im",
                    "IM_PG_P", "ritm_ecg_p_01", "ritm_ecg_p_02", "ritm_ecg_p_04", "ritm_ecg_p_06",
                    "ritm_ecg_p_07", "ritm_ecg_p_08", "n_r_ecg_p_01", "n_r_ecg_p_02",
                    "n_r_ecg_p_03", "n_r_ecg_p_04", "n_r_ecg_p_05", "n_r_ecg_p_06",
                    "n_r_ecg_p_08", "n_r_ecg_p_09", "n_r_ecg_p_10", "n_p_ecg_p_01",
                    "n_p_ecg_p_03", "n_p_ecg_p_04", "n_p_ecg_p_05", "n_p_ecg_p_06",
                    "n_p_ecg_p_07", "n_p_ecg_p_08", "n_p_ecg_p_09", "n_p_ecg_p_10",
                    "n_p_ecg_p_11", "n_p_ecg_p_12", "fibr_ter_01", "fibr_ter_02", "fibr_ter_03",
                    "fibr_ter_05", "fibr_ter_06", "fibr_ter_07", "fibr_ter_08", "GIPO_K", "K_BLOOD",
                    "GIPER_Na", "Na_BLOOD", "ALT_BLOOD", "AST_BLOOD", "KFK_BLOOD", "L_BLOOD", "ROE",
                    "TIME_B_S", "R_AB_1_n", "R_AB_2_n", "R_AB_3_n", "NA_KB", "NOT_NA_KB", "LID_KB",
                    "NITR_S", "NA_R_1_n", "NA_R_2_n", "NA_R_3_n", "NOT_NA_1_n", "NOT_NA_2_n",
                    "NOT_NA_3_n", "LID_S_n", "B_BLOK_S_n", "ANT_CA_S_n", "GEPAR_S_n", "ASP_S_n",
                    "TIKL_S_n", "TRENT_S_n", "FIBR_PREDS", "PREDS_TAH", "JELUD_TAH", "FIBR_JELUD",
                    "A_V_BLOK", "OTEK_LANC", "RAZRIV", "DRESSLER", "ZSN", "REC_IM", "P_IM_STEN",
                    "LET_IS")



# Get summary of all outcomes
summary(dataset %>% dplyr::select(c("FIBR_PREDS", "PREDS_TAH", "JELUD_TAH",
                                    "FIBR_JELUD", "A_V_BLOK", "OTEK_LANC",
                                    "RAZRIV", "DRESSLER", "ZSN", "REC_IM",
                                    "P_IM_STEN", "LET_IS")))

# Assuming your dataset is named 'dataset'
variable_names <- c("FIBR_PREDS", "PREDS_TAH", "JELUD_TAH", "FIBR_JELUD",
                    "A_V_BLOK", "OTEK_LANC", "RAZRIV", "DRESSLER", "ZSN",
                    "REC_IM", "P_IM_STEN", "LET_IS")

# Selecting the variables of interest
selected_vars <- dataset %>%
  select(all_of(variable_names))

# Convert factors to numeric (if needed)
selected_vars <- lapply(selected_vars, as.numeric)
#
# FK_STENOK, IBS_POST, GB, ZSN_A
# ant_im, lat_im, inf_im, post_im
# L_BLOOD, TIME_B_S
# R_AB_1_n, R_AB_2_n, R_AB_3_n
# NA_R_1_n, NA_R_2_n, NA_R_3_n, NOT_NA_1_n
# NOT_NA_2_n, NOT_NA_3_n

# Calculate counts and percentages for each variable
counts_and_percentages <- sapply(selected_vars, function(x) {
  counts <- sum(x)
  percentages <- (counts / length(x)) * 100
  c(Counts = counts, Percentages = percentages)
})

# View(counts_and_percentages)


# Create binary complications variable & Death
dataset <- dataset %>% mutate(any_complication = ifelse(FIBR_PREDS == 1  | 
                                                          PREDS_TAH ==1 | 
                                                          JELUD_TAH == 1 | 
                                                          FIBR_JELUD ==1 | 
                                                          A_V_BLOK == 1 | 
                                                          OTEK_LANC == 1 | 
                                                          RAZRIV ==1 | 
                                                          DRESSLER == 1| 
                                                          ZSN == 1 | 
                                                          REC_IM == 1| 
                                                          P_IM_STEN == 1  , 1, 0),
                              death = ifelse(LET_IS == 0, 0, 1))

# Get counts of binary death variable
table(dataset$death)
table(dataset$any_complication)

# Some of the variables have a lot of NAs
summary(dataset)
# Get percent missing for each vairable
mi.percent <- sort_by_missingness(dataset, sort_by = "percents")


# Only keep variables  with <10% missing
# IBS_NASL, S_AD_KBRIG, KFK_BLOOD, D_AD_KBRIG - let's remove these since it's WAY Too much missing
dataset <- dataset %>% select(-c("KFK_BLOOD", "IBS_NASL",	"S_AD_KBRIG",
                                 "D_AD_KBRIG",	"NOT_NA_KB",	"LID_KB",
                                 "NA_KB",	"GIPER_Na",	"Na_BLOOD",	"K_BLOOD",
                                 "GIPO_K",	"AST_BLOOD",	"ALT_BLOOD",
                                 "S_AD_ORIT",	"D_AD_ORIT",	"DLIT_AG",	"ROE"))


# Turn to facotr, keep a few as numeric
dataset <- lapply(dataset, as.factor)
dataset$AGE <- as.numeric(dataset$AGE)
dataset$L_BLOOD <- as.numeric(dataset$L_BLOOD)
dataset$TIME_B_S <- as.numeric(dataset$TIME_B_S)
setDT(dataset)

# Complete Case - drop individauls with missing values
dataset.complete.case = na.omit(dataset)

# Get counts pre and post complete case analysis
nrow(dataset)
nrow(dataset.complete.case)

# Sample indices
n = nrow(dataset.complete.case)
indices <- sample(1:n, replace = F, size = n*0.70)

# Create train and test 
# Remove the 11 complications 
# From documenationL: all input columns (2-112) except 93 (R_AB_1_n), 94 (R_AB_2_n), 95 (R_AB_3_n), 100 (NA_R_1_n), 101 (NA_R_2_n), 102(NA_R_3_n),
# 103 (NOT_NA_1_n), 104 (NOT_NA_2_n), 105 (NOT_NA_3_n) can be used for prediction;

data_train = dataset.complete.case[indices,] %>%
  select(-c('ID', "FIBR_PREDS", "PREDS_TAH", "JELUD_TAH", "FIBR_JELUD",
            "A_V_BLOK", "OTEK_LANC", "RAZRIV", "DRESSLER", "ZSN", "REC_IM",
            "P_IM_STEN", "LET_IS", "R_AB_1_n", "R_AB_2_n", "R_AB_3_n",
            "NA_R_1_n",  "NA_R_2_n", "NOT_NA_1_n", "NOT_NA_2_n", "NOT_NA_3_n"))

data_test = dataset.complete.case[-indices,] %>%
  select(-c('ID', "FIBR_PREDS", "PREDS_TAH", "JELUD_TAH", "FIBR_JELUD",
            "A_V_BLOK", "OTEK_LANC", "RAZRIV", "DRESSLER", "ZSN", "REC_IM",
            "P_IM_STEN", "LET_IS", "R_AB_1_n", "R_AB_2_n", "R_AB_3_n",
            "NA_R_1_n",  "NA_R_2_n", "NOT_NA_1_n", "NOT_NA_2_n", "NOT_NA_3_n"))

# THIS INCLUDE MISSING DATA
# Sample indices
n = nrow(dataset)
indices <- sample(1:n, replace = F, size = n*0.70)

# Create train and test 
# Remove the 11 complications 
# From documenationL: all input columns (2-112) except 93 (R_AB_1_n), 94 (R_AB_2_n), 95 (R_AB_3_n), 100 (NA_R_1_n), 101 (NA_R_2_n), 102(NA_R_3_n),
# 103 (NOT_NA_1_n), 104 (NOT_NA_2_n), 105 (NOT_NA_3_n) can be used for prediction;

data_train.mi = dataset[indices,] %>%
  select(-c('ID', "FIBR_PREDS", "PREDS_TAH", "JELUD_TAH", "FIBR_JELUD",
            "A_V_BLOK", "OTEK_LANC", "RAZRIV", "DRESSLER", "ZSN", "REC_IM",
            "P_IM_STEN", "LET_IS", "R_AB_1_n", "R_AB_2_n", "R_AB_3_n",
            "NA_R_1_n",  "NA_R_2_n", "NOT_NA_1_n", "NOT_NA_2_n", "NOT_NA_3_n"))

data_test.mi = dataset[-indices,] %>%
  select(-c('ID', "FIBR_PREDS", "PREDS_TAH", "JELUD_TAH", "FIBR_JELUD",
            "A_V_BLOK", "OTEK_LANC", "RAZRIV", "DRESSLER", "ZSN", "REC_IM",
            "P_IM_STEN", "LET_IS", "R_AB_1_n", "R_AB_2_n", "R_AB_3_n",
            "NA_R_1_n",  "NA_R_2_n", "NOT_NA_1_n", "NOT_NA_2_n", "NOT_NA_3_n"))

# #################
# Fully grown tree using Tree()
# #################
# Complete case
outcome = "any_complication"

if(outcome == "any_complication"){
  temp_train <- data_train %>% select(-c("death"))
  temp_test  <- data_test %>% select(-c("death"))
}else if(outcome == "death"){
  temp_train <- data_train %>% select(-c("any_complication"))
  temp_test <-  data_test %>% select(-c("any_complication"))
}

tree_mod.cc<-tree(any_complication~., temp_train,
                  minsize=2, 
                  model=T)

tree_pred.cc<-predict(tree_mod.cc, newdata=temp_test, type = "class")
tree.cc.error <- 1 - sum(diag(table(temp_test$any_complication, tree_pred.cc)))/length(temp_test$any_complication)

# Not complete case
if(outcome == "any_complication"){
  temp_train.mi <- data_train.mi %>% select(-c("death"))
  temp_test.mi  <- data_test.mi %>% select(-c("death"))
}else if(outcome == "death"){
  temp_train.mi <- data_train.mi %>% select(-c("any_complication"))
  temp_test.mi <-  data_test.mi %>% select(-c("any_complication"))
}

tree_mod.mi<-tree(any_complication~., temp_train.mi,
                  minsize=2, 
                  model=T)


tree_pred.cc<-predict(tree_mod.mi, newdata=temp_test.mi, type = "class") 
tree.error <- 1 - sum(diag(table(temp_test.mi$any_complication, tree_pred.cc)))/length(temp_test.mi$any_complication)

nobs.used <- 760/nrow(temp_train.mi)

# #################
# Fully grown tree using rpart() - surrogate splits
# #################
rpart.tree.surrogate <- rpart(any_complication ~ ., data = temp_train.mi,  method = "class" ,
                              control = rpart.control(usesurrogate = 1))

rpart.pred.surrogate<-predict(rpart.tree.surrogate,temp_test.mi, type = "class")
rpart.error.surrogate <- 1 - sum(diag(table(temp_test.mi$any_complication, rpart.pred.surrogate)))/length(temp_test.mi$any_complication)

rpart.tree.surrogate.2 <- rpart(any_complication ~ ., data = temp_train.mi,  method = "class" ,
                              control = rpart.control(usesurrogate = 2))

rpart.pred.surrogate.2<-predict(rpart.tree.surrogate.2,temp_test.mi, type = "class")
rpart.error.surrogate.2 <- 1 - sum(diag(table(temp_test.mi$any_complication, rpart.pred.surrogate.2)))/length(temp_test.mi$any_complication)

# ###############
# Random Forest - na.action = na.roughfix
# ###############
# Complete Case analysis
rand_forest<-randomForest(as.factor(any_complication) ~., data=temp_train, ntree=200,  
                          nodesize=1, importance=T, 
                          type='classification')
rand_forest_pred<-predict(rand_forest, newdata=temp_test, type = "class")
rand.forst.error <- 1 - sum(diag(table(temp_test$any_complication, rand_forest_pred)))/length(temp_test$any_complication)

# With missing data - na.roughfix
rand_forest.mi <-randomForest(as.factor(any_complication) ~., data=temp_train.mi, ntree=200,  
                              nodesize=1, importance=T, 
                              type='classification', na.action = na.roughfix)

# Issue with prediction on those with missing data in the TEST dataset TOO
rand_forest_pred<-predict(rand_forest.mi, newdata=na.omit(temp_test.mi), type = "class")
rand.forst.error.roughfix <- 1 - sum(diag(table(na.omit(temp_test.mi)$any_complication, rand_forest_pred)))/length(na.omit(temp_test.mi)$any_complication)

# With missing data - proximity
temp_train.mi$any_complication <- as.factor(temp_train.mi$any_complication )
rand.forest.imputed <- rfImpute(any_complication ~., data=temp_train.mi, ntree=200)
rf.imputed <- randomForest(any_complication  ~., data=rand.forest.imputed)
rf.imputed.pred<-predict(rf.imputed, newdata=na.omit(temp_test.mi), type = "class")
rf.imputed.error.proximity <- 1 - sum(diag(table(na.omit(temp_test.mi)$any_complication, rf.imputed.pred)))/length(na.omit(temp_test.mi)$any_complication)


# #####
# Adaboost
# #####
library(gbm)

adaboost <- gbm(any_complication ~., data=temp_train,
                distribution="adaboost",
                n.trees=200, shrinkage=0.01)
pretty.gbm.tree(adaboost, i.tree= 2)

adaboost_pred<-ifelse(
  predict.gbm(adaboost, newdata = temp_test, n.trees=200, type="response")>0.5, 
  1,0)
adaboost.error <- 1 - sum(diag(table(na.omit(temp_test)$any_complication, adaboost_pred)))/length(na.omit(temp_test)$any_complication)


return{list(tree.cc.error,tree.error,rpart.error.surrogate.1,rpart.error.surrogate.2,rf.imputed.error.roughfix, rf.imputed.error.proximity,adaboost.error)}
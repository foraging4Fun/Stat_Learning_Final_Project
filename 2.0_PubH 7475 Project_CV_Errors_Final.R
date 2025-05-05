
rm(list = ls())

library(dplyr)
library(data.table)
library(tree)
library(randomForest)
library(mde)
library(gbm)

######################
# Data preparation
######################

# Read in Data
dataset <- read.table("Myocardial infarction complications Database.csv", sep = ",", dec = ".")
source("methods_function_final.R")

dataset <- dataset %>%
  mutate_all(as.character) %>%
  mutate_all(~na_if(., "?")) %>% 
  mutate_all(as.integer)

# Rename data
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
summary(dataset %>% dplyr::select(c("FIBR_PREDS", "PREDS_TAH", "JELUD_TAH", "FIBR_JELUD",
                "A_V_BLOK", "OTEK_LANC", "RAZRIV", "DRESSLER", "ZSN", "REC_IM", "P_IM_STEN",
                "LET_IS")))

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

data_all = dataset.complete.case %>%
  select(-c('ID', "FIBR_PREDS", "PREDS_TAH", "JELUD_TAH", "FIBR_JELUD",
            "A_V_BLOK", "OTEK_LANC", "RAZRIV", "DRESSLER", "ZSN", "REC_IM",
            "P_IM_STEN", "LET_IS", "R_AB_1_n", "R_AB_2_n", "R_AB_3_n",
            "NA_R_1_n",  "NA_R_2_n", "NOT_NA_1_n", "NOT_NA_2_n", "NOT_NA_3_n"))

setDT(data_all)

######################
# Run CV errors 
######################

# Function to apply k-fold validation to function from METHODS
k_fold_errors<-function(data, n_folds, outcome_inp){
  
        # Shuffle data
      data<-data[sample(1:nrow(data)), ] 
      
      # generate array containing fold-number for each sample (row)
      folds <- rep_len(1:n_folds, nrow(data))
      
      # Vector to store error rates
      loocv_error_vec <- c()
      loocv_auc_vec<-c()
      
      ## loop over the K folds
      for (i in 1:n_folds){
          
        # TRAIN AND TEST
        
          # Test_ids
          fold_test_ids <- which(folds == i)
          
          # save temporary testing dataset for the current fold:
          loocv_test<- data[fold_test_ids,]
          
          # save temporary training dataset for the current fold:
          loocv_train <- data[-fold_test_ids,]
          
          # CALCULATE ERRORS USING FUNCTION FROM A
          error_i<-methods_errors(outcome = outcome_inp, 
                                  training_dt =loocv_train , 
                                  test_dt = loocv_test)
          
          
          auc_i<-c(error_i$roc_tree$auc, 
                   error_i$roc_pruned_tree$auc,
                   error_i$roc_rand_forest$auc,
                   error_i$roc_adaboost$auc)
          
          names(auc_i)<-c("tree" ,"pruned_tree", "rand_forest"  ,  "adaboost")
          
          loocv_error_vec<-rbind(loocv_error_vec,error_i$errors)
          loocv_auc_vec<-rbind(loocv_auc_vec,auc_i)
          
      }
      
      # Get final error
      CV_errors<-apply(loocv_error_vec,2,mean)
      CV_auc<-apply(loocv_auc_vec,2,mean)
      output<-list(CV_errors,CV_auc )
      names(output)<-c("CV_errors","CV_auc" )
      return(output)
  
}


# ANY COMPLICATION

data_inp<-copy(data_all)
data_inp[, death:=NULL]

any_comp<-k_fold_errors(data_inp, 
              10,
              "any_complication")


# ANY COMPLICATION

data_inp<-copy(data_all)
data_inp[, any_complication:=NULL]

death<-k_fold_errors(data_inp, 
                        10,
                        "death")


results<-rbind(
  any_comp$CV_errors,
  any_comp$CV_auc,
  death$CV_errors,
  death$CV_auc
)


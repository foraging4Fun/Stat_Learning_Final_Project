
rm(list = ls())

library(dplyr)
library(data.table)
library(tree)
library(randomForest)
library(mde)
library(readxl)
library(knitr)
library(kableExtra)
library(pROC)
library(gbm)

set.seed(1989)

######################
# Data preparation
######################

# Read in Data
dataset <- read.table("MI.data", sep = ",", dec = ".")
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

# Assuming your dataset is named 'dataset'
variable_names <- c("FIBR_PREDS", "PREDS_TAH", "JELUD_TAH", "FIBR_JELUD",
                    "A_V_BLOK", "OTEK_LANC", "RAZRIV", "DRESSLER", "ZSN", "REC_IM", "P_IM_STEN",
                    "LET_IS")

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

######################
# Run methods - any complication
######################

# Remember to get rid of death 
setDT(data_train)
setDT(data_test)

data_train.1 <- copy(data_train)
data_test.1 <- copy(data_test)

data_train.1 <- data_train.1[, death := NULL]
data_test.1 <- data_test.1[, death := NULL]

output <- methods_errors(outcome = "any_complication", 
               training_dt = data_train.1, 
               test_dt = data_test.1)

any_errors <- output$errors
adaboost_imp <- output$adaboost_imp
rand_forest_imp <- as.data.frame(output$rand_forest_imp)

##################
# Get table with selected variables by method
##################

top_10_tree <- output$tree_vars
top_10_pruned <- output$pruned_tree_vars

#reorder
rand_forest_ordered_imp <- rand_forest_imp[order(rand_forest_imp$MeanDecreaseGini, 
                                                 decreasing = T), ]
top_10_rand_forest <- row.names(rand_forest_ordered_imp[1:10,])
top_10_adaboost <- row.names(adaboost_imp[1:10,])

# get all possible variable names
vars_mi = names(data_train.1)
nvar = length(vars)

# create empty dataframe
initial.df <- data.frame(variables = vars_mi,
                         tree_col = as.vector(rep(x = NA, nvar)),
                         pruned_col = as.vector(rep(x = NA, nvar)),
                         rand_forest_col = as.vector(rep(x = NA, nvar)),
                         boost_col = as.vector(rep(x = NA, nvar)))

# function to fill the dataframe based on top 10 variables
fill_df <- function(top_10_vector, df, column_name) {
  for (j in 1:length(df$variables)) {
    match_found <- FALSE
    for (i in 1:length(top_10_vector)) {
      if (df$variables[j] == top_10_vector[i]) {
        df[[column_name]][j] <- i
        match_found <- TRUE
        break
      }
    }
    if (!match_found) {
      df[[column_name]][j] <- NA
    }
  }
  return(df)
}

# fill the dataframe for each method
initial.df <- fill_df(top_10_tree, initial.df, "tree_col")
initial.df <- fill_df(top_10_pruned, initial.df, "pruned_col")
initial.df <- fill_df(top_10_rand_forest, initial.df, "rand_forest_col")
initial.df <- fill_df(top_10_adaboost, initial.df, "boost_col")
variable_key <- read_excel("variable key.xlsx", 
                           col_names = T)

#add in descriptions
initial.df = initial.df[-87,] # first get rid of outcome
for (i in 1:length(variable_key$key)){
  match_found <- FALSE
  for (j in 1:length(initial.df$variables)) {
    if (variable_key$key[i] == initial.df$variables[j]) {
      initial.df$description[j] <- variable_key$description[i]
      match_found <- TRUE
      break
    }
  }
  if (!match_found) {
    i=i+1
  }
}

# get list of variables that were not selected for the footnote
na.vars.any = initial.df %>%
  filter(is.na(tree_col) & is.na(pruned_col) & 
           is.na(rand_forest_col) & is.na(boost_col)) %>%
  select(variables) %>%
  unlist() %>%
  as.vector()

#create footnote
fn1 = 'Variables not selected were (additional description found in the appendix):'
fn2 =  paste(gsub('"', '', paste(na.vars.any, collapse = ", ")))
footnote.any.comp = paste(fn1, fn2)

# filter out the variables that were not selected
initial.df = initial.df %>% 
  relocate(description, .after = variables) %>%
  filter(!(is.na(tree_col) & is.na(pruned_col) & 
           is.na(rand_forest_col) & is.na(boost_col))) %>%
  mutate(ind1 = is.na(tree_col),
         ind2 = is.na(pruned_col),
         ind3 = is.na(rand_forest_col),
         ind4 = is.na(boost_col),
         times_selected = ind1 + ind2 + ind3 + ind4) %>%
  rowwise() %>%
  mutate(row_sum = sum(tree_col, pruned_col, rand_forest_col, boost_col, na.rm = T), 
         .after = times_selected) %>%
  select(-c(ind1, ind2, ind3, ind4))

# order table
o1 <- with(initial.df, order(times_selected, row_sum))
initial.df <- initial.df[o1, ]
initial.df <- initial.df %>%
  select(-c(times_selected, row_sum))

#replace NA with blank
options(knitr.kable.NA = '')

# add on errors
errors_final <- paste(c("", "RSME", round(any_errors, digits = 4)*100), 
                            c("", "", "%", "%", "%", "%"))
initial.df2 <- rbind(errors_final, initial.df)

final_table <- kbl(initial.df2, 
      col.names = c("Variables", "Description", "Tree", "Pruned Tree",
                    "Random Forest", "ADABoost"), 
      booktabs = T,
      caption = "Predictors selected for any complication across 4 methods") %>%
  kable_styling(latex_options = "HOLD_position") %>%
  column_spec(column = 2, width = "12cm") %>%
  row_spec(1, bold=T) %>%
  footnote(general = footnote.any.comp) 
final_table 

# importance plot for random forest
randomForest::varImpPlot(output$rand_forest, n.var =  20, main = "Variable Importance Plot - Any Complication")

# ROC PLOT
plot(output$roc_tree, col=1)
plot(output$roc_pruned_tree, add=T, col=2)
plot(output$roc_rand_forest, add=T,  col=3)
plot(output$roc_adaboost, add=T, col=4)
legend("bottomright", legend=c(paste0("Tree=", format(round(output$roc_tree$auc, 3), nsmall = 3)),
                               paste0("Pruned Tree=", format(round(output$roc_pruned_tree$auc, 3), nsmall = 3)),
                               paste0("Forest=", format(round(output$roc_rand_forest$auc, 3), nsmall = 3)),
                               paste0("Boost=", format(round(output$roc_adaboost$auc, 3), nsmall = 3))),
       col=1:4, lty=1, cex=0.8)



# Pruned Tree Plots 
plot(output$pruned_tree)
text(output$pruned_tree)

# Tree Plots  
plot(output$tree_mod)
text(output$tree_mod)

summary_dt<-setDT(summary(output$adaboost))
summary_dt<-summary_dt[order(rel.inf)]
summary_dt[, var:= factor(var, 
                          levels = summary_dt$var)]

# ADABOOST Importance 
ggplot(summary_dt[rel.inf>0,]) +
  geom_col(aes(rel.inf, var),fill="skyblue", width = 0.6)+
  ylab("Variable")+
  xlab("Relative Influence")

############################################################################################
# Run methods - death
######################

data_train.2 <- copy(data_train)
data_test.2 <- copy(data_test)

data_train.2 <- data_train.2[, any_complication := NULL]
data_test.2 <- data_test.2[, any_complication := NULL]


output_death <- methods_errors(outcome = "death", 
                         training_dt = data_train.2, 
                         test_dt = data_test.2)

death_errors <- output_death$errors

adaboost_imp_death <- output_death$adaboost_imp
rand_forest_imp_death <- as.data.frame(output_death$rand_forest_imp)

##################
# Get table with selected variables by method
##################

top_10_tree_death <- output_death$tree_vars
top_10_pruned_death <- output_death$pruned_tree_vars

#reorder
rand_forest_ordered_imp_death <- rand_forest_imp_death[order(rand_forest_imp_death$MeanDecreaseGini, 
                                                 decreasing = T), ]
top_10_rand_forest_death <- row.names(rand_forest_ordered_imp_death[1:10,])
top_10_adaboost_death <- row.names(adaboost_imp_death[1:10,])

# get all possible variable names
vars_mi = names(data_train)
nvar = length(vars)

# create empty dataframe
initial.df.death <- data.frame(variables = vars_mi,
                         tree_col = as.vector(rep(x = NA, nvar)),
                         pruned_col = as.vector(rep(x = NA, nvar)),
                         rand_forest_col = as.vector(rep(x = NA, nvar)),
                         boost_col = as.vector(rep(x = NA, nvar)))

# fill the dataframe for each method
initial.df.death <- fill_df(top_10_tree_death, initial.df.death, "tree_col")
initial.df.death <- fill_df(top_10_pruned_death, initial.df.death, "pruned_col")
initial.df.death <- fill_df(top_10_rand_forest_death, initial.df.death, "rand_forest_col")
initial.df.death <- fill_df(top_10_adaboost_death, initial.df.death, "boost_col")

#add in descriptions
initial.df.death = initial.df.death[-87,] # first get rid of outcome
for (i in 1:length(variable_key$key)){
  match_found <- FALSE
  for (j in 1:length(initial.df.death$variables)) {
    if (variable_key$key[i] == initial.df.death$variables[j]) {
      initial.df.death$description[j] <- variable_key$description[i]
      match_found <- TRUE
      break
    }
  }
  if (!match_found) {
    i=i+1
  }
}

# get list of variables that were not selected for the footnote
na.vars.death = initial.df.death %>%
  filter(is.na(tree_col) & is.na(pruned_col) & 
           is.na(rand_forest_col) & is.na(boost_col)) %>%
  select(variables) %>%
  unlist() %>%
  as.vector()

#create footnote
fn1.death = 'Variables not selected were (additional description found in the appendix):'
fn2.death =  paste(gsub('"', '', paste(na.vars.death, collapse = ", ")))
footnote.death = paste(fn1.death, fn2.death)

# filter out the variables that were not selected
initial.df.death = initial.df.death %>% 
  relocate(description, .after = variables) %>%
  filter(!(is.na(tree_col) & is.na(pruned_col) & 
             is.na(rand_forest_col) & is.na(boost_col))) %>%
  mutate(ind1 = is.na(tree_col),
         ind2 = is.na(pruned_col),
         ind3 = is.na(rand_forest_col),
         ind4 = is.na(boost_col),
         times_selected = ind1 + ind2 + ind3 + ind4) %>%
  rowwise() %>%
  mutate(row_sum = sum(tree_col, pruned_col, rand_forest_col, boost_col, na.rm = T), 
         .after = times_selected) %>%
  select(-c(ind1, ind2, ind3, ind4))
  
o <- with(initial.df.death, order(times_selected, row_sum))
initial.df.death <- initial.df.death[o, ]
initial.df.death <- initial.df.death %>%
  select(-c(times_selected, row_sum))

#replace NA with blank
options(knitr.kable.NA = '')

# add on errors
death_errors_final <- paste(c("", "RSME", round(death_errors, digits = 4)*100), 
                            c("", "", "%", "%", "%", "%"))
initial.df.death2 <- rbind(death_errors_final, initial.df.death)

final_table_death <- kbl(initial.df.death2, 
                   col.names = c("Variables", "Description", "Tree", "Pruned Tree", "Random Forest", "ADABoost"), 
                   booktabs = T,
                   caption = "Predictors selected for MI death across 4 methods") %>%
  kable_styling(latex_options = "HOLD_position") %>%
  column_spec(column = 2, width = "12cm") %>%
  row_spec(1, bold=T) %>%
  footnote(general = footnote.death) 
final_table_death 

# importance plot for random forest
randomForest::varImpPlot(output_death$rand_forest, n.var =  20, main = "Variable Importance Plot - Death")

# Make data dictionary look nicer
kbl(variable_key, 
    col.names = c("Variables", "Description"), 
    booktabs = T,
    caption = "Data Dictionary for used variables") %>%
  kable_styling(latex_options = "HOLD_position")

# ROC PLOT
plot(output_death$roc_tree, col=1)
plot(output_death$roc_pruned_tree, add=T, col=2)
plot(output_death$roc_rand_forest, add=T,  col=3)
plot(output_death$roc_adaboost, add=T, col=4)
legend("bottomright", legend=c(paste0("Tree=", format(round(output_death$roc_tree$auc, 3), nsmall = 3)),
                               paste0("Pruned Tree=", format(round(output_death$roc_pruned_tree$auc, 3), nsmall = 3)),
                               paste0("Forest=", format(round(output_death$roc_rand_forest$auc, 3), nsmall = 3)),
                               paste0("Boost=", format(round(output_death$roc_adaboost$auc, 3), nsmall = 3))),
       col=1:4, lty=1, cex=0.8)



# Pruned Tree Plots 
plot(output_death$pruned_tree)
text(output_death$pruned_tree)

# Tree Plots  
plot(output_death$tree_mod)
text(output_death$tree_mod)

summary_dt<-setDT(summary(output_death$adaboost))
summary_dt<-summary_dt[order(rel.inf)]
summary_dt[, var:= factor(var, 
                          levels = summary_dt$var)]

# ADABOOST Importance 
ggplot(summary_dt[rel.inf>0,]) +
  geom_col(aes(rel.inf, var),fill="skyblue", width = 0.6)+
  ylab("Variable")+
  xlab("Relative Influence")


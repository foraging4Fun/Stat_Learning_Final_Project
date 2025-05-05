#####################################
# Models function 
####################################

# Function to carry out methods

# Input
# Outcome= character of var name 
# traning and test dt are the datasets

# Output

# List with 
  # Vector with the test errors for each model 
  # Importance for adaboost and random forest 

library(tree)
library(randomForest)
library(data.table)
library(caret)

methods_errors<-function(outcome, training_dt, test_dt, cv=F){
  
  # Data steps for function 
  setnames(training_dt, outcome, "outcome", skip_absent=TRUE)
  setnames(test_dt, outcome, "outcome", skip_absent=TRUE)
  
  
  ##################
  # Fully grown tree
  ##################        
  
  # Tree
  tree_mod<-tree( outcome~., training_dt,
                  minsize=2, 
                  model=T)
  
  
  VPat = paste0(".*(", paste(colnames(training_dt), collapse="|"), ").*")
  tree_vars<-unique(sub(VPat,"\\1", labels(tree_mod)[-1]))
  
  # Predict   
  tree_pred<-predict(tree_mod, newdata=test_dt[, -c("outcome")], type = "class")
  
  # ROC
  pred<-predict(tree_mod, newdata=test_dt[, -c("outcome")])[,2]
  roc_tree<-roc(test_dt$outcome, pred)
  
  ##################
  # Optimally pruned tree
  ##################
  
  # Get tree and do cv 
  tree_cv<-cv.tree(tree_mod)
  
  # Repeat CV several times
  if(cv){
    for(i in 2:5) {tree_cv$dev<-tree_cv$dev + cv.tree(tree_mod)$dev}
    tree_cv$dev<-tree_cv$dev/5
  }
  
  # Get smallest dev index
  s<-min(tree_cv$size[which(min(tree_cv$dev)==tree_cv$dev)])
  s<-ifelse(s==1,2,s)
  
  # Use size with smallest dev 
  pruned_tree<-prune.tree(tree_mod, best=s)
  
  VPat = paste0(".*(", paste(colnames(training_dt), collapse="|"), ").*")
  pruned_tree_vars<-unique(sub(VPat,"\\1", labels(pruned_tree)[-1]))
  
  # Predict
  pruned_tree_pred<-predict(pruned_tree, newdata=test_dt[, -c("outcome")], type = "class")
  
  
  # ROC
  pred<-predict(pruned_tree, newdata=test_dt[, -c("outcome")])[,2]
  roc_pruned_tree<-roc(test_dt$outcome, pred)
  
  ##################
  # Random forest 
  ##################
  
  # Run model 
  rand_forest<-randomForest(as.factor(outcome) ~., data=training_dt, ntree=200,  
                            nodesize=1, importance=T, 
                            type='classification')
  
  
  
  # Predict 
  rand_forest_pred<-predict(rand_forest, newdata=test_dt[, -c("outcome")], class="type")
  
  # ROC
  pred<-predict(rand_forest, newdata=test_dt[, -c("outcome")], type="prob")[,2]
  roc_rand_forest<-roc(test_dt$outcome, pred)
  
  ##################
  # Boosting
  ##################        
  
  # ADABOOST Model
  adaboost <- gbm(outcome ~., data=training_dt,
                  distribution="adaboost",
                  n.trees=200, shrinkage=0.01)
  
  # Predict   
  adaboost_pred<-ifelse(
    predict.gbm(adaboost, newdata=test_dt[, -c("outcome")], n.trees=200, type="response")>0.5,
    1,0)
  adaboost_pred.prob<-
    predict.gbm(adaboost, newdata=test_dt[, -c("outcome")], n.trees=200, type="response")
  
  # predict.gbm(adaboost, newdata=test_dt[, -c("outcome")], n.trees=200, type="response")
  # pred<-(predict(adaboost, newdata=test_dt[, -c("outcome")],class="response"))[,2]
  roc_adaboost<-roc(test_dt$outcome, adaboost_pred.prob)
  
  
  ##################
  # Get errors 
  ##################
  
  methods<-c("tree", "pruned_tree", "rand_forest", "adaboost" )
  
  errors<-c()
  
  for (met in methods){
    print(met)
    errors<-c(errors, 
              1-sum(diag(table(get(paste0(met, "_pred")),test_dt$outcome)))/length(test_dt$outcome))
  }
  
  names(errors)<- methods
  
  
  ##################
  # Get importance for Random forest and ADABOOST 
  ##################

  adaboost_imp<-summary(adaboost)
  rand_forest_imp<-rand_forest$importance

  ###################
  # FINAL OUTPUT
  ##################
  
  output<-list(errors, adaboost_imp, rand_forest_imp, pruned_tree_vars,
               tree_vars,roc_adaboost,roc_rand_forest, roc_pruned_tree, roc_tree, 
               tree_mod, pruned_tree, adaboost,rand_forest)
  names(output)<-c("errors", "adaboost_imp", "rand_forest_imp",
                   "pruned_tree_vars","tree_vars", "roc_adaboost",
                   "roc_rand_forest", "roc_pruned_tree", "roc_tree",
                   "tree_mod", "pruned_tree", "adaboost","rand_forest")
  
  return(output)
  
}





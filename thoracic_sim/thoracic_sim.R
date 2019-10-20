thor_subset_data <- function(thor){
  train_ind <- sample(nrow(thor), 100)
  train <- thor[train_ind,]
  test <- thor[-train_ind,]
  return(list(train, test))
}

thor_bag_test <- function(thor, fac, grp, trees, max_depth, complexity){
  classes <- unique(thor[,grp])
  split <- thor_subset_data(thor)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(maxdepth = max_depth, cp=complexity)
  rbag <- Reg_Bag(train, trees, fac, grp, tree_control=control)
  frwbag <- FRW_Bag(train, trees, fac, grp, tree_control=control)
  
  reg_pred_prob <- bag_prediction_prob_df(rbag, test, classes)
  frw_pred_prob <- bag_prediction_prob_df(frwbag, test, classes)
  rpnames <- colnames(reg_pred_prob)
  rpnames <- paste(rpnames, "_reg_prob", sep="")
  frwnames <- colnames(frw_pred_prob)
  frwnames <- paste(frwnames, "_frw_prob", sep="")
  colnames(reg_pred_prob) <- rpnames
  colnames(frw_pred_prob) <- frwnames
  reg_pred_class <- bag_prediction_df(rbag, test)
  frw_pred_class <- bag_prediction_df(frwbag, test)
  colnames(reg_pred_class) <- c("Reg_Predicted_Class")
  colnames(frw_pred_class) <- c("FRW_Predicted_Class")
  
  
  results <- cbind(reg_pred_class, frw_pred_class, reg_pred_prob, frw_pred_prob)
  results$true <- test[,grp]
  results$ID <- Sys.time()
  results$train_count_0 <- nrow(train[train[,grp] == 0,])
  results$train_count_1 <- nrow(train[train[,grp] == 1,])
  results$test_count_0 <- nrow(test[test[,grp] == 0,])
  results$test_count_1 <- nrow(test[test[,grp] == 1,])
  results$md <- max_depth
  results$complexity <- complexity
  results$tree_count <- trees
  
  
  return(results)
}


thor_bag_grid <- function(thor){
  fac <- colnames(thor)
  grp <- fac[14]
  fac <- fac[-14]
  
  #min_splits <- c(5, 10, 25)
  max_depths <- c(2,3,5)
  cps <- c(0.01, 0.05, 0.1)
  trees <- c(100, 500)
  outputs <- list()
  #for(ms in min_splits){
  for(md in max_depths){
    for(cp in cps){
      for(t in trees){
        test_df <- thor_bag_test(thor, fac, grp, t, md, cp)
        outputs <- c(outputs, test_df)
      }
    }
  }
  #}
  return(outputs)
}
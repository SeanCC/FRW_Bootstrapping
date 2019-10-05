iris_subset_data <- function(data){
  train_ind <- sample(nrow(data), 100)
  train <- iris[train_ind,]
  test <- iris[-train_ind,]
  return(list(train, test))
}

iris_bag_test <- function(data, fac, grp, trees, min_split, max_depth, complexity){
  classes <- unique(data[,grp])
  split <- iris_subset_data(iris)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(minsplit=min_split, maxdepth = max_depth, cp=complexity)
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
  results$train_count_setosa <- nrow(train[train[,grp] == "setosa",])
  results$train_count_versicolor <- nrow(train[train[,grp] == "versicolor",])
  results$train_count_virginica <- nrow(train[train[,grp] == "virginica",])
  results$test_count_setosa <- nrow(test[test[,grp] == "setosa",])
  results$test_count_versicolor <- nrow(test[test[,grp] == "versicolor",])
  results$test_count_virginica <- nrow(test[test[,grp] == "virginica",])
  results$md <- max_depth
  results$ms <- min_split
  results$complexity <- complexity
  results$tree_count <- trees
  return(results)
}

iris_bag_grid <- function(data){
  fac <- colnames(data)
  grp <- fac[5]
  fac <- fac[-5]
  
  min_splits <- c(5, 10, 25)
  max_depths <- c(2,3,5)
  cps <- c(0.01, 0.05, 0.1)
  trees <- c(100, 500)
  outputs <- list()
  for(ms in min_splits){
    for(md in max_depths){
      for(cp in cps){
        for(t in trees){
          test_df <- iris_bag_test(iris, fac, grp, t, ms, md, cp)
          outputs <- c(outputs, test_df)
        }
      }
    }
  }
  return(outputs)
}






iris_format_output <- function(output){
  out_df <- data.frame(matrix(ncol=22, nrow=0))
  colnames(out_df) <- c("Reg_Predicted_Class", "FRW_Predicted_Class", "pred_ID_reg_prob", "setosa_reg_prob", "versicolor_reg_prob", "virginica_reg_prob", "pred_ID_frw_prob", "setosa_frw_prob", "versicolor_frw_prob", "virginica_frw_prob", "true", "ID", "train_count_setosa", "train_count_versicolor", "train_count_virginica", "test_count_setosa", "test_count_versiolor", "test_count_virginica", "md", "ms", "complexity", "tree_count")
  for(i in seq(0, 53, 1)){
    temp_list <- output[((i*22)+1):((i*22)+22)]
    temp_df <- data.frame(temp_list)
    out_df <- rbind(out_df, temp_df)
  }
  return(out_df)
}





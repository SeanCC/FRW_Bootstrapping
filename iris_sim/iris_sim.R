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
  rpredicts <- lapply(seq(1, nrow(test)), function(x) bag_prediction_probs(rbag, test[x,], classes))
  rpredicts <- as.data.frame(do.call(rbind, rpredicts))
  colnames(rpredicts) <- lapply(classes, function(x) return(paste("reg_", x, sep="")))
  frwpredicts <- lapply(seq(1, nrow(test)), function(x) bag_prediction_probs(frwbag, test[x,], classes))
  frwpredicts <- as.data.frame(do.call(rbind, frwpredicts))
  colnames(frwpredicts) <- lapply(classes, function(x) return(paste("frw_", x, sep="")))
  results <- cbind(rpredicts, frwpredicts)
  results$true <- test[,grp]
  results$ID <- Sys.time()
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
  out_df <- data.frame(matrix(ncol=8, nrow=0))
  colnames(out_df) <- c("reg_setosa", "reg_versicolor", "reg_virginica", "frw_setosa", "frw_versicolor", "frw_virginica", "true", "ID")
  for(i in seq(0, 53, 1)){
    temp_list <- output[((i*8)+1):((i*8)+8)]
    temp_df <- data.frame(temp_list)
    out_df <- rbind(out_df, temp_df)
  }
  return(out_df)
}





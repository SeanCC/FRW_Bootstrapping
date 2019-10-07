heart_subset_data <- function(heart){
  train_ind <- sample(nrow(heart), 202)
  train <- heart[train_ind,]
  test <- heart[-train_ind,]
  return(list(train, test))
}

heart_bag_test <- function(heart, fac, grp, trees, max_depth, complexity){
  classes <- unique(heart[,grp])
  split <- heart_subset_data(heart)
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

heart_report <- function(heart){
  
}

heart_bag_grid <- function(heart){
  heart$target <- 0
  heart[heart$num > 0, 'target'] <- 1
  hrt <- heart[,-14]
  
  hfac <- colnames(hrt)
  hgrp <- hfac[14]
  hfac <- hfac[-14]
  
  #min_splits <- c(5, 10, 25)
  max_depths <- c(2,3,5)
  cps <- c(0.01, 0.05, 0.1)
  trees <- c(100, 500)
  outputs <- list()
  #for(ms in min_splits){
  for(md in max_depths){
    for(cp in cps){
      for(t in trees){
        test_df <- heart_bag_test(hrt, hfac, hgrp, t, md, cp)
        outputs <- c(outputs, test_df)
      }
    }
  }
  #}
  return(outputs)
}

heart_format_output <- function(output){
  out_df <- data.frame(matrix(ncol=17, nrow=0))
  colnames(out_df) <- c("Reg_Predicted_Class", "FRW_Predicted_Class", "pred_ID_reg_prob", "0_reg_prob", "1_reg_prob", "pred_ID_frw_prob", "0_frw_prob", "1_frw_prob", "true", "ID", "train_count_0", "train_count_1", "test_count_0", "test_count_1", "md", "complexity", "tree_count")
  for(i in seq(0, 53, 1)){
    temp_list <- output[((i*17)+1):((i*17)+17)]
    temp_df <- data.frame(temp_list)
    out_df <- rbind(out_df, temp_df)
  }
  return(out_df)
}






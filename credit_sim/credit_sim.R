library(ROCR)

credit_subset_data <- function(data, train_size){
  train_ind <- sample(nrow(data), train_size)
  train <- data[train_ind,]
  credit_subset <- data[-train_ind,]
  credit_0_subset <- credit_subset[credit_subset[,"Class"]==0,]
  credit_1_subset <- credit_subset[credit_subset[,"Class"]==1,]
  subset_0_ind <- sample(nrow(credit_0_subset), 100)
  subset_1_ind <- sample(nrow(credit_1_subset), 100)
  subset_0 <- credit_0_subset[subset_0_ind,]
  subset_1 <- credit_1_subset[subset_1_ind,]
  test <- rbind(subset_0, subset_1)
  return(list(train, test))
}

credit_bag_test <- function(credit, cfac, cgrp, trees, max_depth, complexity, train_size){
  classes <- unique(credit[,cgrp])
  split <- credit_subset_data(credit, train_size)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(maxdepth = max_depth, cp=complexity)
  rbag <- Reg_Bag(train, trees, cfac, cgrp, tree_control=control)
  frwbag <- FRW_Bag(train, trees, cfac, cgrp, tree_control=control)
  
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
  results$true <- test[,cgrp]
  results$ID <- Sys.time()
  results$train_count_0 <- nrow(train[train[,cgrp] == 0,])
  results$train_count_1 <- nrow(train[train[,cgrp] == 1,])
  results$test_count_0 <- nrow(test[test[,cgrp] == 0,])
  results$test_count_1 <- nrow(test[test[,cgrp] == 1,])
  results$train_row_count <- train_size
  results$md <- max_depth
  results$complexity <- complexity
  results$tree_count <- trees
  return(results)
}

credit_bag_grid <- function(data){
  cfac <- colnames(data)
  cgrp <- cfac[31]
  cfac <- cfac[-31]
  
  train_sizes <- c(3000, 5000, 10000)
  max_depths <- c(2,3,5)
  cps <- c(0.01, 0.05, 0.1)
  trees <- c(100, 500)
  outputs <- list()
  for(ts in train_sizes){
    for(md in max_depths){
      for(cp in cps){
        for(t in trees){
          test_df <- credit_bag_test(data, cfac, cgrp, t, md, cp, ts)
          outputs <- c(outputs, test_df)
        }
      }
    }
  }
  return(outputs)
}


credit_rf_test <- function(credit, cfac, cgrp, fac_count, trees, max_depth, complexity){
  classes <- unique(credit[,cgrp])
  split <- credit_subset_data(credit)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(maxdepth = max_depth, cp=complexity)
  rrf <- Reg_RF(train, trees, cfac, cgrp, max_fac=fac_count, tree_control=control)
  frwrf <- FRW_RF(train, trees, cfac, cgrp, fac_count, control)
  
  rpredicts <- lapply(seq(1, nrow(test)), function(x) bag_prediction_probs(rrf, test[x,], classes))
  rpredicts <- as.data.frame(do.call(rbind, rpredicts))
  colnames(rpredicts) <- lapply(classes, function(x) return(paste("reg_", x, sep="")))
  
  frwpredicts <- lapply(seq(1, nrow(test)), function(x) bag_prediction_probs(frwrf, test[x,], classes))
  frwpredicts <- as.data.frame(do.call(rbind, frwpredicts))
  colnames(frwpredicts) <- lapply(classes, function(x) return(paste("frw_", x, sep="")))
  results <- cbind(rpredicts, frwpredicts)
  results$true <- test[,cgrp]
  results$ID <- Sys.time()
  return(results)
}

credit_rf_grid <- function(data){
  cfac <- colnames(data)
  cgrp <- cfac[31]
  cfac <- cfac[-31]
  max_depths <- c(2,3,5)
  cps <- c(0.01, 0.05, 0.1)
  trees <- c(100, 500)
  fac_count <- c(3, 5, 8, 12)
  outputs <- list()
  for(md in max_depths){
    for(cp in cps){
      for(t in trees){
        for(fc in fac_count){
          test_df <- credit_rf_test(data, cfac, cgrp, fc, t, md, cp)
          outputs <- c(outputs, test_df)
        }
      }
    }
  }
  return(outputs)
}


credit_format_output <- function(output){
  out_df <- data.frame(matrix(ncol=18, nrow=0))
  colnames(out_df) <- c("Reg_Predicted_Class", "FRW_Predicted_Class", "pred_ID_reg_prob", "0_reg_prob", "1_reg_prob", "pred_ID_frw_prob", "0_frw_prob", "1_frw_prob", "true", "ID", "train_count_0", "train_count_1", "test_count_0", "test_count_1", "train_row_count", "md", "complexity", "tree_count")
  for(i in seq(0, 54, 1)){
    temp_list <- output[((i*18)+1):((i*18)+18)]
    temp_df <- data.frame(temp_list)
    out_df <- rbind(out_df, temp_df)
  }
  return(out_df)
}


normalize_credit_output <- function(output){
  output$reg_sum <- output$reg_0 + output$reg_1
  output$reg_0 <- output$reg_0/output$reg_sum
  output$reg_1 <- output$reg_1/output$reg_sum
  output$frw_sum <- output$frw_0 + output$frw_1
  output$frw_0 <- output$frw_0/output$frw_sum
  output$frw_1 <- output$frw_1/output$frw_sum
  return(output)
}


create_report_df <- function(output){
  output$pred_reg <- round(output$reg_1)
  output$pred_frw <- round(output$frw_1)
  IDs <- unique(output$ID)
  out_df <- as.data.frame(matrix(ncol=9, nrow=0))
  index <- 1
  for(ID in IDs){
    temp_df <- output[output$ID == ID,]
    reg_prob_pred <- prediction(temp_df$reg_1, temp_df$true)
    frw_prob_pred <- prediction(temp_df$frw_1, temp_df$true)
    reg_auc <- performance(reg_prob_pred, "auc")
    frw_auc <- performance(frw_prob_pred, "auc")
    reg_auc <- reg_auc@y.values[[1]]
    frw_auc <- frw_auc@y.values[[1]]
    
    reg_pred <- prediction(temp_df$pred_reg, temp_df$true)
    frw_pred <- prediction(temp_df$pred_frw, temp_df$true)
    count_1 <- nrow(temp_df[temp_df$true == 1,])
    count_0 <- nrow(temp_df[temp_df$true == 0,])
    reg_1_count <- nrow(temp_df[temp_df$reg_pred == 1,])
    reg_0_count <- nrow(temp_df[temp_df$reg_pred == 0,])
    frw_1_count <- nrow(temp_df[temp_df$frw_pred == 1,])
    frw_0_count <- nrow(temp_df[temp_df$frw_pred == 0,])
    reg_1_correct <- nrow(temp_df[temp_df$true == 1 && temp_df$reg_pred == 1,])
    reg_0_correct <- nrow(temp_df[temp_df$true == 0 && temp_df$reg_pred == 0,])
    frw_1_correct <- nrow(temp_df[temp_df$true == 1 && temp_df$frw_pred == 1,])
    frw_0_correct <- nrow(temp_df[temp_df$true == 0 && temp_df$frw_pred==0,])
    
    reg_1_recall <- reg_1_correct/count_1
    reg_0_recall <- reg_0_correct/count_0
    frw_1_recall <- frw_1_correct/count_1
    frw_0_recall <- frw_0_correct/count_0
    reg_1_precision <- reg_1_correct/reg_1_count
    reg_0_precision <- reg_0_correct/reg_0_count
    frw_1_precision <- frw_1_correct/frw_1_count
    frw_0_precision <- frw_0_correct/frw_0_count
    #row <- c(ID,count_0, count_1, reg_1_recall, reg_0_recall, frw_1_recall, frw_0_recall,
    #         reg_1_precision, reg_0_precision, frw_1_precision, frw_0_precision, reg_auc, frw_auc)
    row <- c(ID, count_0, count_1, reg_1_correct, frw_1_correct, reg_0_correct, frw_0_correct, reg_auc, frw_auc)
    out_df[index,] <- row
    index <- index+1
  }
  return(out_df)
}



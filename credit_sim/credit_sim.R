library(ROCR)

credit_subset_data <- function(data){
  train_ind <- sample(nrow(data), 3500)
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

credit_bag_test <- function(credit, cfac, cgrp, trees, max_depth, complexity){
  classes <- unique(credit[,cgrp])
  split <- credit_subset_data(credit)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(maxdepth = max_depth, cp=complexity)
  rbag <- Reg_Bag(train, trees, cfac, cgrp, tree_control=control)
  frwbag <- FRW_Bag(train, trees, cfac, cgrp, tree_control=control)
  rpredicts <- lapply(seq(1, nrow(test)), function(x) bag_prediction_probs(rbag, test[x,], classes))
  rpredicts <- as.data.frame(do.call(rbind, rpredicts))
  colnames(rpredicts) <- lapply(classes, function(x) return(paste("reg_", x, sep="")))
  frwpredicts <- lapply(seq(1, nrow(test)), function(x) bag_prediction_probs(frwbag, test[x,], classes))
  frwpredicts <- as.data.frame(do.call(rbind, frwpredicts))
  colnames(frwpredicts) <- lapply(classes, function(x) return(paste("frw_", x, sep="")))
  results <- cbind(rpredicts, frwpredicts)
  results$true <- test[,cgrp]
  results$ID <- Sys.time()
  return(results)
}

credit_bag_grid <- function(data){
  cfac <- colnames(data)
  cgrp <- cfac[31]
  cfac <- cfac[-31]
  
  max_depths <- c(2,3,5)
  cps <- c(0.01, 0.05, 0.1)
  trees <- c(100, 500)
  outputs <- list()
  for(md in max_depths){
    for(cp in cps){
      for(t in trees){
        test_df <- credit_bag_test(data, cfac, cgrp, t, md, cp)
        outputs <- c(outputs, test_df)
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
  out_df <- data.frame(matrix(ncol=6, nrow=0))
  colnames(out_df) <- c("reg_1", "reg_0", "frw_1", "frw_0", "true", "ID")
  for(i in seq(0, 71, 1)){
    print(i)
    temp_list <- output[((i*6)+1):((i*6)+6)]
    print(temp_list)
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
  for(ID in IDs){
    temp_df <- output[output$ID == ID,]
    reg_prob_pred <- prediction(output$reg_1, output$true)
    frw_prob_pred <- prediction(output$frw_1, output$true)
    reg_auc <- performance(reg_prob_pred, "auc")
    frw_auc <- performance(frw_prob_pred, "auc")
    reg_auc <- reg_auc@y.values[[1]]
    frw_auc <- frw_auc@y.values[[1]]
    
    reg_pred <- prediction(temp_df$pred_reg, temp_df$true)
    frw_pred <- prediction(temp_df$pred_rf, temp_df$true)
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

    
    
  }
}



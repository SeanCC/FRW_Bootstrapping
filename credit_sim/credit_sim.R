#library(ROCR)
#library(pROC)
library(MLmetrics)

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
  
  reg_pred_prop <- bag_prediction_prop_df(rbag, test, classes)
  frw_pred_prop <- bag_prediction_prop_df(frwbag, test, classes)
  r_prop_names <- colnames(reg_pred_prop)
  r_prop_names <- paste(r_prop_names, "reg_proportion", sep="")
  frw_prop_names <- colnames(frw_pred_prop)
  frw_prop_names <- paste(frw_prop_names, "frw_proportion", sep="")
  colnames(reg_pred_prop) <- r_prop_names
  colnames(frw_pred_prop) <- frw_prop_names
  
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
  
  results <- cbind(reg_pred_class, frw_pred_class, reg_pred_prob, frw_pred_prob, reg_pred_prop, frw_pred_prop)
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
  print("Once!")
  return(results)
}


credit_bag_grid <- function(data){
  cfac <- colnames(data)
  cgrp <- cfac[31]
  cfac <- cfac[-31]
  
  train_sizes <- c(3000, 7500)
  max_depths <- c(3, 7)
  cps <- c(0.01, 0.1)
  trees <- c(200, 600)
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


credit_rf_grid <- function(data){
  cfac <- colnames(data)
  cgrp <- cfac[31]
  cfac <- cfac[-31]
  train_sizes <- c(3000, 7500)
  max_depths <- c(3, 7)
  cps <- c(0.01, 0.1)
  trees <- c(200, 600)
  num_fac <- c(3, 8, 12)
  outputs <- list()
  for(ts in train_sizes){
    for(md in max_depths){
      for(cp in cps){
        for(t in trees){
          for(nf in num_fac){
            test_df <- credit_rf_test(data, cfac, cgrp, t, nf, md, cp, ts)
            outputs <- c(outputs, test_df)
          }
        }
      }
    }
  }
  return(outputs)
}


cc_rep_bag_grid <- function(data){
  cfac <- colnames(data)
  cgrp <- cfac[31]
  cfac <- cfac[-31]
  
  train_sizes <- c(3000)
  max_depths <- c(3, 7)
  cps <- c(0.01, 0.1)
  trees <- c(100)
  repetitions <- 5
  outputs <- list()
  for(ts in train_sizes){
    for(md in max_depths){
      for(cp in cps){
        for(t in trees){
          for(i in seq(1, repetitions)){
            test_df <- credit_bag_test(data, cfac, cgrp, t, md, cp, ts)
            outputs <- c(outputs, test_df)
          }
        }
      }
    }
  }
  return(outputs)
}

cc_rep_rf_grid <- function(data){
  cfac <- colnames(data)
  cgrp <- cfac[31]
  cfac <- cfac[-31]
  train_sizes <- c(2000)
  max_depths <- c(3, 8)
  cps <- c(0.01, 0.1)
  trees <- c(100)
  num_fac <- c(5, 10)
  repetitions <- 5
  outputs <- list()
  for(ts in train_sizes){
    for(md in max_depths){
      for(cp in cps){
        for(t in trees){
          for(nf in num_fac){
            for(i in seq(1, repetitions)){
              test_df <- credit_rf_test(data, cfac, cgrp, t, nf, md, cp, ts)
              outputs <- c(outputs, test_df)
            }
          }
        }
      }
    }
  }
  return(outputs)
}

credit_rf_test <- function(credit, cfac, cgrp, trees, num_fac, max_depth, complexity, train_size){
  classes <- unique(credit[,cgrp])
  split <- credit_subset_data(credit, train_size)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(maxdepth = max_depth, cp=complexity)
  rbag <- Reg_RF(train, trees, cfac, cgrp, max_fac=num_fac, tree_control=control)
  frwbag <- FRW_RF(train, trees, cfac, cgrp, max_fac=num_fac, tree_control=control)
  
  reg_pred_prop <- bag_prediction_prop_df(rbag, test, classes)
  frw_pred_prop <- bag_prediction_prop_df(frwbag, test, classes)
  r_prop_names <- colnames(reg_pred_prop)
  r_prop_names <- paste(r_prop_names, "reg_proportion", sep="")
  frw_prop_names <- colnames(frw_pred_prop)
  frw_prop_names <- paste(frw_prop_names, "frw_proportion", sep="")
  colnames(reg_pred_prop) <- r_prop_names
  colnames(frw_pred_prop) <- frw_prop_names
  
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
  
  results <- cbind(reg_pred_class, frw_pred_class, reg_pred_prob, frw_pred_prob, reg_pred_prop, frw_pred_prop)
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
  print("Once!")
  return(results)
}


credit_format_output <- function(output){
  out_df <- data.frame(matrix(ncol=22, nrow=0))
  colnames(out_df) <- c("Reg_Predicted_Class", "FRW_Predicted_Class", "0_reg_prob", "1_reg_prob", "0_frw_prob", "1_frw_prob", "pred_IDreg_proportion", "0reg_proportion", "1reg_proportion", "pred_IDfrw_proportion", "0frw_proportion", "1frw_proportion", "true", "ID", "train_count_0", "train_count_1", "test_count_0", "test_count_1", "train_row_count", "md", "complexity", "tree_count")
  for(i in seq(0, 19, 1)){
    temp_list <- output[((i*22)+1):((i*22)+22)]
    temp_df <- data.frame(temp_list)
    out_df <- rbind(out_df, temp_df)
  }
  return(out_df)
}



cc_report <- function(output){
  IDs <- unique(output$ID)
  out_df <- as.data.frame(matrix(ncol=32, nrow=0))
  index <- 1
  for(ID in IDs){
    temp_df <- output[output$ID == ID,]

    reg_auc <- AUC(temp_df$X1_reg_prob, temp_df$true)
    frw_auc <- AUC(temp_df$X1_frw_prob, temp_df$true)
    reg_prauc <- PRAUC(temp_df$X1_reg_prob, temp_df$true)
    frw_prauc <- PRAUC(temp_df$X1_frw_prob, temp_df$true)
    
    
    
    count_1 <- nrow(temp_df[temp_df$true == 1,])
    count_0 <- nrow(temp_df[temp_df$true == 0,])
    train_1 <- temp_df[1,"train_count_1"]
    train_0 <- temp_df[1,"train_count_0"]
    test_1 <- temp_df[1,"test_count_1"]
    test_0 <- temp_df[1,"test_count_0"]
    train_row_count <- temp_df[1,"train_row_count"]
    complexity <- temp_df[1,"complexity"]
    tree_count <- temp_df[1,"tree_count"]
    max_depth <- temp_df[1,"md"]
    #num_fac <- temp_df[1,"num_fac"]
    
    
    reg_1_count <- nrow(temp_df[temp_df$Reg_Predicted_Class == 1,])
    reg_0_count <- nrow(temp_df[temp_df$Reg_Predicted_Class == 0,])
    frw_1_count <- nrow(temp_df[temp_df$FRW_Predicted_Class == 1,])
    frw_0_count <- nrow(temp_df[temp_df$FRW_Predicted_Class == 0,])
    reg_1_correct <- nrow(temp_df[(temp_df$true == 1)&(temp_df$Reg_Predicted_Class == 1),])
    reg_0_correct <- nrow(temp_df[(temp_df$true == 0) & (temp_df$Reg_Predicted_Class == 0),])
    frw_1_correct <- nrow(temp_df[(temp_df$true == 1) & (temp_df$FRW_Predicted_Class == 1),])
    frw_0_correct <- nrow(temp_df[(temp_df$true == 0) & (temp_df$FRW_Predicted_Class==0),])
    
    reg_prob_1_avg <- mean(temp_df[temp_df$true==1,"X1_reg_prob"])
    frw_prob_1_avg <- mean(temp_df[temp_df$true==1,"X1_frw_prob"])
    reg_prob_0_avg <- mean(temp_df[temp_df$true==0, "X0_reg_prob"])
    frw_prob_0_avg <- mean(temp_df[temp_df$true==0, "X0_frw_prob"])
    
    reg_prop_1_avg <- mean(temp_df[temp_df$true==1, "X1reg_proportion"])
    frw_prop_1_avg <- mean(temp_df[temp_df$true==1, "X1frw_proportion"])
    reg_prop_0_avg <- mean(temp_df[temp_df$true==0, "X0reg_proportion"])
    frw_prop_0_avg <- mean(temp_df[temp_df$true==0, "X0frw_proportion"])
    
    reg_1_recall <- reg_1_correct/count_1
    reg_0_recall <- reg_0_correct/count_0
    frw_1_recall <- frw_1_correct/count_1
    frw_0_recall <- frw_0_correct/count_0
    reg_1_precision <- reg_1_correct/reg_1_count
    reg_0_precision <- reg_0_correct/reg_0_count
    frw_1_precision <- frw_1_correct/frw_1_count
    frw_0_precision <- frw_0_correct/frw_0_count
    
    row <- c(train_row_count, train_0, train_1, count_0, count_1, complexity, max_depth, tree_count, reg_auc, frw_auc,
             reg_prauc, frw_prauc, reg_prob_1_avg, frw_prob_1_avg, reg_prob_0_avg, frw_prob_0_avg, reg_prop_1_avg, frw_prop_1_avg,
             reg_prop_0_avg, frw_prop_0_avg,
             reg_0_correct, reg_1_correct, frw_0_correct, frw_1_correct, reg_1_recall, reg_0_recall, frw_1_recall, frw_0_recall,
             reg_1_precision, reg_0_precision, frw_1_precision, frw_0_precision)
    out_df[index,] <- row
    index <- index+1
  }
  colnames(out_df) <- c("train_row_count", "train_0", "train_1", "count_0", "count_1", "complexity", "max_depth", "tree_count", "reg_auc", "frw_auc", 
                        "reg_prauc", "frw_prauc", "reg_prob_1_avg", "frw_prob_1_avg", "reg_prob_0_avg", "frw_prob_0_avg", "reg_prop_1_avg", "frw_prop_1_avg",
                        "reg_prop_0_avg", "frw_prop_0_avg",
                        "reg_0_correct", "reg_1_correct", "frw_0_correct", "frw_1_correct", "reg_1_recall", "reg_0_recall", "frw_1_recall", "frw_0_recall",
                        "reg_1_precision", "reg_0_precision", "frw_1_precision", "frw_0_precision")
  return(out_df)
}


ccrf_report <- function(output){
  IDs <- unique(output$ID)
  out_df <- as.data.frame(matrix(ncol=29, nrow=0))
  index <- 1
  for(ID in IDs){
    temp_df <- output[output$ID == ID,]
    
    reg_auc <- AUC(temp_df$X1_reg_prob, temp_df$true)
    frw_auc <- AUC(temp_df$X1_frw_prob, temp_df$true)
    reg_prauc <- PRAUC(temp_df$X1_reg_prob, temp_df$true)
    frw_prauc <- PRAUC(temp_df$X1_frw_prob, temp_df$true)
    
    
    
    count_1 <- nrow(temp_df[temp_df$true == 1,])
    count_0 <- nrow(temp_df[temp_df$true == 0,])
    train_1 <- temp_df[1,"train_count_1"]
    train_0 <- temp_df[1,"train_count_0"]
    test_1 <- temp_df[1,"test_count_1"]
    test_0 <- temp_df[1,"test_count_0"]
    train_row_count <- temp_df[1,"train_row_count"]
    complexity <- temp_df[1,"complexity"]
    tree_count <- temp_df[1,"tree_count"]
    max_depth <- temp_df[1,"md"]
    num_fac <- temp_df[1,"num_fac"]
    
    reg_1_count <- nrow(temp_df[temp_df$Reg_Predicted_Class == 1,])
    reg_0_count <- nrow(temp_df[temp_df$Reg_Predicted_Class == 0,])
    frw_1_count <- nrow(temp_df[temp_df$FRW_Predicted_Class == 1,])
    frw_0_count <- nrow(temp_df[temp_df$FRW_Predicted_Class == 0,])
    reg_1_correct <- nrow(temp_df[(temp_df$true == 1)&(temp_df$Reg_Predicted_Class == 1),])
    reg_0_correct <- nrow(temp_df[(temp_df$true == 0) & (temp_df$Reg_Predicted_Class == 0),])
    frw_1_correct <- nrow(temp_df[(temp_df$true == 1) & (temp_df$FRW_Predicted_Class == 1),])
    frw_0_correct <- nrow(temp_df[(temp_df$true == 0) & (temp_df$FRW_Predicted_Class==0),])
    
    reg_prob_1_avg <- mean(temp_df[temp_df$true==1,"X1_reg_prob"])
    frw_prob_1_avg <- mean(temp_df[temp_df$true==1,"X1_frw_prob"])
    reg_prob_0_avg <- mean(temp_df[temp_df$true==0, "X1_reg_prob"])
    frw_prob_0_avg <- mean(temp_df[temp_df$true==0, "X1_frw_prob"])
    
    reg_1_recall <- reg_1_correct/count_1
    reg_0_recall <- reg_0_correct/count_0
    frw_1_recall <- frw_1_correct/count_1
    frw_0_recall <- frw_0_correct/count_0
    reg_1_precision <- reg_1_correct/reg_1_count
    reg_0_precision <- reg_0_correct/reg_0_count
    frw_1_precision <- frw_1_correct/frw_1_count
    frw_0_precision <- frw_0_correct/frw_0_count
    
    row <- c(train_row_count, train_0, train_1, count_0, count_1, complexity, max_depth, tree_count, num_fac, reg_auc, frw_auc,
             reg_prauc, frw_prauc, reg_prob_1_avg, frw_prob_1_avg, reg_prob_0_avg, frw_prob_0_avg,
             reg_0_correct, reg_1_correct, frw_0_correct, frw_1_correct, reg_1_recall, reg_0_recall, frw_1_recall, frw_0_recall,
             reg_1_precision, reg_0_precision, frw_1_precision, frw_0_precision)
    out_df[index,] <- row
    index <- index+1
  }
  colnames(out_df) <- c("train_row_count", "train_0", "train_1", "count_0", "count_1", "complexity", "max_depth", "tree_count", "num_fac", "reg_auc", "frw_auc", 
                        "reg_prauc", "frw_prauc", "reg_prob_1_avg", "frw_prob_1_avg", "reg_prob_0_avg", "frw_prob_0_avg",
                        "reg_0_correct", "reg_1_correct", "frw_0_correct", "frw_1_correct", "reg_1_recall", "reg_0_recall", "frw_1_recall", "frw_0_recall",
                        "reg_1_precision", "reg_0_precision", "frw_1_precision", "frw_0_precision")
  return(out_df)
}

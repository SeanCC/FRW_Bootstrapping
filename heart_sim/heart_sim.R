library(MLmetrics)

heart_subset_data <- function(heart){
  train_ind <- sample(nrow(heart), 202)
  train <- heart[train_ind,]
  test <- heart[-train_ind,]
  return(list(train, test))
}

heart_lp_subset_data <- function(heart, lcount){
  h1 <- heart[heart[,14] == 1,]
  h0 <- heart[heart[,14] == 0,]
  train1 <- sample(nrow(h1), lcount)
  train0 <- sample(nrow(h0), 110)
  test1 <- h1[-train1,]
  test0 <- h0[-train0,]
  train1 <- h1[train1,]
  train0 <- h0[train0,]
  train <- rbind(train1, train0)
  test <- rbind(test1, test0)
  return(list(train, test))
}

heart_lp_test <- function(heart, fac, grp, trees, max_depth, complexity, num_fac, lowcount){
  classes <- unique(heart[,grp])
  split <- heart_lp_subset_data(heart, lcount=lowcount)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(maxdepth = max_depth, cp=complexity)
  rbag <- Reg_Bag(train, trees, fac, grp, tree_control=control)
  frwbag <- FRW_Bag(train, trees, fac, grp, tree_control=control)
  r_rf <- Reg_RF(train, trees, fac, grp, max_fac=num_fac, tree_control=control)
  frw_rf <- FRW_RF(train, trees, fac, grp, max_fac=num_fac, tree_control=control)
  
  reg_bag_pred_prop <- bag_prediction_prop_df(rbag, test, classes)
  frw_bag_pred_prop <- bag_prediction_prop_df(frwbag, test, classes)
  r_prop_names <- colnames(reg_bag_pred_prop)
  r_prop_names <- paste(r_prop_names, "reg_bag_proportion", sep="")
  frw_prop_names <- colnames(frw_bag_pred_prop)
  frw_prop_names <- paste(frw_prop_names, "frw_bag_proportion", sep="")
  colnames(reg_bag_pred_prop) <- r_prop_names
  colnames(frw_bag_pred_prop) <- frw_prop_names
  
  reg_bag_pred_prob <- bag_prediction_prob_df(rbag, test, classes)
  frw_bag_pred_prob <- bag_prediction_prob_df(frwbag, test, classes)
  rpnames <- colnames(reg_bag_pred_prob)
  rpnames <- paste(rpnames, "_reg_bag_prob", sep="")
  frwnames <- colnames(frw_bag_pred_prob)
  frwnames <- paste(frwnames, "_frw_bag_prob", sep="")
  colnames(reg_bag_pred_prob) <- rpnames
  colnames(frw_bag_pred_prob) <- frwnames
  
  reg_bag_pred_class <- bag_prediction_df(rbag, test)
  frw_bag_pred_class <- bag_prediction_df(frwbag, test)
  colnames(reg_bag_pred_class) <- c("Reg_bag_Predicted_Class")
  colnames(frw_bag_pred_class) <- c("FRW_bag_Predicted_Class")
  
  
  
  reg_rf_pred_prop <- bag_prediction_prop_df(r_rf, test, classes)
  frw_rf_pred_prop <- bag_prediction_prop_df(frw_rf, test, classes)
  r_prop_names <- colnames(reg_rf_pred_prop)
  r_prop_names <- paste(r_prop_names, "reg_rf_proportion", sep="")
  frw_prop_names <- colnames(frw_rf_pred_prop)
  frw_prop_names <- paste(frw_prop_names, "frw_rf_proportion", sep="")
  colnames(reg_rf_pred_prop) <- r_prop_names
  colnames(frw_rf_pred_prop) <- frw_prop_names
  
  reg_rf_pred_prob <- bag_prediction_prob_df(r_rf, test, classes)
  frw_rf_pred_prob <- bag_prediction_prob_df(frw_rf, test, classes)
  rpnames <- colnames(reg_rf_pred_prob)
  rpnames <- paste(rpnames, "_reg_rf_prob", sep="")
  frwnames <- colnames(frw_rf_pred_prob)
  frwnames <- paste(frwnames, "_frw_rf_prob", sep="")
  colnames(reg_rf_pred_prob) <- rpnames
  colnames(frw_rf_pred_prob) <- frwnames
  
  reg_rf_pred_class <- bag_prediction_df(r_rf, test)
  frw_rf_pred_class <- bag_prediction_df(frw_rf, test)
  colnames(reg_rf_pred_class) <- c("Reg_rf_Predicted_Class")
  colnames(frw_rf_pred_class) <- c("FRW_rf_Predicted_Class")
  
  
  results <- cbind(reg_bag_pred_class, frw_bag_pred_class, reg_bag_pred_prob, frw_bag_pred_prob, reg_bag_pred_prop, frw_bag_pred_prop,
                   reg_rf_pred_class, frw_rf_pred_class, reg_rf_pred_prob, frw_rf_pred_prob, reg_rf_pred_prop, frw_rf_pred_prop)
  results$true <- test[,grp]
  results$ID <- Sys.time()
  results$train_count_0 <- nrow(train[train[,grp] == 0,])
  results$train_count_1 <- nrow(train[train[,grp] == 1,])
  results$test_count_0 <- nrow(test[test[,grp] == 0,])
  results$test_count_1 <- nrow(test[test[,grp] == 1,])
  results$md <- max_depth
  results$complexity <- complexity
  results$tree_count <- trees
  results$num_fac <- num_fac
  results$lcount <- lowcount
  
  return(results)
}



heart_bag_test <- function(heart, fac, grp, trees, max_depth, complexity){
  classes <- unique(heart[,grp])
  split <- heart_subset_data(heart)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(maxdepth = max_depth, cp=complexity)
  rbag <- Reg_Bag(train, trees, fac, grp, tree_control=control)
  frwbag <- FRW_Bag(train, trees, fac, grp, tree_control=control)
  
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

heart_rf_test <- function(heart, fac, grp, trees, max_depth, complexity, num_fac){
  classes <- unique(heart[,grp])
  split <- heart_subset_data(heart)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(maxdepth = max_depth, cp=complexity)
  rbag <- Reg_RF(train, trees, fac, grp, max_fac=num_fac, tree_control=control)
  frwbag <- FRW_RF(train, trees, fac, grp, max_fac=num_fac, tree_control=control)
  
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
  results$true <- test[,grp]
  results$ID <- Sys.time()
  results$train_count_0 <- nrow(train[train[,grp] == 0,])
  results$train_count_1 <- nrow(train[train[,grp] == 1,])
  results$test_count_0 <- nrow(test[test[,grp] == 0,])
  results$test_count_1 <- nrow(test[test[,grp] == 1,])
  results$md <- max_depth
  results$complexity <- complexity
  results$tree_count <- trees
  results$num_fac <- num_fac
  
  
  return(results)
}

heart_rf_grid <- function(heart){
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
  num_fac <- c(2, 5)
  outputs <- list()
  #for(ms in min_splits){
  for(md in max_depths){
    for(cp in cps){
      for(t in trees){
        for(nf in num_fac){
          test_df <- heart_rf_test(hrt, hfac, hgrp, t, md, cp, nf)
          outputs <- c(outputs, test_df)
        }
      }
    }
  }
  return(outputs)
}

heart_bag_grid <- function(heart){
  heart$target <- 0
  heart[heart$num > 0, 'target'] <- 1
  hrt <- heart[,-14]
  
  hfac <- colnames(hrt)
  hgrp <- hfac[14]
  hfac <- hfac[-14]
  
  #min_splits <- c(5, 10, 25)
  max_depths <- c(3,8)
  cps <- c(0.01, 0.1)
  trees <- c(100)
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

heart_bag_grid_rep <- function(heart){
  heart$target <- 0
  heart[heart$num > 0, 'target'] <- 1
  hrt <- heart[,-14]
  
  hfac <- colnames(hrt)
  hgrp <- hfac[14]
  hfac <- hfac[-14]
  
  #min_splits <- c(5, 10, 25)
  max_depths <- c(3,8)
  cps <- c(0.01, 0.1)
  trees <- c(100)
  repetitions <- 5
  outputs <- list()
  #for(ms in min_splits){
  for(md in max_depths){
    for(cp in cps){
      for(t in trees){
        for(i in seq(1, repetitions)){
          test_df <- heart_bag_test(hrt, hfac, hgrp, t, md, cp)
          outputs <- c(outputs, test_df)
        }
      }
    }
  }
  #}
  return(outputs)
}

heart_rf_grid_rep <- function(heart){
  heart$target <- 0
  heart[heart$num > 0, 'target'] <- 1
  hrt <- heart[,-14]
  
  hfac <- colnames(hrt)
  hgrp <- hfac[14]
  hfac <- hfac[-14]
  
  #min_splits <- c(5, 10, 25)
  max_depths <- c(3, 8)
  cps <- c(0.01, 0.1)
  trees <- c(100)
  num_fac <- c(5, 10)
  repetitions <- 5
  outputs <- list()
  #for(ms in min_splits){
  for(md in max_depths){
    for(cp in cps){
      for(t in trees){
        for(nf in num_fac){
          for(i in seq(1, repetitions)){
            test_df <- heart_rf_test(hrt, hfac, hgrp, t, md, cp, nf)
            outputs <- c(outputs, test_df)
          }
        }
      }
    }
  }
  return(outputs)
}

heart_lp_rep_grid <- function(heart){
  heart$target <- 0
  heart[heart$num > 0, 'target'] <- 1
  hrt <- heart[,-14]
  
  hfac <- colnames(hrt)
  hgrp <- hfac[14]
  hfac <- hfac[-14]
  
  #min_splits <- c(5, 10, 25)
  max_depths <- c(3, 8)
  cps <- c(0.01, 0.1)
  trees <- c(100, 300)
  num_fac <- c(5, 10)
  repetitions <- 3
  lcounts <- c(3, 10)
  outputs <- list()
  #for(ms in min_splits){
  for(md in max_depths){
    for(cp in cps){
      for(t in trees){
        for(nf in num_fac){
          for(lc in lcounts){
            for(i in seq(1, repetitions)){
              test_df <- heart_lp_test(hrt, hfac, hgrp, t, md, cp, nf, lc)
              outputs <- c(outputs, test_df)
            }
          }
        }
      }
    }
  }
  return(outputs)
}

heart_report <- function(heart){
  
}

heart_format_output <- function(output){
  out_df <- data.frame(matrix(ncol=22, nrow=0))
  colnames(out_df) <- c("Reg_bag_Predicted_Class","FRW_bag_Predicted_Class","1_reg_bag_prob", "0_reg_bag_prob", "1_frw_bag_prob", "0_frw_bag_prob", "pred_IDreg_bag_proportion", "0reg_bag_proportion", "1reg_bag_proportion", "pred_IDfrw_bag_proportion", "0frw_bag_proportion", "1frw_bag_proportion", "Reg_rf_Predicted_Class", "FRW_rf_Predicted_Class", "1_reg_rf_prob", "0_reg_rf_prob", "1_frw_rf_prob", "0_frw_rf_prob", "pred_IDreg_rf_proportion", "0reg_rf_proportion", "1reg_rf_proportion", "pred_IDfrw_rf_proportion", "0frw_rf_proportion", "1frw_rf_proportion", "true", "ID", "train_count_0", "train_count_1", "test_count_0", "test_count_1", "md", "complexity", "tree_count", "num_fac")
  for(i in seq(0, 39, 1)){
    temp_list <- output[((i*22)+1):((i*22)+22)]
    temp_df <- data.frame(temp_list)
    out_df <- rbind(out_df, temp_df)
  }
  return(out_df)
}
heart_lp_format_output <- function(output){
  out_df <- data.frame(matrix(ncol=34, nrow=0))
  colnames(out_df) <- c("Reg_bag_Predicted_Class","FRW_bag_Predicted_Class","1_reg_bag_prob", "0_reg_bag_prob", "1_frw_bag_prob", "0_frw_bag_prob", "pred_IDreg_bag_proportion", "0reg_bag_proportion", "1reg_bag_proportion", "pred_IDfrw_bag_proportion", "0frw_bag_proportion", "1frw_bag_proportion", "Reg_rf_Predicted_Class", "FRW_rf_Predicted_Class", "1_reg_rf_prob", "0_reg_rf_prob", "1_frw_rf_prob", "0_frw_rf_prob", "pred_IDreg_rf_proportion", "0reg_rf_proportion", "1reg_rf_proportion", "pred_IDfrw_rf_proportion", "0frw_rf_proportion", "1frw_rf_proportion", "true", "ID", "train_count_0", "train_count_1", "test_count_0", "test_count_1", "md", "complexity", "tree_count", "num_fac")
  for(i in seq(0, 23, 1)){
    temp_list <- output[((i*34)+1):((i*34)+34)]
    temp_df <- data.frame(temp_list)
    out_df <- rbind(out_df, temp_df)
  }
  return(out_df)
}





hrt_report <- function(output){
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
    
    row <- c(train_0, train_1, count_0, count_1, complexity, max_depth, tree_count, num_fac, reg_auc, frw_auc,
             reg_prauc, frw_prauc, reg_prob_1_avg, frw_prob_1_avg, reg_prob_0_avg, frw_prob_0_avg, reg_prop_1_avg, frw_prop_1_avg,
             reg_prop_0_avg, frw_prop_0_avg,
             reg_0_correct, reg_1_correct, frw_0_correct, frw_1_correct, reg_1_recall, reg_0_recall, frw_1_recall, frw_0_recall,
             reg_1_precision, reg_0_precision, frw_1_precision, frw_0_precision)

    print(length(row))
    out_df[index,] <- row
    index <- index+1
  }
  colnames(out_df) <- c("train_0", "train_1", "count_0", "count_1", "complexity", "max_depth", "tree_count", "num_fac", "reg_auc", "frw_auc", 
                        "reg_prauc", "frw_prauc", "reg_prob_1_avg", "frw_prob_1_avg", "reg_prob_0_avg", "frw_prob_0_avg", "reg_prop_1_avg", "frw_prop_1_avg",
                        "reg_prop_0_avg", "frw_prop_0_avg",
                        "reg_0_correct", "reg_1_correct", "frw_0_correct", "frw_1_correct", "reg_1_recall", "reg_0_recall", "frw_1_recall", "frw_0_recall",
                        "reg_1_precision", "reg_0_precision", "frw_1_precision", "frw_0_precision")
  return(out_df)
}

hrt_lp_report <- function(output){
  IDs <- unique(output$ID)
  out_df <- as.data.frame(matrix(ncol=56, nrow=0))
  index <- 1
  for(ID in IDs){
    temp_df <- output[output$ID == ID,]
    
    reg_bag_auc <- AUC(temp_df$X1_reg_bag_prob, temp_df$true)
    frw_bag_auc <- AUC(temp_df$X1_frw_bag_prob, temp_df$true)
    reg_bag_prauc <- PRAUC(temp_df$X1_reg_bag_prob, temp_df$true)
    frw_bag_prauc <- PRAUC(temp_df$X1_frw_bag_prob, temp_df$true)
    
    reg_rf_auc <- AUC(temp_df$X1_reg_rf_prob, temp_df$true)
    frw_rf_auc <- AUC(temp_df$X1_frw_rf_prob, temp_df$true)
    reg_rf_prauc <- PRAUC(temp_df$X1_reg_rf_prob, temp_df$true)
    frw_rf_prauc <- PRAUC(temp_df$X1_frw_rf_prob, temp_df$true)
    
    count_1 <- nrow(temp_df[temp_df$true == 1,])
    count_0 <- nrow(temp_df[temp_df$true == 0,])
    train_1 <- temp_df[1,"train_count_1"]
    train_0 <- temp_df[1,"train_count_0"]
    test_1 <- temp_df[1,"test_count_1"]
    test_0 <- temp_df[1,"test_count_0"]
    complexity <- temp_df[1,"complexity"]
    tree_count <- temp_df[1,"tree_count"]
    max_depth <- temp_df[1,"md"]
    num_fac <- temp_df[1,"num_fac"]
    
    reg_bag_1_count <- nrow(temp_df[temp_df$Reg_bag_Predicted_Class == 1,])
    reg_bag_0_count <- nrow(temp_df[temp_df$Reg_bag_Predicted_Class == 0,])
    frw_bag_1_count <- nrow(temp_df[temp_df$FRW_bag_Predicted_Class == 1,])
    frw_bag_0_count <- nrow(temp_df[temp_df$FRW_bag_Predicted_Class == 0,])
    reg_bag_1_correct <- nrow(temp_df[(temp_df$true == 1)&(temp_df$Reg_bag_Predicted_Class == 1),])
    reg_bag_0_correct <- nrow(temp_df[(temp_df$true == 0) & (temp_df$Reg_bag_Predicted_Class == 0),])
    frw_bag_1_correct <- nrow(temp_df[(temp_df$true == 1) & (temp_df$FRW_bag_Predicted_Class == 1),])
    frw_bag_0_correct <- nrow(temp_df[(temp_df$true == 0) & (temp_df$FRW_bag_Predicted_Class==0),])
    
    reg_bag_prob_1_avg <- mean(temp_df[temp_df$true==1,"X1_reg_bag_prob"])
    frw_bag_prob_1_avg <- mean(temp_df[temp_df$true==1,"X1_frw_bag_prob"])
    reg_bag_prob_0_avg <- mean(temp_df[temp_df$true==0, "X1_reg_bag_prob"])
    frw_bag_prob_0_avg <- mean(temp_df[temp_df$true==0, "X1_frw_bag_prob"])
    
    reg_bag_prop_1_avg <- mean(temp_df[temp_df$true==1, "X1reg_bag_proportion"])
    frw_bag_prop_1_avg <- mean(temp_df[temp_df$true==1, "X1frw_bag_proportion"])
    reg_bag_prop_0_avg <- mean(temp_df[temp_df$true==0, "X0reg_bag_proportion"])
    frw_bag_prop_0_avg <- mean(temp_df[temp_df$true==0, "X0frw_bag_proportion"])
    
    reg_bag_1_recall <- reg_bag_1_correct/count_1
    reg_bag_0_recall <- reg_bag_0_correct/count_0
    frw_bag_1_recall <- frw_bag_1_correct/count_1
    frw_bag_0_recall <- frw_bag_0_correct/count_0
    reg_bag_1_precision <- reg_bag_1_correct/reg_bag_1_count
    reg_bag_0_precision <- reg_bag_0_correct/reg_bag_0_count
    frw_bag_1_precision <- frw_bag_1_correct/frw_bag_1_count
    frw_bag_0_precision <- frw_bag_0_correct/frw_bag_0_count
    
    
    reg_rf_1_count <- nrow(temp_df[temp_df$Reg_rf_Predicted_Class == 1,])
    reg_rf_0_count <- nrow(temp_df[temp_df$Reg_rf_Predicted_Class == 0,])
    frw_rf_1_count <- nrow(temp_df[temp_df$FRW_rf_Predicted_Class == 1,])
    frw_rf_0_count <- nrow(temp_df[temp_df$FRW_rf_Predicted_Class == 0,])
    reg_rf_1_correct <- nrow(temp_df[(temp_df$true == 1)&(temp_df$Reg_rf_Predicted_Class == 1),])
    reg_rf_0_correct <- nrow(temp_df[(temp_df$true == 0) & (temp_df$Reg_rf_Predicted_Class == 0),])
    frw_rf_1_correct <- nrow(temp_df[(temp_df$true == 1) & (temp_df$FRW_rf_Predicted_Class == 1),])
    frw_rf_0_correct <- nrow(temp_df[(temp_df$true == 0) & (temp_df$FRW_rf_Predicted_Class==0),])
    
    reg_rf_prob_1_avg <- mean(temp_df[temp_df$true==1,"X1_reg_rf_prob"])
    frw_rf_prob_1_avg <- mean(temp_df[temp_df$true==1,"X1_frw_rf_prob"])
    reg_rf_prob_0_avg <- mean(temp_df[temp_df$true==0, "X1_reg_rf_prob"])
    frw_rf_prob_0_avg <- mean(temp_df[temp_df$true==0, "X1_frw_rf_prob"])
    
    reg_rf_prop_1_avg <- mean(temp_df[temp_df$true==1, "X1reg_rf_proportion"])
    frw_rf_prop_1_avg <- mean(temp_df[temp_df$true==1, "X1frw_rf_proportion"])
    reg_rf_prop_0_avg <- mean(temp_df[temp_df$true==0, "X0reg_rf_proportion"])
    frw_rf_prop_0_avg <- mean(temp_df[temp_df$true==0, "X0frw_rf_proportion"])
    
    reg_rf_1_recall <- reg_rf_1_correct/count_1
    reg_rf_0_recall <- reg_rf_0_correct/count_0
    frw_rf_1_recall <- frw_rf_1_correct/count_1
    frw_rf_0_recall <- frw_rf_0_correct/count_0
    reg_rf_1_precision <- reg_rf_1_correct/reg_rf_1_count
    reg_rf_0_precision <- reg_rf_0_correct/reg_rf_0_count
    frw_rf_1_precision <- frw_rf_1_correct/frw_rf_1_count
    frw_rf_0_precision <- frw_rf_0_correct/frw_rf_0_count
    
    row <- c(train_0, train_1, count_0, count_1, complexity, max_depth, tree_count, num_fac, reg_bag_auc, frw_bag_auc,
             reg_bag_prauc, frw_bag_prauc, reg_rf_auc, frw_rf_auc, reg_rf_prauc, frw_rf_prauc,
             reg_bag_prob_1_avg, frw_bag_prob_1_avg, reg_bag_prob_0_avg, frw_bag_prob_0_avg, reg_bag_prop_1_avg, frw_bag_prop_1_avg,
             reg_bag_prop_0_avg, frw_bag_prop_0_avg, reg_bag_0_correct, reg_bag_1_correct, frw_bag_0_correct, frw_bag_1_correct,
             reg_rf_prob_1_avg, frw_rf_prob_1_avg, reg_rf_prob_0_avg, frw_rf_prob_0_avg, reg_rf_prop_1_avg, frw_rf_prop_1_avg,
             reg_rf_prop_0_avg, frw_rf_prop_0_avg, reg_rf_0_correct, reg_rf_1_correct, frw_rf_0_correct, frw_rf_1_correct,
             reg_bag_1_recall, reg_bag_0_recall, frw_bag_1_recall, frw_bag_0_recall, reg_bag_1_precision, reg_bag_0_precision, frw_bag_1_precision, frw_bag_0_precision,
             reg_rf_1_recall, reg_rf_0_recall, frw_rf_1_recall, frw_rf_0_recall, reg_rf_1_precision, reg_rf_0_precision, frw_rf_1_precision, frw_rf_0_precision)
    
    print(length(row))
    out_df[index,] <- row
    index <- index+1
  }
  colnames(out_df) <- row<-c("train_0","train_1","count_0","count_1","complexity","max_depth","tree_count","num_fac","reg_bag_auc","frw_bag_auc","reg_bag_prauc","frw_bag_prauc","reg_rf_auc","frw_rf_auc","reg_rf_prauc","frw_rf_prauc","reg_bag_prob_1_avg","frw_bag_prob_1_avg","reg_bag_prob_0_avg","frw_bag_prob_0_avg","reg_bag_prop_1_avg","frw_bag_prop_1_avg","reg_bag_prop_0_avg","frw_bag_prop_0_avg","reg_bag_0_correct","reg_bag_1_correct","frw_bag_0_correct","frw_bag_1_correct","reg_rf_prob_1_avg","frw_rf_prob_1_avg","reg_rf_prob_0_avg","frw_rf_prob_0_avg","reg_rf_prop_1_avg","frw_rf_prop_1_avg","reg_rf_prop_0_avg","frw_rf_prop_0_avg","reg_rf_0_correct","reg_rf_1_correct","frw_rf_0_correct","frw_rf_1_correct","reg_bag_1_recall","reg_bag_0_recall","frw_bag_1_recall","frw_bag_0_recall","reg_bag_1_precision","reg_bag_0_precision","frw_bag_1_precision","frw_bag_0_precision","reg_rf_1_recall","reg_rf_0_recall","frw_rf_1_recall","frw_rf_0_recall","reg_rf_1_precision","reg_rf_0_precision","frw_rf_1_precision","frw_rf_0_precision")
  return(out_df)
}

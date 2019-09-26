heart_subset_data <- function(heart){
  train_ind <- sample(nrow(heart), 202)
  train <- heart[train_ind,]
  test <- heart[-train_ind,]
  return(list(train, test))
}

heart_bag_test <- function(heart, hfac, hgrp, trees, min_split, max_depth, complexity){
  classes <- unique(heart[,hgrp])
  split <- heart_subset_data(heart)
  train <- split[[1]]
  test <- split[[2]]
  control <- rpart.control(minsplit=min_split, maxdepth = max_depth, cp=complexity)
  rbag <- Reg_Bag(train, trees, hfac, hgrp, tree_control=control)
  frwbag <- FRW_Bag(train, trees, hfac, hgrp, tree_control=control)
  rpredicts <- lapply(seq(1, nrow(test)), function(x) bag_prediction_probs(rbag, test[x,], classes))
  rpredicts <- as.data.frame(do.call(rbind, rpredicts))
  colnames(rpredicts) <- lapply(classes, function(x) return(paste("reg_", x, sep="")))
  frwpredicts <- lapply(seq(1, nrow(test)), function(x) bag_prediction_probs(frwbag, test[x,], classes))
  frwpredicts <- as.data.frame(do.call(rbind, frwpredicts))
  colnames(frwpredicts) <- lapply(classes, function(x) return(paste("frw_", x, sep="")))
  results <- cbind(rpredicts, frwpredicts)
  results$true <- test[,hgrp]
  results$ID <- Sys.time()
  return(results)
}

heart_bag_grid <- function(heart){
  heart$target <- 0
  heart[heart$num > 0, 'target'] <- 1
  heart <- heart[,-14]
  
  hfac <- colnames(heart)
  hgrp <- hfac[14]
  hfac <- hfac[-14]
  
  min_splits <- c(5, 10, 25)
  max_depths <- c(2,3,5)
  cps <- c(0.01, 0.05, 0.1)
  trees <- c(100, 500)
  outputs <- list()
  for(ms in min_splits){
    for(md in max_depths){
      for(cp in cps){
        for(t in trees){
          test_df <- heart_bag_test(heart, hfac, hgrp, t, ms, md, cp)
          outputs <- c(outputs, test_df)
        }
      }
    }
  }
  return(outputs)
}

heart_format_output <- function(output){
  out_df <- data.frame(matrix(ncol=6, nrow=0))
  colnames(out_df) <- c("reg_1", "reg_0", "frw_1", "frw_0", "true", "ID")
  for(i in seq(0, 53, 1)){
    print(i)
    temp_list <- output[((i*6)+1):((i*6)+6)]
    print(temp_list)
    temp_df <- data.frame(temp_list)
    out_df <- rbind(out_df, temp_df)
  }
  return(out_df)
}






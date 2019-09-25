SingleSample_BaggingTest <- function(dataset, dfac, dgrp, train_size, tree_count, tree_control = NA){
  train_samp <- sample(nrow(dataset), train_size, replace=FALSE)
  test_samp <- dataset[-train_samp,]
  train_samp <- dataset[train_samp,]
  rbag <- Reg_Bag(train_samp, tree_count, dfac, dgrp, train_size, tree_control)
  frwbag <- FRW_Bag(train_samp, tree_count, dfac, dgrp, tree_control)
  rpredicts <- lapply(seq(1, nrow(test_samp)), function(x) bag_prediction(rbag, test_samp[x,]))
  frwpredicts <- lapply(seq(1, nrow(test_samp)), function(x) bag_prediction(frwbag, test_samp[x,]))
  results <- data.frame(rbag = unlist(rpredicts), frwbag = unlist(frwpredicts), true=test_samp[,dgrp], ID = Sys.time())
  return(results)
}

SingleSample_RFTest <- function(dataset, dfac, dgrp, train_size, tree_count, tree_control=NA, max_fac=5){
  train_samp <- sample(nrow(dataset), train_size, replace=FALSE)
  test_samp <- dataset[-train_samp,]
  train_samp <- dataset[train_samp,]
  rf <- Reg_RF(train_samp, tree_count, dfac, dgrp, train_size, max_fac)
  frwrf <- FRW_RF(train_samp, tree_count, dfac, dgrp, min_fac, max_fac)
  rfpredicts <- lapply(seq(1, nrow(test_samp)), function(x) bag_prediction(rf, test_samp[x,]))
  frwrfpredicts <- lapply(seq(1, nrow(test_samp)), function(x) bag_prediction(frwrf, test_samp[x,]))
  results <- data.frame(rf = unlist(rfpredicts), frwrf = unlist(frwrfpredicts), true=test_samp[,dgrp], ID = Sys.time())
  return(results)
}

ManySample_BaggingTest <- function(dataset, dfac, dgrp, train_size, test_count, tree_count, tree_control=NA){
  #many_res <- replicate(test_count, SingleSample_BaggingTest(dataset, dfac, dgrp, train_size, tree_count))
  #as.data.frame(do.call(rbind, many_res))
  many_res <- replicate(test_count, SingleSample_BaggingTest(dataset, dfac, dgrp, train_size, tree_count, tree_control), simplify=FALSE)
  return(many_res)
}

ManySample_RFTest <- function(dataset, dfac, dgrp, train_size, test_count, tree_count, min_fac, max_fac, tree_control=NA){
  many_res <- replicate(test_count, SingleSample_RFTest(dataset, dfac, dgrp, train_size, tree_count, min_fac, max_fac, tree_control), simplify=FALSE)
  return(many_res)
}



OutputSummary2 <- function(output){
  ids <- unique(output[,4])
  classes <- unique(output[,3])
  outdf <- data.frame(matrix(ncol=length(classes)*5, nrow=length(ids)))
  row_ind = 1
  names = lapply(classes, function(x) c(
   paste(x, "_count", sep=""),
   paste(x,"_reg_precision", sep=""),
   paste(x,"_frw_precision", sep=""),
   paste(x,"_reg_recall", sep=""),
   paste(x,"_frw_recall", sep="")
  ))
  names <- unlist(names)
  colnames(outdf) <- names
  for (id in ids){
    row = list()
    for(class in classes){
      class_count <- sum(output[,3] == class & output[,4] == id)
      reg_precision <- sum(output[,4] == id & output[,1] == class & output[,3] == class)/sum(output[,4] == id & output[,1] == class)
      frw_precision <- sum(output[,4] == id & output[,2] == class & output[,3] == class)/sum(output[,4] == id & output[,2] == class)
      reg_recall <- sum(output[,4] == id & output[,1] == class & output[,3] == class)/sum(output[,4] == id & output[,3] == class)
      frw_recall <- sum(output[,4] == id & output[,2] == class & output[,3] == class)/sum(output[,4] == id & output[,3] == class)
      row = append(row, c(class_count, reg_precision, frw_precision, reg_recall, frw_recall))
    }
    outdf[row_ind,] = row
    row_ind = row_ind+1
  }
  return(outdf)
}


analyze_grid_output <- function(data){
  for(dat in data){
    
  }
}

bagging_grid_search <- function(data, dfac, dgrp, train_size, test_count, tree_counts, tree_maxdepths, tree_mincounts ){
  outputs <- list()
  for(tcount in tree_counts){
    for (tdep in tree_maxdepths){
      for (tminc in tree_mincounts){
        print(paste(tcount,tdep, tminc, sep=","))
        tcontrol = rpart.control(mincount=tminc, maxdepth=tdep)
        test <- ManySample_BaggingTest(data, dfac, dgrp, train_size, test_count, tcount, tcontrol)
        outputs <- c(outputs, test)
      }
    }
  }
  return(outputs)
}

RF_grid_search <- function(data, dfac, dgrp, train_size, test_count, tree_counts, factor_counts, tree_controls ){
  outputs <- list()
  for(tcount in tree_counts){
    for (fcount in factor_counts){
      for (tcont in tree_controls){
        print(paste("Tree Count: ", toString(tcount), "\n Factor Count: ", toString(fcount), "\n Tree Control: ", tString(tcont)))
        
      }
    }
  }
  return(outputs)
}



ccdat_bag_tests <- function(train_data, test_data, dfac, dgrp, train_size, tree_count, tree_depth, min_bin){
  outputs <- list()
  for(tdep in tree_depth){
    for(mc in min_bin){
      print(paste(tdep,mc,sep=","))
      tcontrol = rpart.control(minsplit=mc,maxdepth=tdep)
      test <- SingleSample_BaggingTest_TestData(train_data, test_data, dfac, dgrp, train_size, tree_count, tcontrol)
      outputs <- c(outputs, test)
    }
  }
  return(outputs)
}



SingleSample_BaggingTest_TestData <- function(dataset, test_data, dfac, dgrp, train_size, tree_count, tree_control = NA){
  train_samp <- sample(nrow(dataset), train_size, replace=FALSE)
  train_samp <- dataset[train_samp,]
  rbag <- Reg_Bag(train_samp, tree_count, dfac, dgrp, train_size, tree_control)
  frwbag <- FRW_Bag(train_samp, tree_count, dfac, dgrp, tree_control)
  rpredicts <- lapply(seq(1, nrow(test_data)), function(x) bag_prediction(rbag, test_data[x,]))
  frwpredicts <- lapply(seq(1, nrow(test_data)), function(x) bag_prediction(frwbag, test_data[x,]))
  results <- data.frame(rbag = unlist(rpredicts), frwbag = unlist(frwpredicts), true=test_data[,dgrp], ID = Sys.time())
  return(results)
}


ccdat_save_output <- function(output, titles){
  outdfs <- list()
  ind <- 1
  for(index in seq(from=1, to=64, by=4)){
    out_df <- as.data.frame(output[index:(index+3)])
    outdfs[[ind]] <- out_df
    write.csv(out_df, file=paste("CC_OUT_",titles[ind],".csv", sep=""))
    ind <- ind + 1
  }
  return(outdfs)
}



save_grid_search_output <- function(output){
  
}






ccdat_bag_test <- function(data){
  depths = c(2, 3, 5, 8)
  tree_counts = c(100, 500, 1000)
  min_splits = c(2, 5, 10, 25, 50)
  for(t in tree_counts){
    for(d in depths){
      for(m in min_splits){
        
      }
    }
  }
}

ccdat_rf_test <- function(train_dat, test_dat, fac, grp){
  depths = c(2, 3, 5, 8)
  tree_counts = c(500, 1000)
  min_splits = c(2, 5, 10, 25, 50)
  max_features = c(2, 4, 6, 8)
  results = data.frame(matrix(ncol=4, nrow=0))
  colnames(results) <- c("rf", "frwrf", "true", "ID")
  for(t in tree_counts){
    for(d in depths){
      for(m in min_splits){
        for(f in max_features){
          tree_control <- rpart.control(minsplit=m, maxdepth=d)
          train_samp <- train_dat[sample(nrow(train_dat), 10000),]
          rf <- Reg_RF(train_samp, t, fac, grp, 10000, f, tree_control)
          frw_rf <- FRW_RF(train_samp, t, fac, grp, f, tree_control)
          rfpredicts <- lapply(seq(1, nrow(test_samp)), function(x) bag_prediction(rf, test_samp[x,]))
          frwrfpredicts <- lapply(seq(1, nrow(test_samp)), function(x) bag_prediction(frwrf, test_samp[x,]))
          res <- data.frame(rf = unlist(rfpredicts), frwrf = unlist(frwrfpredicts), true=test_samp[,grp], ID=Sys.time())
          results <- rbind(results, res)
        }
      }
    }
  }
  return(results)
}

iris_bag_test <- function(data){
  depths = c(2, 3, 5)
  tree_counts = c(50, 100, 500)
  min_splits = c(5, 10, 20, 30)
  for(t in tree_counts){
    for(d in depths){
      for(m in min_splits){
        
      }
    }
  }
}

heart_rf_test <- function(train_dat, test_dat, fac, grp){
  depths = c(2, 3, 5)
  tree_counts = c(250, 500, 1000)
  min_splits = c(5, 10, 20, 30)
  max_feature = c(2, 3, 4, 6)
  for(t in tree_counts){
    for(d in depths){
      for(m in min_splits){
        for(f in max_features){
          tree_control <- rpart.control(minsplit=m, maxdepth=d)
          train_samp <- train_dat[sample(nrow(train_dat), 10000),]
          rf <- Reg_RF(train_samp, t, fac, grp, 10000, f, tree_control)
          frw_rf <- FRW_RF(train_samp, t, fac, grp, f, tree_control)
          rfpredicts <- lapply(seq(1, nrow(test_samp)), function(x) bag_prediction(rf, test_samp[x,]))
          frwrfpredicts <- lapply(seq(1, nrow(test_samp)), function(x) bag_prediction(frwrf, test_samp[x,]))
          res <- data.frame(rf = unlist(rfpredicts), frwrf = unlist(frwrfpredicts), true=test_samp[,grp], ID=Sys.time())
          results <- rbind(results, res)
        }
      }
    }
  }
}


cc_bag_output_depths_and_splits <- function(data){
  depths <- c(1, 2, 3, 4)
  min_splits <- c(10, 20, 50, 100)
  unique_IDs <- unique(data[,4])
  index = 1
  data$min_split = 0
  data$max_depth = 0
  for(d in depths){
    for(s in min_splits){
      data[data[,4] == unique_IDs[index],'min_split'] <- s
      data[data[,4] == unique_IDs[index], 'max_depth'] <- d
      index <- index+1
    }
  }
  return(data)
}

cc_bag_output_analysis <- function(data){
  unique_IDs <- unique(data[,4])
  out_frame <- data.frame(matrix(ncol=11, nrow=0))
  for(ID in unique_IDs){
    sub_df<-data[data[,4] == ID,]
    min_split <- unique(sub_df$min_split)
    max_depth <- unique(sub_df$max_depth)
    rbag_0 <- sum(sub_df[,1] == 0)
    rbag_1 <- sum(sub_df[,1] == 1)
    frwbag_0 <- sum(sub_df[,2] == 0)
    frwbag_1 <- sum(sub_df[,2] == 1)
    rbag_correct_0 <- sum(sub_df[sub_df[,1] == 0,1] == sub_df[sub_df[,1] == 0, 3])
    rbag_correct_1 <- sum(sub_df[sub_df[,1] == 1, 1] == sub_df[sub_df[,1] == 1, 3])
    rbag_incorrect_0 <- rbag_0 - rbag_correct_0
    rbag_incorrect_1 <- rbag_1 - rbag_correct_1
    frwbag_correct_0 <- sum(sub_df[sub_df[,2] == 0, 2] == sub_df[sub_df[,2] == 0, 3])
    frwbag_correct_1 <- sum(sub_df[sub_df[,2] == 1, 2] == sub_df[sub_df[,2] == 1, 3])
    frwbag_incorrect_0 <- frwbag_0 - frwbag_correct_0
    frwbag_incorrect_1 <- frwbag_1 - frwbag_correct_1
    out_frame <- rbind(out_frame,
        c(ID, min_split, max_depth, rbag_0, rbag_1, frwbag_0, frwbag_1, rbag_correct_0, rbag_correct_1,
          rbag_incorrect_0, rbag_incorrect_1, frwbag_correct_0, frwbag_correct_1, frwbag_incorrect_0, frwbag_incorrect_1)
      )
  }
  colnames(out_frame) <- c('ID', 'min_split', 'max_depth', 'rbag_0', 'rbag_1', 'frwbag_0', 'frwbag_1', 'rbag_0_correct', 
                           'rbag_1_correct', 'rbag_0_incorrect', 'rbag_1_incorrect', 'frw_bag_0_correct', 'frw_bag_1_correct',
                           'frw_bag_0_incorrect', 'frw_bag_1_incorrect')
  
  return(out_frame)
}


heart_bag_output_analysis <- function(data){
  
  
}

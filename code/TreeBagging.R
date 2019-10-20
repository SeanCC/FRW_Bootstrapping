library(MCMCpack)
library(rpart)
library(data.table)

bootstrap_tree <- function(dataset, factors, group, train_size,tree_control){
  samp <- bootstrap(dataset, train_size, TRUE)
  if(length(unique(samp[,group])) < 2){
    #make root tree if fewer than 2 groups are present
    root_tree <- as.character(samp[1,group])
    return(root_tree)
  }
  else{
    if(is.na(tree_control)){
      # Tree without control
      Tree <- rpart(reformulate(termlabels=factors, response=group), data=samp, method="class")
      return(Tree)
    }
    else{
      # Tree with control
      Tree <- rpart(reformulate(termlabels=factors, response=group), data=samp, method="class", control=tree_control)
      return(Tree)
    }
  }
}
frw_tree <- function(dataset, factors, group, tree_control, weight){
  if(length(unique(dataset[,group])) <2){
    root_tree <- as.character(dataset[1,group])
    return(root_tree)
  }
  else{
    if(is.na(tree_control)){
      Tree <- rpart(reformulate(termlabels=factors, response=group), data=dataset,weights=weight, method="class")
      return(Tree)
    }
    else{
      Tree <- rpart(reformulate(termlabels=factors, response=group), data=dataset,weights=weight, method="class", control=tree_control)
      return(Tree)
    }
  }
}

Reg_Bag <- function(dataset, tree_count, factors, group, train_size=0, tree_control=NA){
  if (train_size == 0){
    train_size = nrow(dataset)
  }
  rbag <- replicate(tree_count, bootstrap_tree(dataset, factors, group, train_size,tree_control), simplify=FALSE)
  return(rbag)
}

FRW_Bag <- function(dataset, tree_count, factors, group,tree_control=NA){
  weights <- rdirichlet(tree_count, alpha=rep(1, nrow(dataset)))
  frwbag <- lapply(as.list(seq(1, tree_count)), function(x) frw_tree(dataset, factors, group, tree_control, weights[x,]))
  return(frwbag)
}

Reg_RF <- function(dataset, tree_count, factors, group, train_size=0, max_fac=5, tree_control=NA){
  if(train_size==0){
    train_size = nrow(dataset)
  }
  subfac <- function(){
    facs <- sample(factors, max_fac)
    return(facs)
  }
  rfbag <- replicate(tree_count, bootstrap_tree(dataset, subfac(), group, train_size, tree_control), simplify=FALSE)
  return(rfbag)
}



FRW_RF <- function(dataset, tree_count, factors, group, max_fac=5,tree_control=NA){
  subfac <- function(){
    facs <- sample(factors, max_fac)
    return(facs)
  }
  weights <- rdirichlet(tree_count, alpha=rep(1, nrow(dataset)))
  frw_rfbag <- lapply(as.list(seq(1, tree_count)), function(x) frw_tree(dataset, subfac(), group, tree_control, weights[x,]))
  return(frw_rfbag) 
}

bag_prediction <- function(bag, datapoint){
  predictions <- unlist(lapply(seq(1, length(bag)), function(x) tree_prediction(bag[[x]], datapoint)))
  return(names(sort(table(predictions), decreasing=TRUE)[1]))
}

bag_prediction_df <- function(bag, test_data){
  predictions <- lapply(seq(1, nrow(test_data)), function(x) bag_prediction(bag, test_data[x,]))
  predictions_df <- do.call(rbind, predictions)
  return(predictions_df)
}

# Returns the class vote that each tree made
bag_prediction_long <- function(bag, datapoint){
  predictions <- unlist(lapply(seq(1, length(bag)), function(x) tree_prediction(bag[[x]], datapoint)))
  return(predictions)
}

# Probability prediction using tree proportion
bag_prediction_prop <- function(bag, datapoint, classes){
  predictions <- bag_prediction_long(bag, datapoint)
  counts <- as.data.frame(table(predictions))
  counts$Freq <- counts$Freq/length(predictions)
  for(class in classes){
    if(!(is.element(class, counts$predictions))){
      new_row <- list(class, 0)
      counts <- rbindlist(list(counts, new_row))
    }
  }
  return(counts)
}

bag_prediction_prop_df <- function(bag, test_data, classes){
  prop_preds <- lapply(seq(1, nrow(test_data)), function(x) bag_prediction_prop(bag, test_data[x,], classes))
  out_df <- do.call(rbind, prop_preds)
  colnames(out_df) <- c("Class", "Probability")
  out_df$pred_ID <- ceiling(as.numeric(rownames(out_df))/length(classes))
  out_df <- dcast(data=out_df, formula=pred_ID~Class, value.var="Probability")
  return(out_df)
}

# Probability prediction using response function
bag_prediction_prob_df <- function(bag, test_data, classes){
  prob_preds <- lapply(seq(1, nrow(test_data)), function(x) bag_prediction_prob(bag, test_data[x,], classes))
  out_df <- do.call(rbind, prob_preds)
  return(out_df)
}

bag_prediction_prob <- function(bag, datapoint, classes){
  #predictions <- lapply(seq(1, length(bag)), function(x) unlist(tree_prediction_prob(bag[[x]], datapoint, classes)))
  predictions <- lapply(seq(1, length(bag)), function(x) tree_prediction_prob(bag[[x]], datapoint, classes))
  #prediction_df <- t(data.frame(predictions))
  prediction_df <- do.call(rbind, predictions)
  #colnames(prediction_df) <- classes
  return(colMeans(prediction_df))
}

bag_prediction_probs_long <- function(bag, datapoint, classes){
  #predictions <- lapply(seq(1, length(bag)), function(x) unlist(tree_prediction_prob(bag[[x]], datapoint, classes)))
  #prediction_df <- t(data.frame(predictions))
  predictions <- lapply(seq(1, length(bag)), function(x) tree_prediction_prob(bag[[x]], datapoint, classes))
  prediction_df <- do.call(rbind, predictions)
  return(prediction_df)
}

tree_prediction <- function(tree, datapoint){
  if(is.character(tree)){
    return(tree)
  }
  else{
    prediction <- predict(tree, datapoint, type="class")
    return(levels(prediction)[prediction])
  }
}

tree_prediction_prob <- function(tree, datapoint, classes){
  pvec <- rep(0, length(classes))
  prediction_df <- as.data.frame(matrix(nrow=0, ncol=length(classes)))
  prediction_df <- rbind(prediction_df, pvec)
  colnames(prediction_df) <- classes
  if(is.character(tree)){
    #p_ind <- match(tree, classes)
    #pvec[p_ind] <- 1.0
    #return(pvec)
    prediction_df[1,tree] <- 1
  }
  else{
    prediction <- predict(tree, datapoint, type="prob")
    pclasses <- dimnames(prediction)[[2]]
    for(i in seq(1, length(pclasses))){
      #pvec[match(pclasses[i], classes)] <- prediction[i]
      prediction_df[1,pclasses[i]] <- prediction[i]
    }
    #return(pvec)
  }
  return(prediction_df)
}

bootstrap <- function(dataset, count, replacement){
  resample <- dataset[sample(nrow(dataset), count, replace=replacement),]
  return(resample)
}
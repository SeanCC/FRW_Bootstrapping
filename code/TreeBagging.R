library(MCMCpack)
library(rpart)

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

Reg_RF <- function(dataset, tree_count, factors, group, train_size, max_fac=5, tree_control=NA){
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
  return(names(sort(table(predictions))[1]))
}

bag_prediction_long <- function(bag, datapoint){
  predictions <- unlist(lapply(seq(1, length(bag)), function(x) tree_prediction(bag[[x]], datapoint)))
  return(predictions)
}

bag_prediction_probs <- function(bag, datapoint, classes){
  predictions <- lapply(seq(1, length(bag)), function(x) unlist(tree_prediction_prob(bag[[x]], datapoint, classes)))
  prediction_df <- t(data.frame(predictions))
  return(colMeans(prediction_df))
}

bag_prediction_probs_long <- function(bag, datapoint, classes){
  predictions <- lapply(seq(1, length(bag)), function(x) unlist(tree_prediction_prob(bag[[x]], datapoint, classes)))
  prediction_df <- t(data.frame(predictions))
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
  pvec <- list(rep(0, length(classes)))
  if(is.character(tree)){
    p_ind <- match(tree, classes)
    pvec[p_ind] <- 1.0
    return(pvec)
  }
  else{
    prediction <- predict(tree, datapoint, type="prob")
    pclasses <- dimnames(prediction)[[2]]
    for(i in seq(1, length(pclasses))){
      pvec[match(pclasses[i], classes)] <- prediction[1,i]
    }
    return(pvec)
  }
}

bootstrap <- function(dataset, count, replacement){
  resample <- dataset[sample(nrow(dataset), count, replace=replacement),]
  return(resample)
}
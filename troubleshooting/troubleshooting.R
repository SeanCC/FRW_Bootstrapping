testing_dat_split <- function(ccdat, train_size){
  train_ind <- sample(nrow(ccdat), train_size)
  train <- ccdat[train_ind,]
  newcc <- ccdat[-train_ind,]
  newcc_0 <- newcc[newcc[,31] == 0,]
  newcc_1 <- newcc[newcc[,31] == 1,]
  test_0 <- sample(nrow(newcc_0), 100)
  test_0 <- newcc_0[test_0,]
  test_1 <- sample(nrow(newcc_1), 100)
  test_1 <- newcc_1[test_1,]
  test <- rbind(test_0, test_1)
  return(list(train, test))
}
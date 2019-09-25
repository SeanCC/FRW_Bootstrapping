ccdat_output <- function(output){
  final_df <- data.frame(output[1],output[2],output[3],output[4])
  for(i in 1:15){
    outdf <- data.frame(output[4*i+1], output[4*i+2], output[4*i+3], output[4*i+4])
    final_df <- rbind(final_df, outdf)
  }
  return(final_df)
}


hdat_output <- function(output){
  final_df <- output[[1]]
  for(i in 2:1600){
    final_df <- rbind(final_df, output[[i]])
  }
  return(final_df)
}
#'splitter
#'divide a df into a list, with each element containing all records in between two paired times.
#'@param df the dataframe to divide
#'@param timer the first vector of times to select by
#'@param timer2 the second vector of times to select by
#'@name splitter
#'@export
splitter <- function(df,timer,timer2){
  
  list1 <- list()
  for(i in 1:length(timer)){
    list1[[i]] <- filter(df,elapsed<=timer[i] & elapsed >= timer2[i])
  }
  return(list1)
}
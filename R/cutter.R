#'Cutter
#'Divide a dataframe into parts based on two ratios.
#'@param df the dataframe to be divided
#'@param r1 the first chunk size
#'@param r2 the second chunk size
#'@name cutter
#'@export

cutter <- function(df,r1,r2){
  list1 <- list()
  list2 <- list()
  tester<- TRUE
  index <- 1
  i <- 1
  len <- nrow(df)
  while(tester){
    if((index+(r1-1))<=len){
      list1[[i]] <- df[index:(index+r1-1),]
      index <- index+r1
      if((index+(r2-1)) <= len){
        list2[[i]] <- df[index:(index+r2-1),]
        index <- index+r2
      }else{
        tester <- FALSE
        if(index<=len){
          list2[[i]] <- df[index:len,]
        }
      }
    }else{
      tester <- FALSE
      if(index<=len){
        list1[[i]] <- df[index:len,]
      }
    }
    i <- i+1
  }
  return(list(list1,list2))
}

splitter <- function(df,timer,timer2){
  
  list1 <- list()
  for(i in 1:length(timer)){
    list1[[i]] <- filter(df,elapsed<=timer[i] & elapsed >= timer2[i])
  }
  return(list1)
}
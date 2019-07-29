#'timemelt
#'just a convenient wrapper around reshape2::melt
#'@param df the dataframe to melt.
#'@name timemelt

timemelt <- function(df){
  return(reshape2::melt(df,id.vars="Time"))
}
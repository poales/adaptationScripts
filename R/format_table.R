#'format_table_old
#'im not sure this will work if it's not in the global env actually...
#'@param num index to use.
#'@name format_table_old
#'@export

format_table_old <- function(num){
  ff <<- fopcut[[1]][[num]]
  ecs <<- ecscut[[1]][[num]]
  ff <- add_phinpq(ff)
  ff <- add_phino(ff)
  ff$PhiNPQ <- ff$PhiNPQ - ff$NPQ_qL
  ff$PhiNO <- ff$PhiNO - ff$NO_qL
  ff %<>% rownames_to_column() 
  ff$rowname <- factor(ff$rowname,levels=c(1,2,3,4,5,6,7,8,9,10,11))
  ff %<>% add_column("b6f_control" = 1-(ff$NPQ_qL+ff$NO_qL))
  ffb <- select(ff,-Time,-Fm,-Fs,-Fo,-rowname)
  ecs <- select(ecs,-Time)
  ffb <- bind_cols(ffb,ecs)
  ff %>% 
    select(-Time, -Fm,-Fs,-Fo, -qL, -NPQ, -NPQt, -b6f_control)%>%
    reshape2::melt(id.vars="rowname") %>% 
    ggplot()+
    geom_bar(mapping=aes(x=rowname,y=value,fill=variable),stat="identity")+
    scale_fill_viridis_d()+
    xlab("Point")+
    ylab("Yield")+
    theme_linedraw()-> grph
  return(list(grph,ffb))
}
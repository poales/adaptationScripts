#'collator
#'compiles gas exchange, spectroscopic and fluorescence data into a set of graphs and 
#'tables for use in rmarkdown reports
#'@param ge the gas exchange data to use
#'@param ecs the spec data to use
#'@param fop the fluorescence data to use
#'@name collator
#'@export
collator <- function(ge,ecs,fop,aci=NA){
  ge %<>% rename("Time" = elapsed)
  
  if(is.na(aci)){
    #pull out the A and Ci values ####
    times <- ecs$Time
    l <- length(times)
    aci_as <- 1:l
    aci_cis <- 1:l
    
    for(i in 1:l){
      time <- times[i]
      data <- filter(ge,Time<time-5 & Time>time-20)
      #print(data)
      aci_as[i] <- mean(data$A)
      aci_cis[i] <- mean(data$Ci)
    }
    aci_table <- tibble("Ci" = aci_cis,"A" = aci_as,"Time" = times)
  } else {
    aci_table <- aci
    if("gsw" %in% colnames(aci_table)){
      aci_table <- select(aci_table, -gsw)
    }
  }
  
  
  
  ge %<>% select(-Ci,-contains("hhmmss"))
  fop %<>% select(-Fm,-Fs,-Fo,-NPQ)
  
  #plot the chosen A against the raw A/Time ####
  aci_fit <- ggplot()+
    geom_point(timemelt(select(filter(ge,ge$A<35 & ge$A>-2.3),-gsw)),mapping=aes(x=Time,y=value))+
    geom_point(timemelt(select(aci_table,-Ci)),mapping=aes(x=Time,y=value,col="chosen points"))+
    theme_linedraw()+
    theme(strip.background = element_rect(fill = "white"),
          strip.text = element_text(color = "black"))+
    ylab("A")
  
  #plot the A/Ci ####
  aciplot <- ggplot(aci_table,mapping=aes(x=Ci,y=A))+
    geom_point()+
    theme_linedraw()+
    ylab("A")+
    xlab("Ci")
  
  
  expanded_table <- ggplot()+
    geom_point(timemelt(filter(ge,ge$A<30 & ge$A>-2.3)),mapping=aes(x=Time,y=value))+
    geom_point(timemelt(ecs),mapping=aes(x=Time,y=value))+
    geom_point(timemelt(fop),mapping=aes(x=Time,y=value))+
    geom_point(timemelt(aci_table),mapping=aes(x=Time,y=value,col="red"))+
    theme_linedraw()+
    theme(strip.background = element_rect(fill = "white"),
          strip.text = element_text(color = "black"),
          legend.position = "none")+
    facet_wrap(~variable,scales="free_y")
  
  
    
  return(list(expanded_table,aci_table,aciplot,aci_fit))
  
}
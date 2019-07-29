#'fastql_replacer
#'helper function for carveFastQL
#'@param mystring the string to be processed
#'@name fastql_replacer

fastql_replacer <-  function(mystring){
  gsub(mystring,pattern = "(?i)\nset_fc\\(COM6,as\\d*?\\)\n",replacement = "") %>% 
    gsub(pattern="\nsub\\(fop_trace\\)\nwait\\(5\\)",replacement="") %>% 
    gsub(pattern="wait\\((\\d*?)\\)",replacement = "\\1") %>% 
    return()
}
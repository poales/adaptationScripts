#'carveFastQL
#'process a text file containing a fastql script
#'returns a set of wait times
#'@param loc the location of the script file.
#'@name carveFastQL
#'@export

carveFastQL <- function(loc){
  script <- readtext::readtext(loc)
  script <- script$text
  cut <- stringr::str_split(script,pattern="wait\\(600\\)")
  cut <-  cut[[1]]
  cut <-  cut[-1]
  cut %>% lapply(fastql_replacer) %>% 
    unlist() %>% 
    return()
}
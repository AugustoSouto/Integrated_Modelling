# script to rewrite management files
require(tidyverse)

replace_table <- function(input, replace, 
                          first_line, last_line) {
  # input is the readLines() from the mgt file
  # replace is the replacement table (also read with readLines)
  input[first_line:last_line] <- replace
  return(input)
}



list_of_files <- dir(pattern = ".mgt")

for (i in 1:length(list_of_files)) {
  i <- 1
  
  # read in the management file
  foo <- file(list_of_files[i], "r")
  filelines <- readLines(foo)
  close(foo)
  # read in the SOYB replacement table
  foo2 <- file("NewSOYB_operation.txt", "r")
  SOYB_repl <- readLines(foo2)
  close(foo2)
  
  if (grep("SOYB",filelines[1])==1) {
    # change the management
    # write a function to change management
    filelines_out <- replace_table(filelines, SOYB_repl, 30, 44)
    foo <- file(list_of_files[i], "w+")
    writeLines(filelines_out,file =list_of_files[i])
    close(foo)
  }
  
}
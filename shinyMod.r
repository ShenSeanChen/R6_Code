library(devtools)
library(roxygen2)

rm(list = ls())
ls()

if_so <- function(condition, do_what) {
  if(condition) {do_what}
}


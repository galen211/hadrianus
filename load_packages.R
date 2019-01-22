packages.used = c(
  "bupaR",
  "eventdataR",
  "xesreadR",
  "edeaR",
  "processmapR",
  "processmonitR",
  "dplyr",
  "stringr",
  "readr",
  "lubridate",
  "DiagrammeR")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))

# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

output <- lapply(packages.used, require, character.only = TRUE)

assertthat::assert_that(all(output)) #  all code libraries are correctly loaded
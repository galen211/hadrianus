library(readr)
library(tools)

# get license dependencies
pkg.used <- c(
  "shiny",
  "shinydashboard",
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
  "DiagrammeR"
  )

pkg.installed <- installed.packages() %>% as.data.frame()

df <- p
library(readr)
library(tools)

# get license dependencies
tags <- c(
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
  "DiagrammeR",
  )

d <- package_dependencies(packages = tags, which = c("Depends"))


dg <- makeDepGraph(tags, enhances = TRUE, availPkgs = cranJuly2014)
set.seed(1)
plot(dg, legendPosition = c(-1, -1), vertex.size = 10, cex = 0.7)
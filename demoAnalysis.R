#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(dplyr)

#Import general info
load("./EPID600_Kwon_RData/demo.general.info")
Reduce("+", cases)

#Merge demographic data
directory <- list.files("./EPID600_Kwon_RData/Demo", recursive = T, full.names = T)
year <- unlist(lapply(directory, function(x) strsplit(strsplit(x, "/")[[1]][4], "\\q")[[1]][1]))
quarter <- unlist(lapply(directory, function(x) strsplit(strsplit(strsplit(x, "/")[[1]][4], "\\q")[[1]][2], "demo")[[1]][1]))
files <- data.frame(directory, year, quarter)
demo <- data.frame()

for (i in 1:nrow(files)) {
  load(as.character(files[i,]$directory))
  data$year <- files[i,]$year
  data$quarter <- files[i,]$quarter
  demo <- merge(demo, data, all = T)
  rm(data)
  gc()
}

save(list = "demo", file = "./EPID600_Kwon_RData/all.demographic.RData")
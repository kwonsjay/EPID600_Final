#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(dplyr)
library(ggplot2)

#Merge drug data
directory <- list.files("./EPID600_Kwon_RData/Drug", recursive = T, full.names = T)
year <- unlist(lapply(directory, function(x) strsplit(strsplit(x, "/")[[1]][4], "\\q")[[1]][1]))
year <- as.integer(year)
quarter <- unlist(lapply(directory, function(x) strsplit(strsplit(strsplit(x, "/")[[1]][4], "\\q")[[1]][2], "drug")[[1]][1]))
quarter <- as.integer(quarter)
files <- data.frame(directory, year, quarter)
drug <- data.frame()

for (i in 1:nrow(files)) {
  load(as.character(files[i,]$directory))
  data$year <- files[i,]$year
  data$quarter <- files[i,]$quarter
  drug <- rbind(drug, data)
  rm(data)
  gc()
}

#Save merged dataframe for later use
save(list = "drug", file = "./EPID600_Kwon_RData/partial.drug.RData")

top <- drug %>%
  filter(!is.na(drugname)) %>%
  group_by(drugname) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)

ps <- drug %>%
  filter(!is.na(drugname)) %>%
  filter(!is.na(role_cod)) %>%
  filter(role_cod == "PS") %>%
  group_by(drugname) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)
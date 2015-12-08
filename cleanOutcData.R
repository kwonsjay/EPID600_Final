#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(dplyr)

#Create directories
dir.create("EPID600_Kwon_RData/Outc", showWarnings = F)

#Load clean directory/database survey from file
load("./EPID600_Kwon_RData/all.directories.info")

#Select database of interest
outc <- survey %>%
filter(database == "outc")

#Define legacy data
legacy <- outc %>%
filter(year < 2013)
legacy <- legacy[-c(nrow(legacy)),]

#Add trailing $ to legacy headers, overwriting original data
flag <- "clean.outc.flag" %in% list.files("./EPID600_Kwon_Data", recursive = F)
if (!flag) {
  print("No flag. Adding trailing $ to legacy headers...")
  for (directory in legacy$directory) {
    lines <- readLines(directory)
    lines[1] <- paste0(lines[1], "$")
    writeLines(lines, directory)
  }
  save(list = "flag", file = "./EPID600_Kwon_Data/clean.outc.flag")
} else {
  print("Flag observed. Skipping this step...")
}

#Read in each file and collect some basic metrics
exclude <- c("caseid")
cases <- list()
columns <- list()
failures <- list()
names <- list()
count <- 0

for (directory in outc$directory) {
  count <- count + 1
  fname <-  paste0(strsplit(directory, "/")[[1]][3], "outc.RData")
  error <- try(data <- read.table(directory, header = T, sep = "$", comment.char = "", quote = "", na.strings = c("")), silent = F)
  
  if (class(error) == "try-error") {
    failures[[length(failures) + 1]] <- c(strsplit(directory, "/")[[1]][3], error[1])
    cases[count] <- 0
    columns[count] <- 0
    names[[count]] <- "NA"
    next
  }
  
  #Standardize column names and remove/add those not needed/needed
  names(data) <- tolower(names(data))
  # names(data)[names(data) == "i_f_code"] <- "i_f_cod"
  # names(data)[names(data) == "isr"] <- "primaryid"
  # names(data)[names(data) == "case"] <- "caseid"
  # names(data)[names(data) == "gndr_cod"] <- "sex"
  data <- data[, !(names(data) %in% exclude)]
  
  #Collect general stats and QC-related information
  cases[count] <- dim(data)[1]
  columns[[count]] <- c(strsplit(directory, "/")[[1]][3], dim(data)[2])
  names[[count]] <- names(data)
  
  save(list = c("data"), file = paste0("./EPID600_Kwon_RData/Outc/", fname))
}
save(list = c("cases", "columns", "names"), file = "./EPID600_Kwon_RData/outc.general.info")

#QC
names <- lapply(names, tolower)
colnames <- unlist(names)
unique <- list()
for (name in colnames) {
  if (!(name %in% unlist(unique))) {
    unique[length(unique) + 1] <- name
  }
}
print(length(unlist(unique)) == 12)


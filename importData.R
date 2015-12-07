#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(dplyr)

#Weed out irrelevant files
files <- list.files("./EPID600_Kwon_Data", recursive = T, full.names = T)
files <- lapply(files, tolower)
files <- files[unlist(lapply(files, function(x) length(strsplit(x, "/")[[1]]) == 5))]
files <- files[unlist(lapply(files, function(x) grepl(".txt", x)))]
files <- files[unlist(lapply(files, function(x) strsplit(strsplit(x, "/")[[1]][5], "[0-9]")[[1]][1] != "stat"))]

#Screener to see if all databases have been identified
database <- unlist(lapply(files, function(x) strsplit(strsplit(x, "/")[[1]][5], "[0-9]")[[1]][1]))
directory <- unlist(files)
survey <- data.frame(directory, database)
survey %>%
group_by(database) %>%
summarise(count = n())

#Split databases by category
demo <- survey %>%
filter(database == "demo")

# drug <- survey %>%
# filter(database == "drug")
#
# indi <- survey %>%
# filter(database == "indi")
#
# outc <- survey %>%
# filter(database == "outc")
#
# reac <- survey %>%
# filter(database == "reac")
#
# rpsr <- survey %>%
# filter(database == "rpsr")
#
# ther <- survey %>%
# filter(database == "ther")

#Import data, catch errors to see what values are missing, export dataframes
dir.create("EPID600_Kwon_RData", showWarnings = F)
cases <- list()
columns <- list()
failures <- list()
names <- list()
count <- 0
for (directory in demo$directory) {
  count <- count + 1
  error <- try(data <- read.table(directory, header = TRUE, sep = "$", comment.char = "", quote = "", na.strings = c("")), silent = F)
  if (class(error) == "try-error") {
    failures[length(failures) + 1] <- error[1]
    cases[count] <- 0
    columns[count] <- 0
    next
  }
  cases[count] <- dim(data)[1]
  columns[count] <- dim(data)[2]
  names[count] <- names(data)
}
#Found that 2009Q3 data has an error where a row is prematurely terminated by newline. Manually fixed using vim. Moving on.
save(list = c("cases", "columns", "names"), file = "./EPID600_Kwon_RData/demo.info")

#All adverse events in FAERS
Reduce("+", cases)


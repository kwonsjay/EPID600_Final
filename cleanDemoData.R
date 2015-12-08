#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(dplyr)

#Country to ISO conversion function
getISO <- function(country, table) {
  result <- NA
  if (!is.na(country)) {
    iso <- table %>%
    filter(Name == toupper(country))
    if (dim(iso)[1] != 0) {
      result <- as.character(iso$Code)
    }
  }
  return(result)
}

#Create directories
dir.create("EPID600_Kwon_RData", showWarnings = F)
dir.create("EPID600_Kwon_RData/Demo", showWarnings = F)

#Weed out irrelevant files
files <- list.files("./EPID600_Kwon_Data", recursive = T, full.names = T)
files <- lapply(files, tolower)
files <- files[unlist(lapply(files, function(x) length(strsplit(x, "/")[[1]]) == 5))]
files <- files[unlist(lapply(files, function(x) grepl(".txt", x)))]
files <- files[unlist(lapply(files, function(x) strsplit(strsplit(x, "/")[[1]][5], "[0-9]")[[1]][1] != "stat"))]

#Screener to see if all databases have been identified
database <- unlist(lapply(files, function(x) strsplit(strsplit(x, "/")[[1]][5], "[0-9]")[[1]][1]))
year <- unlist(lapply(files, function(x) as.integer(strsplit(strsplit(x, "/")[[1]][3], "\\q")[[1]][1])))
quarter <- unlist(lapply(files, function(x) as.integer(strsplit(strsplit(x, "/")[[1]][3], "\\q")[[1]][2])))
directory <- unlist(files)
survey <- data.frame(directory, database, year, quarter)
survey %>%
group_by(database) %>%
summarise(count = n())

#Save clean directory/database survey to file
save(list = "survey", file = "./EPID600_Kwon_RData/all.directories.info")

#Select database of interest
demo <- survey %>%
filter(database == "demo")

#Define legacy data
legacy <- demo %>%
filter(year < 2013)
legacy <- legacy[-c(nrow(legacy)),]

#Add trailing $ to legacy headers, overwriting original data
flag <- "clean.demo.flag" %in% list.files("./EPID600_Kwon_Data", recursive = F)
if (!flag) {
  print("No flag. Adding trailing $ to legacy headers...")
  for (directory in legacy$directory) {
    lines <- readLines(directory)
    lines[1] <- paste0(lines[1], "$")
    writeLines(lines, directory)
  }
  save(list = "flag", file = "./EPID600_Kwon_Data/clean.demo.flag")
} else {
  print("Flag observed. Skipping this step...")
}

#Read in each file and collect some basic metrics
exclude <- c("death_dt", "confid", "image", "to_mfr", "foll_seq", "event_dt", "mfr_dt", "rept_cod", "auth_num", "mfr_num", "mfr_sndr", "rept_dt", "caseversion", "lit_ref", "age_grp", "occr_country", "init_fda_dt", "x")
cases <- list()
columns <- list()
failures <- list()
names <- list()
count <- 0

for (directory in demo$directory) {
  count <- count + 1
  fname <-  paste0(strsplit(directory, "/")[[1]][3], "demo.RData")
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
  names(data)[names(data) == "i_f_code"] <- "i_f_cod"
  names(data)[names(data) == "isr"] <- "primaryid"
  names(data)[names(data) == "case"] <- "caseid"
  names(data)[names(data) == "gndr_cod"] <- "sex"
  data <- data[, !(names(data) %in% exclude)]
  
  if (!("reporter_country" %in% names(data))) {
    data$reporter_country <- NA
  }
  
  #Perform country code conversion
  codes <- read.table("./iso.txt", header = T, sep = ",", quote = '\"', na.strings = c(""))
  codes$Name <- toupper(codes$Name)
  if (directory %in% legacy$directory) {
    data$reporter_country <- unlist(lapply(data$reporter_country, function(x) getISO(as.character(x), codes)))
    data$reporter_country <- as.factor(data$reporter_country)
  }
  
  #Convert inconsistent column formats to factor
  data$wt <- as.factor(data$wt)
  
  #Collect general stats and QC-related information
  cases[count] <- dim(data)[1]
  columns[[count]] <- c(strsplit(directory, "/")[[1]][3], dim(data)[2])
  names[[count]] <- names(data)
  
  save(list = c("data"), file = paste0("./EPID600_Kwon_RData/Demo/", fname))
}
#Found that 2009Q3 data has an error where a row is prematurely terminated by newline. Manually fixed using vim. Moving on. (line 53920)
save(list = c("cases", "columns", "names"), file = "./EPID600_Kwon_RData/demo.general.info")

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


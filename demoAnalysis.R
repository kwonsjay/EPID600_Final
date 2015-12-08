#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(dplyr)
library(rworldmap)
library(ggplot2)
library(maps)
library(countrycode)

#Define helper functions
convertAge <- function(age, code) {
  result <- NA
  if (code == "DEC") {
    result <- age * 10
  } else if (code == "YR") {
    result <- age
  } else if (code == "MON") {
    result <- age / 12
  } else if (code == "WK") {
    result <- age / 48
  } else if (code == "DY") {
    result <- age / 365
  } else if (code == "HR") {
    result <- age / (365 * 24)
  } else if (code == "SEC") {
    result <- age / (365 * 24 * 3600)
  }
  if (!is.na(result)) {
    return(as.integer(result))
  }
  return(result)
}

convertWt <- function(wt, code) {
  result <- NA
  if (code == "KG") {
    result <- wt
  } else if (code == "LBS") {
    result <- wt / 2.2
  } else if (code == "GMS") {
    result <- wt / 1000
  }
  if(!is.na(result)) {
    return(as.integer(result))
  }
  return(result)
}

assignWtLow <- function(wt) {
  result <- NA
  if (!(class(wt) == "numeric") & !(class(wt) == "integer")) {
    return(result)
  }
  mod.100 <- wt %% 100
  result <- as.integer(wt / 100) * 100
  if (mod.100 >= 50) {
    result <- result + 50
  }
  return(result)
}

#Import general info
load("./EPID600_Kwon_RData/demo.general.info")

#This is the number of rows the merged dataframe should have
Reduce("+", cases)

#Merge demographic data
directory <- list.files("./EPID600_Kwon_RData/Demo", recursive = T, full.names = T)
year <- unlist(lapply(directory, function(x) strsplit(strsplit(x, "/")[[1]][4], "\\q")[[1]][1]))
year <- as.integer(year)
quarter <- unlist(lapply(directory, function(x) strsplit(strsplit(strsplit(x, "/")[[1]][4], "\\q")[[1]][2], "demo")[[1]][1]))
quarter <- as.integer(quarter)
files <- data.frame(directory, year, quarter)
demo <- data.frame()

for (i in 1:nrow(files)) {
  load(as.character(files[i,]$directory))
  data$year <- files[i,]$year
  data$quarter <- files[i,]$quarter
  demo <- rbind(demo, data)
  rm(data)
  gc()
}

#Save merged dataframe for later use
save(list = "demo", file = "./EPID600_Kwon_RData/all.demographic.RData")

#Load all demographics data into workspace
#load("./EPID600_Kwon_RData/all.demographic.RData")

#Draw world map of adverse event reports
.pardefault <- par()

regions <- demo %>%
  filter(!is.na(reporter_country)) %>%
  filter(nchar(as.character(reporter_country)) == 2) %>%
  group_by(reporter_country) %>%
  summarise(count = n())

map <- map_data("world")
map$reporter_country = countrycode(map$region, origin = "country.name", destination = "iso2c")
world.map <- left_join(regions, map)
p <- ggplot(world.map, aes(x = long, y = lat, group = group, fill = count)) +
geom_polygon() + expand_limits(x = world.map$long, y = world.map$lat) +
labs(title = "Report Density by Country", x = "Longitude", y = "Latitude")
ggsave(p, file = "A1.WorldDensity.pdf", width = 7, height = 4)

p <- ggplot(world.map, aes(x = long, y = lat, group = group, fill = log(count))) +
geom_polygon() + expand_limits(x = world.map$long, y = world.map$lat) +
labs(title = "Report Log Density by Country", x = "Longitude", y = "Latitude")
ggsave(p, file = "A2.WorldDensityLog.pdf", width = 7, height = 4)

rm(list = c("map", "world.map", "regions"))
gc()

#Draw bar plot of adverse event reports
by.quarter <- demo %>%
  group_by(year, quarter) %>%
  summarise(count = n())
  
by.quarter$qname <- paste0(by.quarter$year, "Q", by.quarter$quarter)
p <- ggplot(by.quarter, aes(x = qname, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Total Reports by Quarter", x = "Quarter", y = "Count") +
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(p, file = "A3.QuarterlyReports.pdf", width = 7, height = 4)

by.year <- demo %>%
  group_by(year) %>%
  summarise(count = n())
  
p <- ggplot(by.year, aes(x = year, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Total Reports by Year", x = "Year", y = "Count")
ggsave(p, file = "A4.YearlyReports.pdf", width = 7, height = 4)

rm(list = c("by.quarter", "by.year"))
gc()

#Draw bar plot of age distribution
by.age <- demo %>%
  filter(!is.na(age)) %>%
  filter(!is.na(age_cod))

by.age$age <- as.numeric(by.age$age)

#Weed out negative age values
by.age <- by.age %>%
  filter(age >= 0)

by.age <- cbind(by.age, age_yr = mapply(convertAge, by.age$age, by.age$age_cod))

#Too many age and age_cod errors in db. Had to set an upper bound just above the oldest person in the world.
by.age <- by.age %>%
  filter(age_yr < 130)

by.age.count <- by.age %>%
  group_by(age_yr) %>%
  summarise(count = n())

p <- ggplot(by.age.count, aes(x = age_yr, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Age Distribution in Reports", x = "Age", y = "Count")
ggsave(p, file = "A5.AgeDist.pdf", width = 7, height = 4)

#Draw bar plot of gender distribution
by.gender <- demo %>%
  filter(!is.na(sex)) %>%
  filter(sex %in% c("F", "M")) %>%
  group_by(sex) %>%
  summarise(count = n())

p <- ggplot(by.gender, aes(x = sex, y = count, fill = sex)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Gender Distribution in Reports", x = "Gender", y = "Count")
ggsave(p, file = "A6.GenderDist.pdf", width = 7, height = 4)

#Draw bar plot of gender distribution in age groups
by.age.gender <- by.age %>%
  filter(!is.na(sex)) %>%
  filter(sex %in% c("F", "M")) %>%
  group_by(age_yr, sex) %>%
  summarise(count = n())

p <- ggplot(by.age.gender, aes(x = age_yr, y = count, fill = sex)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Age Distribution of Reports by Gender", x = "Age", y = "Count")
ggsave(p, file = "A7.AgeGenderDist.pdf", width = 7, height = 4)

rm(list = c("by.age", "by.age.count", "by.gender", "by.age.gender"))
gc()

#Draw bar plot of follow-ups
by.if <- demo %>%
  filter(!is.na(i_f_cod)) %>%
  filter(i_f_cod %in% c("I", "F"))

by.if.count <- by.if %>%
  group_by(i_f_cod) %>%
  summarise(count = n())

p <- ggplot(by.if.count, aes(x = i_f_cod, y = count, fill = i_f_cod)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Follow-up Distribution", x = "Follow-up Code", y = "Count")
ggsave(p, file = "A8.FollowUpDist.pdf", width = 7, height = 4)

by.if.gender <- by.if %>%
  filter(!is.na(sex)) %>%
  filter(sex %in% c("F", "M")) %>%
  group_by(i_f_cod, sex) %>%
  summarise(count = n())

p <- ggplot(by.if.gender, aes(x = i_f_cod, y = count, fill = sex)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Follow-ups by Gender", x = "Follow-up Code", y = "Count")
ggsave(p, file = "A9.FollowUpGenderDist.pdf", width = 7, height = 4)

rm(list = c("by.if", "by.if.count", "by.if.gender"))
gc()

#Draw bar plot of report provider occupation
by.job <- demo %>%
  filter(!is.na(occp_cod))

by.job.count <- by.job %>%
  group_by(occp_cod) %>%
  summarise(count = n())

by.job.gender <- by.job %>%
  filter(!is.na(sex)) %>%
  filter(sex %in% c("F", "M")) %>%
  group_by(occp_cod, sex) %>%
  summarise(count = n())

by.job.gender.pc <- by.job %>%
  filter(!is.na(sex)) %>%
  filter(sex %in% c("F", "M")) %>%
  group_by(sex, occp_cod) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 *count / sum(count))

by.job.male <- by.job.gender.pc %>%
  filter(sex == "M")

by.job.female <- by.job.gender.pc %>%
  filter(sex == "F")

p <- ggplot(by.job.count, aes(x = occp_cod, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Reporter Occupation Distribution", x = "Occupation Code", y = "Count")
ggsave(p, file = "A10.JobDist.pdf", width = 7, height = 4)

p <- ggplot(by.job.gender, aes(x = occp_cod, y = count, fill = sex)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Reporter Occupation by Gender", x = "Occupation Code", y = "Count")
ggsave(p, file = "A11.JobGenderDist.pdf", width = 7, height = 4)

p <- ggplot(by.job.male, aes(x = factor(1), y = percent, fill = occp_cod)) +
geom_bar(width = 1, stat = "identity") +
coord_polar("y", start = 0) +
labs(title = "Reporter Occupation for Males", y = "Percent")
ggsave(p, file = "A12.MaleReportJob.pdf", width = 7, height = 4)

p <- ggplot(by.job.female, aes(x = factor(1), y = percent, fill = occp_cod)) +
geom_bar(width = 1, stat = "identity") +
coord_polar("y", start = 0) +
labs(title = "Reporter Occupation for Females", y = "Percent")
ggsave(p, file = "A13.FemaleReportJob.pdf", width = 7, height = 4)

rm(list = c("by.job", "by.job.count", "by.job.gender", "by.job.gender.pc", "by.job.male", "by.job.female"))
gc()

#Draw bar graph for electronic submissions by year
by.esub <- demo %>%
  filter(!is.na(e_sub)) %>%
  filter(e_sub %in% c("N", "Y"))

by.esub.quarter <- by.esub %>%
  group_by(year, quarter, e_sub) %>%
  summarise(count = n())

by.esub.year <- by.esub %>%
  group_by(year, e_sub) %>%
  summarise(count = n())

by.esub.quarter$qname <- paste0(by.esub.quarter$year, "Q", by.esub.quarter$quarter)
p <- ggplot(by.esub.quarter, aes(x = qname, y = count, fill = e_sub)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Electronic Submissions by Quarter", x = "Quarter", y = "Count") +
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(p, file = "A14.QuarterlyEsubs.pdf", width = 7, height = 4)

p <- ggplot(by.esub.year, aes(x = year, y = count, fill = e_sub)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Electronic Submissions by Year", x = "Year", y = "Count")
ggsave(p, file = "A15.YearlyEsubs.pdf", width = 7, height = 4)

rm(list = c("by.esub", "by.esub.quarter", "by.esub.year"))
gc()

#Draw bar graphs for weight
by.wt <- demo %>%
  filter(!is.na(wt)) %>%
  filter(!is.na(wt_cod)) %>%
  filter(wt_cod %in% c("KG", "LBS", "GMS"))

by.wt$wt <- as.numeric(by.wt$wt)
by.wt <- cbind(by.wt, wt_clean = mapply(convertWt, by.wt$wt, by.wt$wt_cod))

#Too many wt and wt_cod errors in db. Had to set an upper bound just above the heaviest person in the world.
by.wt <- by.wt %>%
  filter(wt_clean < 700)
  
by.wt <- cbind(by.wt, wt_low = mapply(assignWtLow, by.wt$wt_clean))

by.wt.count <- by.wt %>%
  group_by(wt_clean) %>%
  summarise(count = n())

by.wt.low.count <- by.wt %>%
  group_by(wt_low) %>%
  summarise(count = n()) %>%
  arrange(wt_low)

by.wt.low.count$cat <- paste0(by.wt.low.count$wt_low, "-", by.wt.low.count$wt_low + 50)

p <- ggplot(by.wt.count, aes(x = wt_clean, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Weight Distribution in Reports", x = "Weight (KG)", y = "Count")
ggsave(p, file = "A16.WeightDist.pdf", width = 7, height = 4)

p <- ggplot(by.wt.low.count, aes(x = wt_low, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Grouped Weight Distribution in Reports", x = "Weight Groups (KG)", y = "Count")
ggsave(p, file = "A17.WeightCatDist.pdf", width = 7, height = 4)

rm(list = c("by.wt", "by.wt.count", "by.wt.low.count"))
gc()

#Clean data for fitting
all.fields <- demo %>%
  filter(!is.na(i_f_cod)) %>%
  filter(i_f_cod %in% c("I", "F")) %>%
  filter(!is.na(sex)) %>%
  filter(sex %in% c("F", "M")) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(age_cod)) %>%
  filter(!is.na(wt)) %>%
  filter(!is.na(wt_cod)) %>%
  filter(wt_cod %in% c("KG", "LBS", "GMS"))

all.fields$age <- as.numeric(all.fields$age)
all.fields$wt <- as.numeric(all.fields$wt)

all.fields <- all.fields %>%
  filter(age >= 0)

all.fields <- cbind(all.fields, age_yr = mapply(convertAge, all.fields$age, all.fields$age_cod))
all.fields <- cbind(all.fields, wt_clean = mapply(convertWt, all.fields$wt, all.fields$wt_cod))

all.fields <- all.fields %>%
  filter(age_yr < 130) %>%
  filter(wt_clean < 700)


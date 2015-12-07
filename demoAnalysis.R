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
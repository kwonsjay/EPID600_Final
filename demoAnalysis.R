#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(dplyr)
library(rworldmap)

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
load("./EPID600_Kwon_RData/all.demographic.RData")

#Draw world map of adverse event reports
.pardefault <- par()
regions <- demo %>%
filter(!is.na(reporter_country)) %>%
filter(nchar(as.character(reporter_country)) == 2) %>%
group_by(reporter_country) %>%
summarise(count = n())

sPDF <- joinCountryData2Map(regions, joinCode = "ISO2", nameJoinColumn = "reporter_country")
#par(mai = c(0,0,0.2,0), xaxs = "i", yaxs = "i")
mapCountryData(sPDF, nameColumnToPlot = "count", mapTitle = "Adverse Event Reports by Country")
#mapParams <- mapCountryData(sPDF, nameColumnToPlot = "count", addLegend = F)
#do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2))
dev.copy(pdf, "worldmap.pdf")
dev.off()

#Draw bar plot of adverse event reports
by.quarter <- demo %>%
group_by(year, quarter) %>%
summarise(count = n())
by.quarter$label <- paste0(by.quarter$year, "Q", by.quarter$quarter)
barplot(by.quarter$count, names.arg = by.quarter$label, main = "Total Reports by Quarter", ylab = "Count", space = 1)
dev.copy(pdf, "quarterlyreports.pdf")
dev.off()

by.year <- demo %>%
group_by(year) %>%
summarise(count = n())
barplot(by.year$count, names.arg = by.year$year, main = "Total Reports by Year", ylab = "Count")
dev.copy(pdf, "yearlyreports.pdf")
dev.off()

#Draw bar plot of age distribution
by.age <- demo %>%
filter(!is.na(age)) %>%
filter(!is.na(age_cod))

by.age$age <- as.numeric(by.age$age)

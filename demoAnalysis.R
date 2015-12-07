#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(dplyr)
library(rworldmap)

#Import general info
load("./EPID600_Kwon_RData/demo.general.info")
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

#Draw world map of adverse event reports
regions <- demo %>%
filter(!is.na(reporter_country)) %>%
filter(nchar(as.character(reporter_country)) == 2) %>%
group_by(reporter_country) %>%
summarise(count = n())

sPDF <- joinCountryData2Map(regions, joinCode = "ISO2", nameJoinColumn = "reporter_country")
par(mai = c(0,0,0.2,0), xaxs = "i", yaxs = "i")
mapCountryData(sPDF, nameColumnToPlot = "count", mapTitle = "Adverse Event Reports by Country")
#mapParams <- mapCountryData(sPDF, nameColumnToPlot = "count", addLegend = F)
#do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendMar = 2))
dev.copy(pdf, "worldmap.pdf")
dev.off()
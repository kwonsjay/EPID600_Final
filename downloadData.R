#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(XML)
library(dplyr)

#Define data sources
source1 <- "http://www.fda.gov/Drugs/GuidanceComplianceRegulatoryInformation/Surveillance/AdverseDrugEffects/ucm082193.htm"
source2 <- "http://www.fda.gov/Drugs/GuidanceComplianceRegulatoryInformation/Surveillance/AdverseDrugEffects/ucm083765.htm"

#Define function to get valid links from url
getValidLinks <- function(url) {
  html <- htmlParse(url)
  link <- unlist(xpathApply(html, "//a[@href]", xmlGetAttr, "href"))
  text <- unlist(lapply(xpathApply(html, "//a[@href]", xmlValue, "href"), toupper))
  results <- data.frame(link, text)
  results <- results %>%
  filter(grepl(".ZIP", text) & grepl("ASCII", text))
  return(results)
}

#Download data
links1 <- getValidLinks(source1)
links2 <- getValidLinks(source2)
links <- merge(links1, links2, all = T)

dir.create("EPID600_Kwon_Downloads", showWarnings = F)

for (i in 1:nrow(links)) {
  link <- paste0("http://www.fda.gov", as.character(links[i, "link"]))
  quarter <- strsplit(strsplit(as.character(links[i, "text"]), ".ZIP")[[1]][1], "_")[[1]][3]
  if (is.na(quarter)) {
    quarter <- strsplit(strsplit(as.character(links[i, "text"]), ".ZIP")[[1]][1], "\\s")[[1]][3]
  }
  dest <- paste0("EPID600_Kwon_Downloads/", quarter, ".zip")
  print(paste0(i, " Downloading: ", quarter, " data..."))
  download.file(url = link, destfile = dest, method = "curl")
}
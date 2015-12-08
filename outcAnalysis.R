#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Import libraries
library(dplyr)
library(ggplot2)

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
load("./EPID600_Kwon_RData/outc.general.info")

#This is the number of rows the merged dataframe should have
Reduce("+", cases)

#Merge patient outcome data
directory <- list.files("./EPID600_Kwon_RData/Outc", recursive = T, full.names = T)
year <- unlist(lapply(directory, function(x) strsplit(strsplit(x, "/")[[1]][4], "\\q")[[1]][1]))
year <- as.integer(year)
quarter <- unlist(lapply(directory, function(x) strsplit(strsplit(strsplit(x, "/")[[1]][4], "\\q")[[1]][2], "outc")[[1]][1]))
quarter <- as.integer(quarter)
files <- data.frame(directory, year, quarter)
outc <- data.frame()

for (i in 1:nrow(files)) {
  load(as.character(files[i,]$directory))
  data$year <- files[i,]$year
  data$quarter <- files[i,]$quarter
  outc <- rbind(outc, data)
  rm(data)
  gc()
}

#Save merged dataframe for later use
save(list = "outc", file = "./EPID600_Kwon_RData/all.outcomes.RData")

#Load all demographics data into workspace
#load("./EPID600_Kwon_RData/all.outcomes.RData")

#Draw bar plot of outcomes
by.outcome <- outc %>%
  filter(!is.na(outc_cod)) %>%
  group_by(outc_cod) %>%
  summarise(count = n())

p <- ggplot(by.outcome, aes(x = outc_cod, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Total Outcomes Distribution", x = "Outcomes", y = "Count")
ggsave(p, file = "A18.OutcDist.pdf", width = 7, height = 4)

#Deaths by quarter
by.death.quarter <- outc %>%
  filter(outc_cod == "DE") %>%
  group_by(year, quarter) %>%
  summarise(count = n())

by.death.quarter$qname <- paste0(by.death.quarter$year, "Q", by.death.quarter$quarter)

p <- ggplot(by.death.quarter, aes(x = qname, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Deaths per Quarter", x = "Quarter", y = "Count") +
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(p, file = "A19.DeathQuarter.pdf", width = 7, height = 4)

#Deaths by year
by.death.year <- outc %>%
  filter(outc_cod == "DE") %>%
  group_by(year) %>%
  summarise(count = n())

p <- ggplot(by.death.year, aes(x = year, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Deaths per Year", x = "Year", y = "Count")
ggsave(p, file = "A20.DeathYear.pdf", width = 7, height = 4)

#Hospitalizations by quarter
by.ho.quarter <- outc %>%
  filter(outc_cod == "HO") %>%
  group_by(year, quarter) %>%
  summarise(count = n())

by.ho.quarter$qname <- paste0(by.ho.quarter$year, "Q", by.ho.quarter$quarter)

p <- ggplot(by.ho.quarter, aes(x = qname, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Hospitalizations per Quarter", x = "Quarter", y = "Count") +
theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(p, file = "A21.HospQuarter.pdf", width = 7, height = 4)

#Hospitalizations by year
by.ho.year <- outc %>%
  filter(outc_cod == "HO") %>%
  group_by(year) %>%
  summarise(count = n())

p <- ggplot(by.ho.year, aes(x = year, y = count)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Hospitalizations per Year", x = "Year", y = "Count")
ggsave(p, file = "A22.HospYear.pdf", width = 7, height = 4)

#Join outcomes to demographics data
load("./EPID600_Kwon_RData/all.demographic.RData")
joined <- left_join(demo, outc, by = "primaryid")
joined <- joined %>%
filter(!is.na(outc_cod)) %>%
filter(!is.na(i_f_cod)) %>%
filter(i_f_cod %in% c("I", "F")) %>%
filter(!is.na(sex)) %>%
filter(sex %in% c("F", "M")) %>%
filter(!is.na(age)) %>%
filter(!is.na(age_cod)) %>%
filter(!is.na(wt)) %>%
filter(!is.na(wt_cod)) %>%
filter(wt_cod %in% c("KG", "LBS", "GMS"))

joined <- joined[, !(names(joined) %in% c("reporter_country", "caseid", "fda_dt", "year.y", "quarter.y"))]

joined$age <- as.numeric(joined$age)
joined$wt <- as.numeric(joined$wt)

joined <- joined %>%
  filter(age >= 0)

joined <- cbind(joined, age_yr = mapply(convertAge, joined$age, joined$age_cod))
joined <- cbind(joined, wt_clean = mapply(convertWt, joined$wt, joined$wt_cod))

joined <- joined %>%
  filter(age_yr < 130) %>%
  filter(wt_clean < 700)

joined$death <- sapply(joined$outc_cod, function(x) ifelse(x == "DE", 1, 0))
joined$i_f_cod <- sapply(joined$i_f_cod, function(x) ifelse(x == "F", 1, 0))

save(list = "joined", file = "./EPID600_Kwon_RData/join.demo.outc.clean.RData")

#Observe some correlations
chisq.test(table(joined$death, joined$age_yr))
chisq.test(table(joined$death, joined$wt_clean))
chisq.test(table(joined$death, joined$i_f_cod))

#Death by gender
by.death.gender <- joined %>%
  group_by(death, sex) %>%
  summarise(count = n())

p <- ggplot(by.death.gender, aes(x = death, y = count, fill = sex)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Death by Gender", x = "State", y = "Count")
ggsave(p, file = "A23.DeathGender.pdf", width = 7, height = 4)

#Death by age
by.death.age <- joined %>%
  group_by(age_yr, death) %>%
  summarise(count = n())

p <- ggplot(by.death.age, aes(x = age_yr, y = count, fill = death)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Death by Age", x = "Age", y = "Count")
ggsave(p, file = "A24.DeathAge.pdf", width = 7, height = 4)

#Death by weight
by.death.wt <- joined %>%
  group_by(wt_clean, death) %>%
  summarise(count = n())

p <- ggplot(by.death.wt, aes(x = wt_clean, y = count, fill = death)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Death by Weight", x = "Weight (KG)", y = "Count")
ggsave(p, file = "A25.DeathWt.pdf", width = 7, height = 4)

#Death by weight group
joined <- cbind(joined, wt_low = mapply(assignWtLow, joined$wt_clean))

by.death.wt.cat <- joined %>%
  group_by(wt_low, death) %>%
  summarise(count = n()) %>%
  arrange(wt_low)

p <- ggplot(by.death.wt.cat, aes(x = wt_low, y = count, fill = death)) +
geom_bar(position = position_dodge(), stat = "identity") +
labs(title = "Death by Weight Category", x = "Weight (KG)", y = "Count")
ggsave(p, file = "A26.DeathWtCat.pdf", width = 7, height = 4)
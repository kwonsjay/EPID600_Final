#EPID600 Final Project
#Jay (Soon Jae) Kwon

#Define directories
downloads <- "./EPID600_Kwon_Downloads/"
extract <- "./EPID600_Kwon_Data/"

#Unzip downloaded files into data directory
dir.create(extract, showWarnings = F)
files <- list.files(downloads)

for (i in 1:length(files)) {
  zip <- paste0(downloads, files[i])
  dest <- paste0(extract, strsplit(files[i], ".zip")[[1]][1])
  print(paste0(i, " Unzipping: ", files[i]))
  unzip(zip, exdir = dest)
}
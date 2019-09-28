# EPID600 Final Project
## Jay (Soon Jae) Kwon
### Mining the FDA Adverse Events Database for Trends
The goal of this project was to perform some exploratory analysis of the FDA Adverse Events Reporting System (FAERS), and to mine trends in the data. Due to the limitations of obtaining data from OpenFDA, I used text data of quarterly reports from FDA Adverse Events Reporting System (FAERS) as well as from Legacy Adverse Events Reporting System (LAERS), downloaded as individual zip files from the FDA website.  


### Directory Information
`EPID600_Kwon_Downloads` contains the raw downloaded files from the FDA website, formatted to indicate which quarter the data is for.  

`EPID600_Kwon_Data` contains the unzipped directories for each archive file in the downloads folder, and some "flag" files to indicate whether or not certain datasets have been cleaned.  

`EPID600_Kwon_RData` contains several directories and files. The directories indicate what type of information has been cleaned and organized into individual data frames. The files contain general information on the information their titles specify, and all cleaned, merged, and joined tables are present as files at the top level.  

`image_assets` contains all images that were used in the final presentation.  

`presentation` contains the final presentation in .pptx format.  

`report` contains the final report in .Rmd and .html formats.  

`sample_demographic_data` contains a single sample of demographic data taken from a single quarter.  

### File Information
`iso.txt` contains the conversion table for country names to ISO2 format.  

`downloadData.R` contains code to sequentially download all available FAERS/LAERS data from the FDA website.  

`unzipData.R` contains code to process, format, and unzip the downloaded archive files.  

`cleanDemoData.R` contains code to process and clean the unzipped data, specifically for demographic data.  

`cleanOutcData.R` contains code to process and clean the unzipped data, specifically for patient outcomes data.  

`cleanDrugData.R` contains code to process and clean the unzipped data, specifically for drug information data.  

`demoAnalysis.R` contains all snippets of code used in the analysis of demographic data. Not intended to be run all at once.  

`outcAnalysis.R` contains all snippets of code used in the analysis of patient outcomes data. Not intended to be run all at once.  

`drugAnalysis.R` contains a short snippet of code used in the *partial* analysis of demographic data.  

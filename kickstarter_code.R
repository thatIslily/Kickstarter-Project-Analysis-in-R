# Requirements
# 1. create folder called "raw_data"
# 2. save all kickstarter csv extracts downloaded from "https://webrobots.io/kickstarter-datasets/" in the raw_data folder
# 2. in line 6 change your wd to the folder path

# Merge all CSVs into master file
setwd("~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Project - Kickstarter Analysis/raw_data/")

filenames = list.files(full.names=TRUE)
df = do.call("rbind", lapply(filenames, read.csv, header=TRUE))
write.csv(df, file="~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Project - Kickstarter Analysis/df.csv",
          row.names=FALSE, 
          na='')
rm(list=ls()); gc()
library(tidyverse)

### read mortality data
files<-paste("./data/", list.files(path = "./data/"), sep = "")
files<-files[!grepl(".zip", files)]

mort_files<-files[grepl("mort", files)]

mort_data<-list()

### coerce all to character
### convert later
### file structure varies year-to-year

for(i in 1:length(mort_files)){
  mort_data[[i]]<-read_csv(mort_files[i],
                           col_types = cols(.default = col_character()),
                           trim_ws = F)
}
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

### check file structure across data

structure<-lapply(mort_data, names)
structure<-unlist(structure)
table(structure)

### from: https://data.nber.org/mortality/1959-1967/mor59_67.pdf
### for 1959-67: E984: ucod=="984-"
### files 1-9; pol_death = ucod=="984-"
### year = datayear
### age = ager27; (1 <1mo, 2: 1mo-1yr, 3 1yr, 4 2yr, 5 3yr, 6 yr, 7 5-9,
### ... 26 over 100, 27 not stated)
### sex = sex (1: male, 2: female)
### race = racer6; (1: White, 2 Black, 3: Indian, 4: Chinese, 5: Japanese, 6: All others, 7: not stated (NJ))
### state = stateoc (I'll write a function for this)


### process years 1959 - 1967
state_year_data<-list()
for(i in 1:9){
  temp<-mort_data[[i]]
  temp<-temp %>% 
    rename(year = datayear) %>% 
    mutate(year = as.numeric(year))
  temp<-temp %>% 
    mutate(police_mort = ucod=="984-")
  temp<-temp %>% 
    group_by(year, ager27, sex, race, stateoc) %>% 
    summarise(police_mort = sum(police_mort),
              tot_mort = n()) %>% 
    ungroup()
  state_year_data[[i]]<-temp
}

state_year_data<-bind_rows(state_year_data)

### from: https://data.nber.org/mortality/www.cdc.gov/nchs/data/dvs/dt78icd8.pdf
### and ICD 7 - 10 codes from http://harvardpublichealthreview.org/wp-content/uploads/2015/01/HPHRv3-Krieger-Trends-in-Deaths-Copy.pdf
### for 1968-78: E970-E977: ucr281 recode: 33600
### files 1-9; pol_death = ucod=="984-"
### year = datayear
### age = ager27; (1 <1mo, 2: 1mo-1yr, 3 1yr, 4 2yr, 5 3yr, 6 yr, 7 5-9,
### ... 26 over 100, 27 not stated)
### sex = sex (1: male, 2: female)
### race = racer; (see codebook page 27)
### state = stateoc (I'll write a function for this)

### datayear lacks all but last digit, manually code it
year_base<-1967
for(i in 10:20){
  year_base<-year_base+1
  temp<-mort_data[[i]]
  temp<-temp %>% 
    mutate(year = year_base)
  temp<-temp %>% 
    mutate(police_mort = ucr281=="33600")
  temp<-temp %>% 
    group_by(year, ager27, sex, race, stateoc) %>% 
    summarise(police_mort = sum(police_mort),
              tot_mort = n()) %>% 
    ungroup()
  
  state_year_data<-state_year_data %>% 
    bind_rows(temp)
}


### from: https://data.nber.org/mortality/www.cdc.gov/nchs/data/dvs/dt78icd8.pdf
### and ICD 7 - 10 codes from http://harvardpublichealthreview.org/wp-content/uploads/2015/01/HPHRv3-Krieger-Trends-in-Deaths-Copy.pdf
### for 1979-1998: E970-E977: ucr282 recode: 35100
### files 1-9; pol_death = ucod=="984-"
### year = datayear
### age = ager27; (1 <1mo, 2: 1mo-1yr, 3 1yr, 4 2yr, 5 3yr, 6 yr, 7 5-9,
### ... 26 over 100, 27 not stated)
### sex = sex (1: male, 2: female)
### race = racer; (see codebook page 27)
### state = stateoc (I'll write a function for this)

### datayear lacks all but last digit, manually code it
for(i in 21:37){
  temp<-mort_data[[i]]
  temp<-temp %>% 
    mutate(year = as.numeric(datayear) + 1900)
  temp<-temp %>% 
    mutate(police_mort = ucod %in% c("970", "971", "972", "973", "974", "975",
                                     "976", "977"))
  temp<-temp %>% 
    group_by(year, ager27, sex, race, stateoc) %>% 
    summarise(police_mort = sum(police_mort),
              tot_mort = n()) %>% 
    ungroup()
  
  state_year_data<-state_year_data %>% 
    bind_rows(temp)
}

### for 1996 - 1998, includes full year as year
for(i in 38:40){
  temp<-mort_data[[i]]
  temp<-temp %>% 
    mutate(police_mort = ucod %in% c("970", "971", "972", "973", "974", "975",
                                     "976", "977")) %>% 
    mutate(year = as.numeric(year))
  
  temp<-temp %>% 
    group_by(year, ager27, sex, race, stateoc) %>% 
    summarise(police_mort = sum(police_mort),
              tot_mort = n()) %>% 
    ungroup()
  
  state_year_data<-state_year_data %>% 
    bind_rows(temp)
}

### for 1999 - uses ICD-10
### uco in Y35.0-Y35.4, Y35.6-Y35.7

for(i in 41:46){
  temp<-mort_data[[i]]
  temp<-temp %>% 
    mutate(police_mort = ucod %in% c("Y350", "Y351", "Y352", "Y353",
                                     "Y354", "Y356", "Y357")) %>% 
    mutate(year = as.numeric(year))
  
  temp<-temp %>% 
    group_by(year, ager27, sex, race, stateoc) %>% 
    summarise(police_mort = sum(police_mort),
              tot_mort = n()) %>% 
    ungroup()
  
  state_year_data<-state_year_data %>% 
    bind_rows(temp)
}

### check validity of time series
### on year in the 70s is a 50% sample
valid_check<-state_year_data %>% 
  group_by(year) %>% 
  summarise(police_mort = sum(police_mor),
            tot_mort = sum(tot_mort))

### NEED TO HARMONIZE RACE/STATE/SEX. 
### 1972 is a 50% sample
### Otherwise, looks good!

write_csv(state_year_data, "./data/state_year_data.csv")
q(save = "no")
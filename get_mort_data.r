### download and unzip multiple cause of death data from NBER
### https://data.nber.org/data/vital-statistics-mortality-data-multiple-cause-of-death.html
### download 
setwd("./data")

years<-1959:2004
for(i in years){
  url<-paste(
    "https://data.nber.org/mortality/",
    i,
    "/mort",
    i,
    ".csv.zip",
    sep=""
  )
  
  download.file(url,
                destfile = 
                  paste(
                    "mort",
                    i,
                    ".csv.zip",
                    sep=""
                  ))
  
  unzip(zipfile = 
          paste(
            "mort",
            i,
            ".csv.zip",
            sep=""
          ))
}

### grab seer population 19-age 3 race 1969- data

download.file(url = "https://data.nber.org/seer-pop/uswbo19agesadj.csv.zip",
              destfile = "uswbo19agesadj.csv.zip")

unzip(zipfile = "uswbo19agesadj.csv.zip")

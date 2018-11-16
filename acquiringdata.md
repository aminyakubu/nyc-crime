NYC Crime Data
================

``` r
library(tidyverse)
```

The dataset as assessed on 11th November, 2018 [here](https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i)

Since we are interested in looking at incidence i.e. when the crime happen, we will filter out variables without exact date of occurrence. There were 655 out of 6,036,805 observations without exact date of crime occurrence. Also we are interested in 2017, 2016, 2015, 2014, 2013.

``` r
crime = read_csv("data/NYPD_Complaint_Data_Historic.csv", col_names = TRUE) 

# This code was used to assess the observations with missing date of crime occurrence
crime_missing_cp = crime %>% filter(is.na(CMPLNT_FR_DT))

#Cleaning and subsetting the data

nyc_felony_misdemeanor = crime %>% filter(!is.na(CMPLNT_FR_DT)) %>%
  separate(CMPLNT_FR_DT, into = c("day", "month", "year"), sep = "\\/") %>% 
  filter(year %in% c("2017", "2016", "2015", "2014", "2013")) %>% 
  janitor::clean_names() %>% 
  filter(law_cat_cd == "FELONY" | law_cat_cd == "MISDEMEANOR")

saveRDS(nyc_felony_misdemeanor, file = "datafiles/nyc_felony_misdemeanor.rds")
```

To use our data, use the following code

``` r
readRDS(file = "datafiles/nyc_felony_misdemeanor.rds")
```

You can use the code below to assess the complete data

``` r
## Install the required package with:
## install.packages("RSocrata")

library("RSocrata")

df <- read.socrata(
  "https://data.cityofnewyork.us/resource/9s4h-37hy.json",
  app_token = "xxx",
  email     = "xxx",
  password  = "xxx" )
```

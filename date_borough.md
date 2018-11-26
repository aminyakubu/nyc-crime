date\_and\_borough
================
Jacky Choi
November 18, 2018

This is one of the exploratory analyses for our P8105 - Data Science group project. In this exploratory analysis, we attempt to look at whether the length of time of the reported event differs by borough.

First, I load the tidyverse and patchwork package.

    ## -- Attaching packages --------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## Skipping install of 'patchwork' from a github remote, the SHA1 (fd7958ba) has not changed since last install.
    ##   Use `force = TRUE` to force installation

Next, I read in the CSV file and tidy it up. I limit the data by randomly sampling 1,000 observations from the data frame.

``` r
crime_df = readRDS(file = "datasets/nyc_felony_misdemeanor.rds")

crime_data = crime_df %>% 
  janitor::clean_names() %>%
  sample_n(1000) %>% 
  mutate(time_diff = (difftime(cmplnt_to_dt, cmplnt_fr_dt))) %>% 
  mutate(time_diff2 = (as.numeric(cmplnt_to_dt - cmplnt_fr_dt, units = "days", na.rm = TRUE))) %>% 
  select(cmplnt_fr_dt, cmplnt_to_dt, law_cat_cd, time_diff, time_diff2, boro_nm)

head(crime_data)
```

    ##   cmplnt_fr_dt cmplnt_to_dt  law_cat_cd time_diff time_diff2   boro_nm
    ## 1   2014-03-15   2014-03-15 misdemeanor    0 secs          0 manhattan
    ## 2   2017-11-27   2017-11-27 misdemeanor    0 secs          0  brooklyn
    ## 3   2015-07-09   2015-07-09      felony    0 secs          0    queens
    ## 4   2014-03-04   2014-03-04 misdemeanor    0 secs          0     bronx
    ## 5   2016-03-09   2016-03-09 misdemeanor    0 secs          0  brooklyn
    ## 6   2014-09-19         <NA> misdemeanor   NA secs         NA manhattan

I can look at the data in a table to examine general trends and get a feel of my data.

``` r
crime_table = 
  crime_data %>% 
  knitr::kable(digits = 2)

head(crime_table, 12)
```

    ##  [1] "cmplnt_fr_dt   cmplnt_to_dt   law_cat_cd    time_diff        time_diff2  boro_nm       "
    ##  [2] "-------------  -------------  ------------  --------------  -----------  --------------"
    ##  [3] "2014-03-15     2014-03-15     misdemeanor   0 secs                 0.00  manhattan     "
    ##  [4] "2017-11-27     2017-11-27     misdemeanor   0 secs                 0.00  brooklyn      "
    ##  [5] "2015-07-09     2015-07-09     felony        0 secs                 0.00  queens        "
    ##  [6] "2014-03-04     2014-03-04     misdemeanor   0 secs                 0.00  bronx         "
    ##  [7] "2016-03-09     2016-03-09     misdemeanor   0 secs                 0.00  brooklyn      "
    ##  [8] "2014-09-19     NA             misdemeanor   NA                       NA  manhattan     "
    ##  [9] "2016-02-04     2016-02-04     misdemeanor   0 secs                 0.00  brooklyn      "
    ## [10] "2015-03-20     2015-03-20     misdemeanor   0 secs                 0.00  bronx         "
    ## [11] "2017-03-22     2017-03-22     misdemeanor   0 secs                 0.00  manhattan     "
    ## [12] "2016-08-18     2016-08-18     misdemeanor   0 secs                 0.00  bronx         "

I can also look at whether the length of time for the reported event/crime differs by borough visually.

``` r
crime_data2 <- crime_data[complete.cases(crime_data),]

crime_graph2 = 
  crime_data2 %>% 
  group_by(boro_nm, law_cat_cd) %>% 
  ggplot(aes(x = boro_nm, y = time_diff2, color = law_cat_cd)) + 
  geom_point() +
    labs(
    title = "Spread of Times of Reported Event, By Borough",
    x = "Borough",
    y = "Length of Reported Event",
    caption = "Data from the crime_data file"
    ) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom")

crime_graph2
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](date_borough_files/figure-markdown_github/length_of_time_of_reported_event-1.png) From the graph above, we can see a couple of things. First, there are, numerically speaking, fewer reported events in Staten Island. Secondly, the spread of the reported events seems to be slightly larger in the Bronx than in other boroughs. Lastly, although the spread among Brooklyn, Manhattan, and Queens is approximately the same, Queens has more outliers.

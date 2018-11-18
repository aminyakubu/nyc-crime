date\_and\_borough
================
Jacky Choi
November 18, 2018

This is one of the exploratory analyses for our P8105 - Data Science group project.

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

Next, I read in the CSV file and tidy it up. I limit the data to just individuals in the borough of Queens and randomly sample 500 observations from the data frame.

``` r
crime_df = readRDS(file = "datasets/nyc_felony_misdemeanor.rds")

crime_data = crime_df %>% 
  janitor::clean_names() %>%
  sample_n(500) %>% 
  filter(boro_nm == "queens") %>% 
  separate(cmplnt_fr_dt, into = c("year_of_complaint", "month_complaint", "day_complaint"), sep = "-") %>% 
  select(year_of_complaint, law_cat_cd, boro_nm)

head(crime_data)
```

    ##   year_of_complaint  law_cat_cd boro_nm
    ## 1              2014      felony  queens
    ## 2              2014 misdemeanor  queens
    ## 3              2017      felony  queens
    ## 4              2014 misdemeanor  queens
    ## 5              2015      felony  queens
    ## 6              2017      felony  queens

I can look at the data in a table to examine general trends and get a feel of my data.

``` r
crime_table = 
  crime_data %>% 
  knitr::kable(digits = 2)

head(crime_table, 12)
```

    ##  [1] "year_of_complaint   law_cat_cd    boro_nm "
    ##  [2] "------------------  ------------  --------"
    ##  [3] "2014                felony        queens  "
    ##  [4] "2014                misdemeanor   queens  "
    ##  [5] "2017                felony        queens  "
    ##  [6] "2014                misdemeanor   queens  "
    ##  [7] "2015                felony        queens  "
    ##  [8] "2017                felony        queens  "
    ##  [9] "2015                misdemeanor   queens  "
    ## [10] "2016                misdemeanor   queens  "
    ## [11] "2015                misdemeanor   queens  "
    ## [12] "2016                misdemeanor   queens  "

It is hard to visualize any trends using solely the data table, so I now attempt to graph my data. I want to graph the year of the complaint against the category of offense in Queens. That is, I want to see if any particular year in Queens was associated with a higher category of offense.

``` r
crime_graph = 
  crime_data %>% 
  ggplot(aes(x = year_of_complaint, y = law_cat_cd)) + 
  geom_point() +
    labs(
    title = "Categories of Offenses, By Year of Complaint, In Queens",
    x = "Year",
    y = "Category",
    caption = "Data from the crime_data file"
    ) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom")

crime_graph
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](date_and_borough_files/figure-markdown_github/graph_crime_data-1.png)

I can also look at whether the length of time for the reported event/crime differs by borough.

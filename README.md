Minnesota COVID Report
================

Report last run: 2023-03-08 22:09:03

## Introduction

This is an example report that uses COVID-19 data from the New York
Times to illustrate the use of automation processes.

First, we load some necessary libraries, define some key variables, then
read in the data:

``` r
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(forcats)
library(knitr)

LAG_DAYS <- 7
POP_DENOM <- 100000

## County populations (read from a local data file in this repo)
pops <- read_csv("countypop_us.csv")

## COVID-19 case counts from the NYTimes (read from the web; updated daily)
county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2023.csv")

rate_data <- county_data %>%
  select(date, state, county, cases) %>%
  mutate(date = ymd(date)) %>%
  left_join(pops, by = c("state", "county")) %>%
  group_by(state, county) %>%
  mutate(cases_lag = lag(cases, LAG_DAYS),
         totalcases_last = cases - cases_lag) %>%
  ungroup() %>%
  mutate(rate_last = totalcases_last / pop * POP_DENOM)
```

### Minnesota

Here is a plot of COVID-19 rates since Jan. 1, 2023 in the 10 most
populous Minnesota counties:

``` r
## Identify the top 10 most populous counties
top10_pop <- pops %>% filter(state == "Minnesota") %>%
  arrange(desc(pop)) %>%
  slice(1:10) %>%
  mutate(county = factor(county))

## Make the plot
rate_data %>%
    filter(state == "Minnesota", 
         county %in% top10_pop$county,
         date > Sys.Date() - 30) %>%
  ggplot(aes(x = date, y = rate_last, color = county)) +
  geom_line(linewidth = 2) +
  xlab(NULL) +
  ylab("7-day COVID-19 case total per 100,000 population") +
  scale_color_discrete(name = "") +
  theme_minimal() +
  ggtitle("COVID-19 rates for the ten most populous Minnesota counties", 
          subtitle = paste("Latest data:", max(rate_data$date)))
```

![](README_files/figure-commonmark/unnamed-chunk-2-1.png)

### United States

The following plot shows the distribution of current COVID-19 rates by
county for each state in the United States. The x-axis is truncated at a
7-day rate of 500 per 100,000 people for improved readability.

``` r
rate_data %>%
  filter(date == max(date),
         !is.na(rate_last)) %>%
  mutate(state = fct_reorder(factor(state), -rate_last, median, na.rm = TRUE)) %>%
  ggplot(aes(x = rate_last, y = state)) +
  geom_boxplot() +
  xlim(c(0,500)) +
  xlab("7-day COVID-19 total cases per 100,000 people") +
  ylab(NULL) +
  theme_minimal() +
  ggtitle("Distribution of county-level COVID-19 case rates, by state",
          subtitle = paste("Latest data:", max(rate_data$date)))
```

![](README_files/figure-commonmark/unnamed-chunk-3-1.png)

Here is a table of the 20 counties with the highest 7-day per 100,000
COVID-19 case rates:

``` r
rate_data %>%
  filter(date == max(date),
         !is.na(rate_last)) %>%
  arrange(desc(rate_last)) %>%
  select(county, state, pop, rate_last) %>%
  rename(covid_rate = rate_last) %>%
  mutate(covid_rate = round(covid_rate)) %>%
  slice(1:20) %>%
  knitr::kable()
```

| county             | state        |     pop | covid_rate |
|:-------------------|:-------------|--------:|-----------:|
| Loving             | Texas        |     169 |       2367 |
| Clark              | Kansas       |    1994 |        802 |
| Rawlins            | Kansas       |    2530 |        593 |
| Rolette            | North Dakota |   14176 |        578 |
| Union              | Kentucky     |   14381 |        570 |
| Traverse           | Minnesota    |    3259 |        552 |
| Miami-Dade         | Florida      | 2716940 |        508 |
| Bethel Census Area | Alaska       |   18386 |        506 |
| Jackson            | Florida      |   46414 |        489 |
| Gadsden            | Florida      |   45660 |        466 |
| Hamilton           | Nebraska     |    9324 |        461 |
| Concho             | Texas        |    2726 |        440 |
| Menominee          | Wisconsin    |    4556 |        439 |
| Oglala Lakota      | South Dakota |   14177 |        423 |
| Petroleum          | Montana      |     487 |        411 |
| Sargent            | North Dakota |    3898 |        410 |
| Lemhi              | Idaho        |    8027 |        399 |
| Pawnee             | Kansas       |    6414 |        390 |
| Todd               | South Dakota |   10177 |        383 |
| Suwannee           | Florida      |   44417 |        374 |

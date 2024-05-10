# Adopted from https://www.r-bloggers.com/tidying-the-new-johns-hopkins-covid-19-time-series-datasets/

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(rvest)
  library(stringdist)
  library(data.table)
  library(ggplot2)
  library(gghighlight)
  library(plm)
})


# Function to read the raw CSV files. The files are aggregated to the country
# level and then converted to long format

clean_jhd_to_long <- function(df) {
  df_str <- deparse(substitute(df))
  var_str <- substr(df_str, 1, str_length(df_str) - 4)
  
  df %>% group_by(`Country/Region`) %>%
    filter(`Country/Region` != "Cruise Ship") %>%
    select(-`Province/State`, -Lat, -Long) %>%
    mutate_at(vars(-group_cols()), sum) %>% 
    distinct() %>%
    ungroup() %>%
    rename(country = `Country/Region`) %>%
    pivot_longer(
      -country, 
      names_to = "date_str", 
      values_to = var_str
    ) %>%
    mutate(date = mdy(date_str)) %>%
    select(country, date, !! sym(var_str)) 
}

confirmed_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")


jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
  full_join(clean_jhd_to_long(deaths_raw))
jh_covid19_data <- pdata.frame(jh_covid19_data, index=c("country", "date"), row.names = F)
jh_covid19_data$confirmed_inc <- diff(jh_covid19_data$confirmed, 1)
jh_covid19_data$deaths_inc <- diff(jh_covid19_data$deaths, 1)
jh_covid19_data <- as.data.table(jh_covid19_data)

jh_covid19_data[, confirmed_inc_avg := frollmean(confirmed_inc, 3, na.rm=T), by=.(country)]
jh_covid19_data[, deaths_inc_avg := frollmean(deaths_inc, 3, na.rm=T), by=.(country)]

# Data process
# Days since 100 confirmed cases
jh_covid19_data[confirmed >= 100, DS100 := 0:(.N-1), by=.(country)]
country.sel <- c("US", "China", "Italy", "United Kingdom", "France", "Spain")

jh_covid19_data <- jh_covid19_data[country %in% country.sel & DS100 <= as.numeric(Sys.Date()-as.Date("2020-02-20"))]



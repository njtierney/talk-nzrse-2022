covid_url <- "https://covidlive.com.au/report/daily-vaccinations/aus"

library(polite)
library(tidyverse)
library(rvest)
library(lubridate)

strp_date <- function(x) as.Date(strptime(x, format = "%d %b %y"))

strp_date()

strp_date(vax_raw$DATE[1])
strp_date(tail(vax_raw$DATE, 1))

vax_raw <- covid_url %>% 
  bow() %>% 
  scrape() %>% 
  html_table() %>% 
  pluck(2)

vax_clean <- vax_raw %>% 
  mutate(
    DATE = strp_date(DATE)
  ) %>% 
  mutate(
    across(
      c(DOSES, NET),
      parse_number
    )
  ) %>% 
  select(-VAR) %>% 
  rename_with(.fn = tolower) %>% 
  mutate(year = year(date),
         month = month(date),
         day = mday(date),
         .after = date)

gg_covid_ts <- function(data){
  ggplot(data,
       aes(x = date,
           y = doses)) + 
  geom_line() 
}

gg_covid_ts(vax_clean) +
  scale_y_continuous(
    labels = scales::label_comma()
  )

vax_clean %>% 
  filter(year == 2021,
         month <= 8) %>% 
  gg_covid_ts() + 
  scale_y_continuous(
    labels = scales::label_comma(),
    limits = c(0, 4e7)
  ) +
  scale_x_date(
    labels = scales::label_date(format = "%b"),
    breaks = scales::date_breaks(),
    limits = as.Date(c("2021-02-15", "2021-12-31"))
  ) +
  labs(
    title = "Number of vaccinations in Australia in 2021",
    subtitle = "Everyone wants to be home for Christmas"
  ) +
  geom_vline(
    xintercept = as.Date("2021-12-25"),
    colour = "firebrick",
    lty = 4
  )
  

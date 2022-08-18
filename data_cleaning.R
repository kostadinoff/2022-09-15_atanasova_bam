# data cleaning

# loading libraries
library(tidyverse)
library(here)
library(readr)
library(lubridate)

setwd("~/MEGAsync/2022-09-15_atanasova_bam")
setwd("~/MEGA/MEGAsync/2022-09-15_atanasova_bam")

# functions 

calc_age <- function(birthDate, refDate) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  period$year
} 

calc_days<- function(day_1, day_2) {
  require(lubridate)
  period <- as.period(interval(day_1, day_2),
                      unit = "day")
  period$day
} 



round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
# Данни
## Секвенирани пациенти
sec_dat <- read_csv("sec_data.csv", 
                     col_types = cols(birth_date = col_date(format = "%Y-%m-%d"))) %>% 
  mutate_if(is.character, str_trim) %>% 
  mutate(variant_type = case_when(
                                  str_detect(code, "Alpha") ~ "Alpha",
                                  str_detect(code, "Delta") ~ "Delta",
                                  TRUE ~ "Omicron"))
                                  

# Всички пациенти

dat_all <- read_csv("all_patients.csv", 
                    col_types = cols(birth_date = col_date(format = "%Y-%m-%d"), 
                                     admit_day = col_date(format = "%Y-%m-%d"), 
                                     disch_day = col_date(format = "%Y-%m-%d"), 
                                     death_date = col_date(format = "%Y-%m-%d"))) %>% 
  select(-c(type, exam_day, date_pcr_results)) %>% 
  distinct() %>% 
  mutate_if(is.character, str_trim) %>% 
  mutate(age = calc_age(birth_date, admit_day),
         days_hospital = calc_days(admit_day,disch_day),
         days_to_death = calc_days(admit_day, death_date),
         week = week(admit_day),
         year = year(admit_day),
         week_year = str_c(week, year, sep = " ", collapse = NULL),
         month = month(admit_day),
         wmy = str_c(year, "W", week, sep = " ", collapse = NULL),
         my = str_c(year, "M", month, sep = " ", collapse = NULL),
         wday = wday(admit_day, label = TRUE),
         weekend = if_else(wday %in% c("съб", "нед"), "weekedn", "weekday")) %>% 
  filter(between(admit_day, as.Date('2021-07-01'), as.Date('2022-05-09')))

dat_all$my <- factor(dat_all$my, levels = c(
  "2021 M 7" ,"2021 M 8" , "2021 M 9" , "2021 M 10", "2021 M 11" , "2021 M 12", "2022 M 1" , "2022 M 2",
  "2022 M 3" , "2022 M 4" , "2022 M 5"))

dat_all$wmy <- factor(dat_all$wmy, levels = c("2021 W 27" ,"2021 W 28" ,"2021 W 29","2021 W 30" ,"2021 W 31" ,"2021 W 32","2021 W 33", "2021 W 34",
"2021 W 35" ,"2021 W 36" ,"2021 W 37" ,"2021 W 38", "2021 W 39", "2021 W 40", "2021 W 41", "2021 W 42",
"2021 W 43" ,"2021 W 44" ,"2021 W 45" ,"2021 W 46" ,"2021 W 47", "2021 W 48" ,"2021 W 49" ,"2021 W 50",
"2021 W 51" ,"2021 W 52" ,"2021 W 53" ,"2022 W 1"  ,"2022 W 2","2022 W 3"  ,"2022 W 4","2022 W 5" ,
"2022 W 6"  ,"2022 W 7"  ,"2022 W 8"  ,"2022 W 9"  ,"2022 W 10", "2022 W 11", "2022 W 12", "2022 W 13",
"2022 W 14" ,"2022 W 15" ,"2022 W 16" ,"2022 W 17" ,"2022 W 18" ,"2022 W 19"))
write_rds(dat_all, "all_patients.rds")

## Синхронизирани секвенирани пациенти
dat = sec_dat %>% 
  left_join(select(dat_all, -c(birth_date, sex)))

write_rds(dat, "dat.rds")
write.csv(dat, "dat.csv")






















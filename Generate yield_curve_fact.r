library(readrba)
library(dplyr)
library(tidyr)
library(lubridate)

rm(list = ls())
yc <- read_rba(table_no = c('f1', 'f2'))

startDate <- as_date("2015-01-01")
endDate <- today()


filtered_yc <- yc %>%
  filter((grepl("australian government", series, ignore.case = TRUE) &
           !grepl("indexed", series, ignore.case = TRUE) ) | 
           series == "Cash Rate Target",
            between(yc$date, startDate, endDate))

yield_curve <- filtered_yc %>% 
  select(date, series, value, frequency)

#yield_curve <- union_all(filtered_f1, filtered_f2)

yield_curve <- yield_curve %>%
  mutate(order = if_else(grepl('cash rate', series, ignore.case = TRUE ), 1, 
                         if_else(grepl('2 year', series, ignore.case = TRUE), 2, 
                                 if_else(grepl('3 year', series, ignore.case = TRUE), 3,
                                         if_else(grepl('5 year', series, ignore.case = TRUE), 4, 5)))))

#sort by date and order

yield_curve <- yield_curve[with(yield_curve, order(date, order)),]

#Remove redundant tables

rm(list = c('yc', 'filtered_yc'))
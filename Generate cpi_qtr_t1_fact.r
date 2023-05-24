#Bring in all installed packages
library(readabs)

library(tidyverse)

library(dplyr)

library(stringr)

library(lubridate)

library(tidyr)

library(fy)

library(psych)

library(plyr)

#**************************************************************************************************************

#Variables

#LF monthly
cpi_qtr <- "6401.0"

startDate <- as_date("2010-01-01")

endDate <- today()

#WPI indicators

cpi_qtr_base_fact <- read_abs(cat_no = cpi_qtr, 
                              tables = c('1','2'),
                              series_id = NULL,
                              path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                              metadata = TRUE,
                              show_progress_bars = FALSE,
                              retain_files = FALSE,
                              check_local = FALSE
)

#filter data on date variables

cpi_qtr_base_fact <- cpi_qtr_base_fact %>%
  filter(between(cpi_qtr_base_fact$date,startDate,endDate))

#split out series column
cpi_qtr_base_fact <- cpi_qtr_base_fact %>%
  separate_series()

#**************************************************************************************************************
# Functions


filter_funct_1 <- function(df, ...) {
  
  df_new <- df %>%
    filter(...)
  
  
  return(df_new)
}

filter_funct_2 <- function(df_new, val_name, ...){
  
  df_new <- rename(df_new, c("value"= val_name))
  
  #select relevant columns
  df_new <- df_new %>%
    select(all_of(c(...)), all_of(val_name))
  
  return(df_new)
  
}

#region function
region_funct <- function(df) {
  
  df_new <- df %>%
    mutate(region_id = if_else(str_detect(series_3, 'Sydney'), 'NSW', 
                               if_else(str_detect(series_3, 'Brisbane'), 'QLD',
                                       if_else(str_detect(series_3, 'Melbourne'), 'VIC', 
                                               if_else(str_detect(series_3, 'Hobart'), 'TAS',
                                                       if_else(str_detect(series_3, 'Adelaide'), 'SA', 
                                                               if_else(str_detect(series_3, 'Canberra'), 'ACT', 
                                                                       if_else(str_detect(series_3, 'Darwin'), 'NT', 
                                                                               if_else(str_detect(series_3, 'Perth'), 'WA', 'AUS')))))))))
  return(df_new)
  
}


#***************************************************************************************************************************************************************


#table 1 data
cpi_qtr_t1_fact_a  <- filter_funct_1 (cpi_qtr_base_fact,
                                      str_detect(series_1, "Index Numbers")) 
                                      
cpi_qtr_t1_fact_a <- filter_funct_2 (cpi_qtr_t1_fact_a,'qtr_index', 'date', 'series_3', 'series_type')


cpi_qtr_t1_fact_b  <- filter_funct_1 (cpi_qtr_base_fact,
                                      str_detect(series_1, "Previous Period")) 
                                  
cpi_qtr_t1_fact_b <- filter_funct_2 (cpi_qtr_t1_fact_b,'qtr_change', 'date', 'series_3', 'series_type')


cpi_qtr_t1_fact_c  <- filter_funct_1 (cpi_qtr_base_fact,
                                      str_detect(series_1,"Previous Year"))
                                      
cpi_qtr_t1_fact_c <- filter_funct_2 (cpi_qtr_t1_fact_c,'yr_change', 'date', 'series_3','series_type')


#Join the three tables above

cpi_qtr_t1_fact <- join_all(list(cpi_qtr_t1_fact_a, cpi_qtr_t1_fact_b, cpi_qtr_t1_fact_c),
                            by = c("date", "series_3"), type='left')

#remove duplicate columns
cpi_qtr_t1_fact <- cpi_qtr_t1_fact[!duplicated(as.list(cpi_qtr_t1_fact))]


#add linking columns using mutate function

cpi_qtr_t1_fact <- region_funct(cpi_qtr_t1_fact)

cpi_qtr_t1_fact <- rename(cpi_qtr_t1_fact, c("series_3"= "capital"))

#select relevant columns

cpi_qtr_t1_fact <- cpi_qtr_t1_fact %>%
  select(date, capital, region_id, series_type, qtr_index, qtr_change, yr_change)

#Remove redundant tables

rm(list = c('cpi_qtr_t1_fact_a', 'cpi_qtr_t1_fact_b', 'cpi_qtr_t1_fact_c', 'cpi_qtr_base_fact'))

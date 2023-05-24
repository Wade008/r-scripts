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

#WPI quarterly
wpi_qtr_sa <- "6345.0"



startDate <- as_date("2010-01-01")

endDate <- today()

#WPI indicators

wpi_qtr_base_fact <- read_abs(cat_no = wpi_qtr_sa, 
                                 tables = c('1','2b','3b','4b','5b'),
                                 series_id = NULL,
                                 path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                                 metadata = TRUE,
                                 show_progress_bars = FALSE,
                                 retain_files = FALSE,
                                 check_local = FALSE
)


#filter data on date variables

wpi_qtr_base_fact <- wpi_qtr_base_fact %>%
  filter(between(wpi_qtr_base_fact$date,startDate,endDate))

#split out series column
wpi_qtr_base_fact <- wpi_qtr_base_fact %>%
  separate_series()

#table 1 data
wpi_qtr_t1_fact  <- wpi_qtr_base_fact %>%
  filter(table_no =='634501', 
         series_1 =='Quarterly Index', 
         series_type =="Seasonally Adjusted")

#add linking columns using mutate function

#conditional column based on sector
wpi_qtr_t1_fact <- wpi_qtr_t1_fact %>%
  mutate(sector_type_id = if_else(str_detect(series_4, 'Private and Public'), 'ALL', 
                                  if_else(str_detect(series_4, 'Public'), 'PUB', 'PRI')))


wpi_qtr_t1_fact <- wpi_qtr_t1_fact %>%
  select(date, sector_type_id, series_type, value)


#rename the value column

wpi_qtr_t1_fact <- rename(wpi_qtr_t1_fact, c("value"="qtr_index"))
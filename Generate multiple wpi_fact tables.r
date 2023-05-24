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
wpi_qtr <- "6345.0"



startDate <- as_date("2010-01-01")

endDate <- today()

#WPI indicators

wpi_qtr_base_fact <- read_abs(cat_no = wpi_qtr, 
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

#**************************************************************************************************************
#Table 1

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

#select relevant columns
wpi_qtr_t1_fact <- wpi_qtr_t1_fact %>%
  select(date, sector_type_id, series_type, value)


#rename the value column

wpi_qtr_t1_fact <- rename(wpi_qtr_t1_fact, c("value"="qtr_index"))

#**************************************************************************************************************
#Tables 2b to 4b


wpi_qtr_t2b4b_fact  <- wpi_qtr_base_fact %>%
  filter(table_no %in% c('634502b', '634503b', '634504b'),
         series_1 =='Quarterly Index')


#conditional column based on sector
wpi_qtr_t2b4b_fact <- wpi_qtr_t2b4b_fact %>%
  mutate(sector_type_id = if_else(str_detect(series_4, 'Private and Public'), 'ALL', 
                                  if_else(str_detect(series_4, 'Public'), 'PUB', 'PRI')))

       

#conditional column based on state
wpi_qtr_t2b4b_fact <- wpi_qtr_t2b4b_fact %>%
  mutate(region_id = if_else(str_detect(series_3, 'New South Wales'), 'NSW', 
                             if_else(str_detect(series_3, 'Queensland'), 'QLD',
                                     if_else(str_detect(series_3, 'Victoria'), 'VIC', 
                                             if_else(str_detect(series_3, 'Tasmania'), 'TAS',
                                                     if_else(str_detect(series_3, 'South Australia'), 'SA', 
                                                             if_else(str_detect(series_3, 'Australian Capital Territory'), 'ACT', 
                                                                     if_else(str_detect(series_3, 'Northern Territory'), 'NT', 
                                                                             if_else(str_detect(series_3, 'Western Australia'), 'WA', 'AUS')))))))))


#select relevant column
wpi_qtr_t2b4b_fact <- wpi_qtr_t2b4b_fact %>%
  select(date, sector_type_id, region_id, series_type, value)


#rename the value column

wpi_qtr_t2b4b_fact <- rename(wpi_qtr_t2b4b_fact, c("value"="qtr_index"))

#**************************************************************************************************************
#Table 5b

wpi_qtr_t5b_fact  <- wpi_qtr_base_fact %>%
  filter(table_no == '634505b',
         series_1 =='Quarterly Index')


#conditional column based on sector
wpi_qtr_t5b_fact <- wpi_qtr_t5b_fact %>%
  mutate(sector_type_id = if_else(str_detect(series_4, 'Private and Public'), 'ALL', 
                                  if_else(str_detect(series_4, 'Public'), 'PUB', 'PRI')))


#conditional column based on on industry

wpi_qtr_t5b_fact <- wpi_qtr_t5b_fact %>%
  mutate(ind_id = if_else(str_detect(str_to_lower(series_5), str_to_lower('Agriculture, Forestry and Fishing')), 'AFF',
                          if_else(str_detect(str_to_lower(series_5),str_to_lower('Mining')), 'MIN',
                                  if_else(str_detect(str_to_lower(series_5), str_to_lower('Manufacturing')), 'MAN',
                                          if_else(str_detect(str_to_lower(series_5), str_to_lower('Electricity, Gas, Water and Waste Services')), 'EGW',
                                                  if_else(str_detect(str_to_lower(series_5), str_to_lower('Construction')), 'CON',
                                                          if_else(str_detect(str_to_lower(series_5), str_to_lower('Wholesale Trade')), 'WT',
                                                                  if_else(str_detect(str_to_lower(series_5), str_to_lower('Retail Trade')), 'RT',
                                                                          if_else(str_detect(str_to_lower(series_5), str_to_lower('Accommodation and Food Services')), 'AFS',
                                                                                  if_else(str_detect(str_to_lower(series_5), str_to_lower('Transport, Postal and Warehousing')), 'TPW',
                                                                                          if_else(str_detect(str_to_lower(series_5), str_to_lower('Information Media and Telecommunications')), 'IMT',
                                                                                                  if_else(str_detect(str_to_lower(series_5), str_to_lower('Financial and Insurance Services')), 'FIS',
                                                                                                          if_else(str_detect(str_to_lower(series_5), str_to_lower('Rental, Hiring and Real Estate Services')), 'RHR',
                                                                                                                  if_else(str_detect(str_to_lower(series_5), str_to_lower('Professional, Scientific and Technical Services')), 'PST',
                                                                                                                          if_else(str_detect(str_to_lower(series_5), str_to_lower('Administrative and Support Services')), 'ASS',
                                                                                                                                  if_else(str_detect(str_to_lower(series_5), str_to_lower('Public Administration and Safety')), 'PAS',
                                                                                                                                          if_else(str_detect(str_to_lower(series_5), str_to_lower('Education and Training')), 'ET',
                                                                                                                                                  if_else(str_detect(str_to_lower(series_5), str_to_lower('Health Care and Social Assistance')), 'HCS',
                                                                                                                                                          if_else(str_detect(str_to_lower(series_5), str_to_lower('Arts and Recreation Services')), 'ARS', 
                                                                                                                                                                  if_else(str_detect(str_to_lower(series_5), str_to_lower('Other Services')), 'OS', 'ALL')
                                                                                                                                                  )))))))))))))))))))

#select relevant column
wpi_qtr_t5b_fact <- wpi_qtr_t5b_fact %>%
  select(date, sector_type_id, ind_id, series_type, value)



#rename the value column

wpi_qtr_t5b_fact <- rename(wpi_qtr_t5b_fact, c("value"="qtr_index"))

#******************************************************************************************************************

rm(wpi_qtr_base_fact)

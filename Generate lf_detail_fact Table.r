#Bring in all installed packages
library(readabs)

library(tidyverse)

library(plyr)

library(dplyr)

library(stringr)

library(lubridate)

library(tidyr)

library(fy)

library(psych)




#**************************************************************************************************************

#Variables

#LF detailed
lf_detail <- "6291.0.55.001"

startDate <- as_date("2010-01-01")

endDate <- today()




#Labour force indicators
  
lf_detail_fact <- read_abs(cat_no = lf_detail, 
                           tables = 5,
                           series_id = NULL,
                           path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                           metadata = TRUE,
                           show_progress_bars = FALSE,
                           retain_files = FALSE,
                           check_local = FALSE
  )
  
#filter data on date variables

lf_detail_fact <- lf_detail_fact %>%
  filter(between(lf_detail_fact$date,startDate,endDate))

lf_detail_fact <- lf_detail_fact %>%
  separate_series()



# replace na in series_1 with value from series_2
lf_detail_fact$series_1 <- ifelse(is.na(lf_detail_fact$series_1), lf_detail_fact$series_2, lf_detail_fact$series_1)

# replace region name in series_2 with 'All Industries'
lf_detail_fact$series_2 <- ifelse(lf_detail_fact$series_2 == lf_detail_fact$series_1 , "All Industries", (lf_detail_fact$series_2))




#remove all total values

lf_detail_fact <- lf_detail_fact[!(lf_detail_fact$series_1=="Australia"),]
lf_detail_fact <- lf_detail_fact[!(lf_detail_fact$series_2=="All Industries"),]
lf_detail_fact <- lf_detail_fact[!(lf_detail_fact$series_3=="Employed total"),]






#add linking columns using mutate function

#conditional column based on state
lf_detail_fact <- lf_detail_fact %>%
  mutate(region_id = if_else(str_detect(series_1, 'New South Wales'), 'NSW', 
                             if_else(str_detect(series_1, 'Queensland'), 'QLD',
                                   if_else(str_detect(series_1, 'Victoria'), 'VIC', 
                                          if_else(str_detect(series_1, 'Tasmania'), 'TAS',
                                                if_else(str_detect(series_1, 'South Australia'), 'SA', 
                                                     if_else(str_detect(series_1, 'Australian Capital Territory'), 'ACT', 
                                                         if_else(str_detect(series_1, 'Northern Territory'), 'NT', 'WA'))))))))


#conditional column based on on industry

lf_detail_fact <- lf_detail_fact %>%
  mutate(ind_id = if_else(str_detect(series_2, 'Agriculture, Forestry and Fishing'), 'AFF',
                          if_else(str_detect(series_2, 'Mining'), 'MIN',
                                  if_else(str_detect(series_2, 'Manufacturing'), 'MAN',
                                          if_else(str_detect(series_2, 'Electricity, Gas, Water and Waste Services'), 'EGW',
                                                  if_else(str_detect(series_2, 'Construction'), 'CON',
                                                          if_else(str_detect(series_2, 'Wholesale Trade'), 'WT',
                                                                  if_else(str_detect(series_2, 'Retail Trade'), 'RT',
                                                                          if_else(str_detect(series_2, 'Accommodation and Food Services'), 'AFS',
                                                                                  if_else(str_detect(series_2, 'Transport, Postal and Warehousing'), 'TPW',
                                                                                         if_else(str_detect(series_2, 'Information Media and Telecommunications'), 'IMT',
                                                                                                 if_else(str_detect(series_2, 'Financial and Insurance Services'), 'FIS',
                                                                                                        if_else(str_detect(series_2, 'Rental, Hiring and Real Estate Services'), 'RHR',
                                                                                                                if_else(str_detect(series_2, 'Professional, Scientific and Technical Services'), 'PST',
                                                                                                                       if_else(str_detect(series_2, 'Administrative and Support Services'), 'ASS',
                                                                                                                          if_else(str_detect(series_2, 'Public Administration and Safety'), 'PAS',
                                                                                                                               if_else(str_detect(series_2, 'Education and Training'), 'ET',
                                                                                                                                 if_else(str_detect(series_2, 'Health Care and Social Assistance'), 'HCS',
                                                                                                                                           if_else(str_detect(series_2, 'Arts and Recreation Services'), 'ARS', 'OS')
                                                                                                                                           ))))))))))))))))))


#conditional column based on employment type

lf_detail_fact <- lf_detail_fact %>%
  mutate(emp_type_id = if_else(str_detect(series_3, 'Employed full-time'), 'FT', 'PT'))

#select relevant columns

lf_detail_fact <- lf_detail_fact %>%
  select(date, region_id, ind_id, emp_type_id, value )

#rename the value column

lf_detail_fact <- rename(lf_detail_fact, c("value"="no_employed"))



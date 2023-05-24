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

#UE Monthly
ue_monthly <- "6202.0"

startDate <- as_date("2010-01-01")

endDate <- today()
#**************************************************************************************************************

#Labour force indicators

ue_monthly_base_fact <- read_abs(cat_no = ue_monthly, 
                                 tables = 12,
                                 series_id = NULL,
                                 path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                                 metadata = TRUE,
                                 show_progress_bars = FALSE,
                                 retain_files = FALSE,
                                 check_local = FALSE
)

#filter data on date variables

ue_monthly_base_fact <- ue_monthly_base_fact %>%
  filter(between(ue_monthly_base_fact$date,startDate,endDate))

#split out series column
ue_monthly_base_fact <- ue_monthly_base_fact %>%
  separate_series()

#monthly employment figures

ue_monthly_uemp_fact  <- ue_monthly_base_fact %>%
  filter(series_type %in% "Seasonally Adjusted",
         series_1 %in% c('Unemployment rate'))



#add linking columns using mutate function

#conditional column based on state
ue_monthly_uemp_fact <- ue_monthly_uemp_fact %>%
  mutate(region_id = if_else(str_detect(series_3, 'New South Wales'), 'NSW', 
                             if_else(str_detect(series_3, 'Queensland'), 'QLD',
                                     if_else(str_detect(series_3, 'Victoria'), 'VIC', 
                                             if_else(str_detect(series_3, 'Tasmania'), 'TAS',
                                                     if_else(str_detect(series_3, 'South Australia'), 'SA', 
                                                             if_else(str_detect(series_3, 'Australian Capital Territory'), 'ACT', 
                                                                     if_else(str_detect(series_3, 'Northern Territory'), 'NT', 
                                                                             if_else(str_detect(series_3, 'Western Australia'), 'WA', 'AUS')))))))))


#conditional column based on gender

ue_monthly_uemp_fact <- ue_monthly_uemp_fact %>%
  mutate(gender_id = if_else(str_detect(series_2, 'Males'), 'M', 
                             if_else(str_detect(series_2,'Females'), 'F', 'ALL')))

ue_monthly_uemp_fact <- ue_monthly_uemp_fact[order(ue_monthly_uemp_fact$date,
                                                 ue_monthly_uemp_fact$region_id, 
                                                 ue_monthly_uemp_fact$gender_id), ]



#select relevant columns

ue_monthly_uemp_fact <- ue_monthly_uemp_fact %>%
  select(date, region_id, gender_id, series_type, value )


#rename the value column

ue_monthly_uemp_fact <- rename(ue_monthly_uemp_fact, c("value"="unemployment_rate"))

rm('ue_monthly_base_fact')








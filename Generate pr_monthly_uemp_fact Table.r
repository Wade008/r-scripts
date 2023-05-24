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
pr_monthly <- "6202.0"

startDate <- as_date("2010-01-01")

endDate <- today()
#**************************************************************************************************************

#Labour force indicators

pr_monthly_base_fact <- read_abs(cat_no = pr_monthly, 
                                 tables = 12,
                                 series_id = NULL,
                                 path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                                 metadata = TRUE,
                                 show_progress_bars = FALSE,
                                 retain_files = FALSE,
                                 check_local = FALSE
)

#filter data on date variables

pr_monthly_base_fact <- pr_monthly_base_fact %>%
  filter(between(pr_monthly_base_fact$date,startDate,endDate))

#split out series column
pr_monthly_base_fact <- pr_monthly_base_fact %>%
  separate_series()

#monthly employment figures

pr_monthly_uemp_fact  <- pr_monthly_base_fact %>%
  filter(series_type %in% "Seasonally Adjusted",
         series_1 %in% c('Participation rate'))



#add linking columns using mutate function

#conditional column based on state
pr_monthly_uemp_fact <- pr_monthly_uemp_fact %>%
  mutate(region_id = if_else(str_detect(series_3, 'New South Wales'), 'NSW', 
                             if_else(str_detect(series_3, 'Queensland'), 'QLD',
                                     if_else(str_detect(series_3, 'Victoria'), 'VIC', 
                                             if_else(str_detect(series_3, 'Tasmania'), 'TAS',
                                                     if_else(str_detect(series_3, 'South Australia'), 'SA', 
                                                             if_else(str_detect(series_3, 'Australian Capital Territory'), 'ACT', 
                                                                     if_else(str_detect(series_3, 'Northern Territory'), 'NT', 
                                                                             if_else(str_detect(series_3, 'Western Australia'), 'WA', 'AUS')))))))))


#conditional column based on gender

pr_monthly_uemp_fact <- pr_monthly_uemp_fact %>%
  mutate(gender_id = if_else(str_detect(series_2, 'Males'), 'M', 
                             if_else(str_detect(series_2,'Females'), 'F', 'ALL')))

pr_monthly_uemp_fact <- pr_monthly_uemp_fact[order(pr_monthly_uemp_fact$date,
                                                   pr_monthly_uemp_fact$region_id, 
                                                   pr_monthly_uemp_fact$gender_id), ]



#select relevant columns

pr_monthly_uemp_fact <- pr_monthly_uemp_fact %>%
  select(date, region_id, gender_id, series_type, value )


#rename the value column

pr_monthly_uemp_fact <- rename(pr_monthly_uemp_fact, c("value"="participation_rate"))

rm('pr_monthly_base_fact')

#Bring in all installed packages
library(tidyverse)

library(dplyr)

library(stringr)

library(lubridate)

library(tidyr)

library(fy)

library(psych)

library(plyr)



#Region dimension table
region_id <- c('AUS','QLD', 'NSW', 'VIC', 'TAS', 'SA', 'ACT', 'NT', 'WA')
region <- c('Australia', 'Queensland', 'New South Wales', 'Victoria', 'Tasmania', 
            'South Australia', 'Australia Capital Territory', 'Northern Territory', 
            'Western Australia')

order <- c(9, 3, 1, 2, 6, 4, 8, 7, 5)

region_dim <- data.frame(region_id, region, order)

region_dim <- region_dim %>%
  mutate(country = "Australia")

#remove unnecessary data objects
rm(region_id, region, order)
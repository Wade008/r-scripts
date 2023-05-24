
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


#employment type dimension table

emp_type_id <- c('ALL', 'FT', 'PT')
employment_type <- c('All', 'Full-time','Part-time')

emp_type_dim <- data.frame(emp_type_id, employment_type)

rm(emp_type_id, employment_type)

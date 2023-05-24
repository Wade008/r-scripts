#Bring in all installed packages
library(tidyverse)

library(dplyr)

library(stringr)

library(lubridate)

library(tidyr)

library(fy)

library(psych)

library(plyr)


#variables

startDate <- as_date("2010-01-01")

endDate <- today()



#Date dimension table
dateVec <- seq(startDate,endDate,"days")

date_dim <- data.frame(dateVec)
colnames(date_dim) <- c("date")

date_dim <- date_dim %>%
  mutate(qtr = paste0("Qtr ",quarter(date),", ",year(date)),
         year = year(date), fin_yr = date2fy(date))

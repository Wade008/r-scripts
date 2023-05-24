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

#gender dimension table
gender_id <- c('ALL','M','F')
gender <- c('All', 'Male', 'Female')

gender_dim <- data.frame(gender_id, gender)

rm(gender_id, gender)

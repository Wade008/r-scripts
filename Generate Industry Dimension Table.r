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

#industry dimension table


ind_id <- c('ALL','AFF','MIN' ,'MAN','EGW','CON', 'WT', 'RT', 'AFS', 'TPW', 'IMT', 'FIS', 'RHR', 'PST','ASS', 'PAS', 'ET', 'HCS', 'ARS', 'OS')
industry <- c('All Industries', 'Agriculture, Forestry and Fishing', 'Mining', 'Manufacturing', 'Electricity, Gas, Water and Waste Services', 'Construction',
              'Wholesale Trade', 'Retail Trade', 'Accommodation and Food Services', 'Transport, Postal and Warehousing', 'Information Media and Telecommunications',
              'Financial and Insurance Services', 'Rental, Hiring and Real Estate Services', 'Professional, Scientific and Technical Services',
              'Administrative and Support Services', 'Public Administration and Safety', 'Education and Training', 'Health Care and Social Assistance',
              'Arts and Recreation Services', 'Other Services')

industry_dim <- data.frame(ind_id, industry)

#remove unnecessary data objects
rm(ind_id, industry)

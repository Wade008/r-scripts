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

library(rlang)
#**************************************************************************************************************

#get base data and conduct an initial tidy up. 

#na quarterly
na_qtr <- "5206.0"

startDate <- as_date("2010-01-01")

endDate <- today()

#na indicators

na_qtr_base_fact <- read_abs(cat_no = na_qtr, 
                              tables = c('1'),
                              series_id = NULL,
                              path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                              metadata = TRUE,
                              show_progress_bars = FALSE,
                              retain_files = FALSE,
                              check_local = FALSE
)

#filter na data on date variables

na_qtr_base_fact <- na_qtr_base_fact %>%
  filter(between(na_qtr_base_fact$date,startDate,endDate))

#split out series column
na_qtr_base_fact <- na_qtr_base_fact%>%
  separate_series()

#SA indicators
sa_qtr_base_fact <- read_abs(cat_no = na_qtr, 
                             tables = c('25'),
                             series_id = NULL,
                             path = Sys.getenv("R_READABS_PATH", unset = tempdir()),
                             metadata = TRUE,
                             show_progress_bars = FALSE,
                             retain_files = FALSE,
                             check_local = FALSE
)


#filter sa data on date variables

sa_qtr_base_fact <- sa_qtr_base_fact %>%
  filter(between(sa_qtr_base_fact$date,startDate,endDate))

#split out series column
sa_qtr_base_fact <- sa_qtr_base_fact%>%
  separate(series, c('region', 'sector', 'metric'), ';', extra="merge" )
  




#**************************************************************************************************************#
#Functions

filter_funct_1 <- function(df, ...) {
  
  df_new <- df %>%
    filter(...)
  
  
  return(df_new)
}

filter_funct_2 <- function(df_new, val_name, ...){
  
  df_new <- rename(df_new, c("value"= val_name))
  
  #select relevant columns
  df_new <- df_new %>%
    select(all_of(c(...)), all_of(val_name))
  
  return(df_new)
  
}

#region function
region_funct <- function(df) {
  
  df_new <- df %>%
    mutate(region_id = if_else(str_detect(region, 'New South Wales'), 'NSW', 
                               if_else(str_detect(region, 'Queensland'), 'QLD',
                                       if_else(str_detect(region, 'Victoria'), 'VIC', 
                                               if_else(str_detect(region, 'Tasmania'), 'TAS',
                                                       if_else(str_detect(region, 'South Australia'), 'SA', 
                                                               if_else(str_detect(region, 'Australian Capital Territory'), 'ACT', 
                                                                       if_else(str_detect(region, 'Northern Territory'), 'NT', 
                                                                               if_else(str_detect(region, 'Western Australia'), 'WA', 'AUS')))))))))
  return(df_new)
  
}

rename_funct <- function(df) {
  
  df_new <- rename(df, c("series_1" = "metric"))
  
  return (df_new)
  
}


#*************************************************************************************************************#
#Join CVM data into one table
#table cvm data - Key aggregates
na_qtr_base_fact_cv_a  <- filter_funct_1 (na_qtr_base_fact,
                                      series_type =='Seasonally Adjusted',
                                      str_detect(tolower(series_1),'chain volume measures - percentage changes'))
na_qtr_base_fact_cv_a <- filter_funct_2 (na_qtr_base_fact_cv_a,'change', 'date', 'series_1','series_type')


#remove " - Percentage changes"
na_qtr_base_fact_cv_a$series_1 <- gsub(' - percentage changes' ,'', na_qtr_base_fact_cv_a$series_1,
                                  ignore.case = TRUE)


na_qtr_base_fact_cv_b  <- filter_funct_1 (na_qtr_base_fact,
                                       series_type =='Seasonally Adjusted',
                                       str_detect(tolower(series_1),'chain volume measures'),
                                       unit %in% c('$ Millions', '$'))
na_qtr_base_fact_cv_b <- filter_funct_2 (na_qtr_base_fact_cv_b,'levels', 'date', 'series_1','series_type')



#Join the two tables above

na_qtr_kacvm_fact <- join_all(list(na_qtr_base_fact_cv_a, na_qtr_base_fact_cv_b),
                               by = c('date','series_1','series_type'), type='inner')


#Add $ or $M conditionally to series_1 column


na_qtr_kacvm_fact$series_1 <- if_else(str_detect(na_qtr_kacvm_fact$series_1, "per capita"),
                                        paste(na_qtr_kacvm_fact$series_1, "($)", sep=" " ), paste(na_qtr_kacvm_fact$series_1,"($m)", sep=" "))
#call rename_funct
na_qtr_kacvm_fact <- rename_funct(na_qtr_kacvm_fact)
  
 
#use if_else to assign and extra column for ordering

na_qtr_kacvm_fact <- na_qtr_kacvm_fact %>%
  mutate(order = if_else(metric == 'Gross domestic product: Chain volume measures ($m)', 1,
                         if_else(metric == 'GDP per capita: Chain volume measures ($)', 2, 
                            if_else(metric == 'Gross value added market sector: Chain volume measures ($m)', 3, 
                              if_else(metric == 'Net domestic product: Chain volume measures ($m)', 4, 
                                  if_else(metric == 'Real gross domestic income: Chain volume measures ($m)', 5,
                                      if_else(metric == 'Real gross national income: Chain volume measures ($m)', 6,
                                          if_else(metric == 'Real net national disposable income: Chain volume measures ($m)', 7, 8))))))))




#Get current prices metrics

#table current prices data - Key aggregates - exclude percentage change
na_qtr_base_fact_cp_a  <- filter_funct_1 (na_qtr_base_fact,
                                       series_type =='Seasonally Adjusted',
                                       str_detect(tolower(series_1),'current prices'),
                                       str_detect(tolower(series_1), 'percentage changes', negate = TRUE))
na_qtr_base_fact_cp_a <- filter_funct_2 (na_qtr_base_fact_cp_a,'levels', 'date', 'series_1','series_type')



#table current prices data - key aggregates - only percentage change for GDP

na_qtr_base_fact_cp_b  <- filter_funct_1 (na_qtr_base_fact,
                                          series_type =='Seasonally Adjusted',
                                          str_detect(tolower(series_1),'current prices - percentage changes'))
na_qtr_base_fact_cp_b <- filter_funct_2 (na_qtr_base_fact_cp_b,'change', 'date', 'series_1','series_type')


#Add $ or $M conditionally to series_1 column in na_qtr_base_fact_cp_a 

na_qtr_base_fact_cp_a$series_1 <- if_else(str_detect(na_qtr_base_fact_cp_a$series_1, "per capita"),
                                      paste(na_qtr_base_fact_cp_a$series_1, "($)", sep=" " ), paste(na_qtr_base_fact_cp_a$series_1,"($m)", sep=" "))

#call rename_funct
na_qtr_kacpv_fact <- rename_funct(na_qtr_base_fact_cp_a)

#use if_else to assign and extra column for ordering

na_qtr_kacpv_fact <- na_qtr_kacpv_fact %>%
  mutate(order = if_else(metric == 'Gross domestic product: Current prices ($m)', 1,
                         if_else(metric == 'GDP per capita: Current prices ($)', 2, 
                                 if_else(metric == 'Gross national income: Current prices ($m)', 3, 4))))


#call rename_funct
na_qtr_kacppc_fact <- rename_funct(na_qtr_base_fact_cp_b)

#remove " - Percentage changes"
na_qtr_kacppc_fact$metric <- gsub(' - Percentage changes' ,'', na_qtr_kacppc_fact$metric,
                                        ignore.case = TRUE)



#Get household savings ratio

na_qtr_base_fact_hsr  <- filter_funct_1 (na_qtr_base_fact,
                                          series_type =='Seasonally Adjusted',
                                          str_detect(tolower(series_1),'household saving ratio'))
na_qtr_base_fact_hsr <- filter_funct_2 (na_qtr_base_fact_hsr,'ratio', 'date', 'series_1','series_type')

#call rename_funct
na_qtr_kahsr_fact <- rename_funct(na_qtr_base_fact_hsr)

#remove the term :Ratio from the metric column
na_qtr_kahsr_fact$metric <- gsub(': Ratio' ,'', na_qtr_kahsr_fact$metric,
                                 ignore.case = TRUE)




#get index data including terms of trade data

na_qtr_base_fact_ind_a  <- filter_funct_1 (na_qtr_base_fact,
                                         series_type =='Seasonally Adjusted',
                                         str_detect(tolower(series_1),'index - percentage changes'))
na_qtr_base_fact_ind_a <- filter_funct_2 (na_qtr_base_fact_ind_a ,'change', 'date', 'series_1','series_type')

#remove " - Percentage changes"
na_qtr_base_fact_ind_a$series_1 <- gsub(' - Percentage changes' ,'', na_qtr_base_fact_ind_a$series_1,
                                       ignore.case = TRUE)


na_qtr_base_fact_ind_b  <- filter_funct_1 (na_qtr_base_fact,
                                          series_type =='Seasonally Adjusted',
                                          str_detect(tolower(series_1),'index'),
                                          str_detect(tolower(series_1), 'percentage changes', negate = TRUE))
na_qtr_base_fact_ind_b <- filter_funct_2 (na_qtr_base_fact_ind_b,'levels', 'date', 'series_1','series_type')


#Join the two tables above

na_qtr_index_fact <- join_all(list(na_qtr_base_fact_ind_a, na_qtr_base_fact_ind_b),
                              by = c('date','series_1','series_type'), type='inner')


#call rename_funct
na_qtr_index_fact <- rename_funct(na_qtr_index_fact)

#use if_else to assign and extra column for ordering

na_qtr_index_fact <- na_qtr_index_fact %>%
  mutate(order = if_else(metric == 'Hours worked: Index', 1,
                         if_else(metric == 'Hours worked market sector: Index', 2, 
                                 if_else(metric == 'GDP per hour worked: Index', 3, 
                                         if_else(metric == 'Gross value added per hour worked market sector: Index', 4, 
                                                 if_else(metric == 'Gross domestic product: Index', 5,
                                                         if_else(metric == 'Domestic final demand: Index', 6,  7)))))))




#remove all redundant tables here

rm(list = c('na_qtr_base_fact_cv_a', 'na_qtr_base_fact_cv_b', 'na_qtr_base_fact_cp_a', 'na_qtr_base_fact_cp_b', 'na_qtr_base_fact_hsr',
            'na_qtr_base_fact_ind_a', 'na_qtr_base_fact_ind_b', 'na_qtr_base_fact'))

#******************************************************************************************************#

#State final demand data

# Key aggregates table a
sa_qtr_base_fact_a  <- filter_funct_1 (sa_qtr_base_fact,
                                       series_type =='Seasonally Adjusted',
                                       unit =='Percent')
sa_qtr_base_fact_a <- filter_funct_2 (sa_qtr_base_fact_a,'qtr_change', 'date', 'region','sector', 'metric', 'series_type')

sa_qtr_base_fact_a$sector <- trimws(sa_qtr_base_fact_a$sector, which=c('both'))


sa_qtr_base_fact_a$metric <- str_sub(sa_qtr_base_fact_a$metric, 1, nchar(sa_qtr_base_fact_a$metric)-2)

sa_qtr_base_fact_a <- sa_qtr_base_fact_a %>%
  unite( 'metric'  ,c('sector', 'metric'), sep='')

sa_qtr_base_fact_a$metric <- str_to_sentence(sa_qtr_base_fact_a$metric)

#remove the words ": percentage changes"

sa_qtr_base_fact_a$metric <- gsub(': percentage changes' ,'', sa_qtr_base_fact_a$metric,
                                  ignore.case = TRUE)



# Key aggregates table b
sa_qtr_base_fact_b  <- filter_funct_1 (sa_qtr_base_fact,
                                       series_type =='Seasonally Adjusted',
                                       unit =='$ Millions')
sa_qtr_base_fact_b <- filter_funct_2 (sa_qtr_base_fact_b,'levels', 'date', 'region','sector', 'metric', 'series_type')

sa_qtr_base_fact_b$sector <- trimws(sa_qtr_base_fact_b$sector, which=c('both'))


sa_qtr_base_fact_b$metric <- str_sub(sa_qtr_base_fact_b$metric, 1, nchar(sa_qtr_base_fact_b$metric)-2)

sa_qtr_base_fact_b <- sa_qtr_base_fact_b %>%
  unite( 'metric'  ,c('sector', 'metric'), sep='')

sa_qtr_base_fact_b$metric <- str_to_sentence(sa_qtr_base_fact_b$metric)


#Join the two tables above

sa_qtr_kacvm_fact <- join_all(list(sa_qtr_base_fact_a, sa_qtr_base_fact_b),
                              by = c('date','region', 'metric','series_type'), type='inner')


#Add region_id
sa_qtr_kacvm_fact <- region_funct(sa_qtr_kacvm_fact)

sa_qtr_kacvm_fact <- sa_qtr_kacvm_fact %>%
  select(date, region_id, metric, series_type, qtr_change, levels)


#Remove all redundant tables
rm(list = c('sa_qtr_base_fact_a', 'sa_qtr_base_fact_b', 'sa_qtr_base_fact'))

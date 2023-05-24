
#series_type dimension table
series_type_id <- c('O', 'S')
series_type <- c('Original', 'Seasonally Adjusted')

series_type_dim <- data.frame(series_type_id, series_type)

rm(series_type_id, series_type)
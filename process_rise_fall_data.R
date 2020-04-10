col_diff <- function(dataframe, headers, rev = F){
  new_df <- dataframe %>% 
    select(headers)
  cols <- colnames(dataframe)
  cols <- cols[! cols %in% headers]
  # pattern of b - a
  col_a <- cols[-length(cols)]
  col_b <- cols[-1]
  if (rev) {
    col_temp <- col_b
    col_b <- col_a
    col_a <- col_temp
  }
  new_headers <- paste(col_b, col_a, sep = "-")
  new_df[new_headers] <- dataframe[col_b]- dataframe[col_a]
  return(new_df)
}

swing <- function(num) {
  num_greater <- num > 0
  new_num <- num
  new_num[num_greater] <- "Positive Change"
  num_greater <- num < 0
  new_num[num_greater] <- "Negative Change"
  num_greater <- num == 0
  new_num[num_greater] <- "No Change"
  return(new_num)
}

# get the columns that contain differences
difference_cpi <- col_diff(corruption_data, c("country", "ISO3", "Region"))
difference_wb <- col_diff(gdp_per_capita_wide, c("iso3c", "iso2c", "country", "indicatorID", "indicator"), rev = T) %>% 
  select(-iso2c)

# count number of increases, decreases, and do-nothings (excluding n/a) for both dfs\
swing_cpi <- difference_cpi %>% 
  gather(key = year_range, value = cpi_swing, -country, -ISO3, -Region) %>% 
  filter(!is.na(cpi_swing)) %>% 
  mutate(year_range = gsub("cpi_", "", year_range), swing = swing(cpi_swing)) %>% 
  mutate(year_range = gsub("X", "", year_range))

swing_wb <- difference_wb %>% 
  select(-indicator, -indicatorID) %>% 
  gather(key = year_range, value = wb_swing, -country, -iso3c) %>% 
  mutate(swing = swing(wb_swing)) 

pos_cpi <- swing_cpi %>% 
  filter(swing == "Positive Change")
neg_cpi<- swing_cpi %>% 
  filter(swing == "Negative Change")
pos_wb<- swing_wb %>% 
  filter(swing == "Positive Change")
neg_wb<- swing_wb %>% 
  filter(swing == "Negative Change")

# filter for positive cpi and positive wb, join based on matches and count
pos_pos <- inner_join(pos_cpi, pos_wb, by = c("country", "year_range")) %>% 
  group_by(year_range) %>% 
  mutate(status = "CPI Rise, GDP Rise") %>%
  select(country, ISO3, year_range, status)
# gather(key = key, value = value, -year_range, -status)
# filter for negative cpi and positive wb, join based on matches and count
pos_neg <- inner_join(neg_cpi, pos_wb, by = c("country", "year_range")) %>% 
  group_by(year_range) %>% 
  mutate(status = "CPI Fall, GDP Rise") %>% 
  select(country, ISO3, year_range, status)
# filter for positive cpi and negative wb, join based on matches and count
neg_pos <- inner_join(pos_cpi, neg_wb, by = c("country", "year_range")) %>% 
  group_by(year_range) %>% 
  mutate(status = "CPI Rise, GDP Fall") %>% 
  select(country, ISO3, year_range, status)
# filter for negative cpi and negative wb, join based on matches and count
neg_neg <- inner_join(neg_cpi, neg_wb, by = c("country", "year_range")) %>% 
  group_by(year_range) %>% 
  mutate(status = "CPI Fall, GDP Fall") %>% 
  select(country, ISO3, year_range, status)

faceted_groups <- rbind(pos_pos, pos_neg, neg_pos, neg_neg) %>% 
  mutate(start = as.numeric(substr(year_range, 1, 4)), stop = as.numeric(substr(year_range, 6, 9))) 

country_dataframe <- faceted_groups %>%
  ungroup() %>% 
  select(country) %>% 
  unique()
max_year <- max(faceted_groups$stop)
min_year <- min(faceted_groups$start)
starts <- min_year:(max_year-1)
stops <- (min_year+1):max_year
year_ranges <- paste0(starts, "-", stops)

culmination <- function(vector) {
  new_vector <- c()
  for (x in 1:length(vector)) {
    new_vector <- c(new_vector, sum(c(new_vector[length(new_vector)], vector[x])))
  }
  return(new_vector)
}

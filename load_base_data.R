options(scipen = 999)

# Load in corruption data
file_location <- "clean_data/CPI_"
unique_years <- c(2000:2011, "2012_2019")
files <- rev(paste0(file_location, unique_years, ".csv"))

corruption_df <- read.csv(files[1], stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>% 
  select(-contains("X."))
for (file in files[-1]) {
  temp_file <-read.csv(file, stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  corruption_df <- corruption_df %>% 
    left_join(temp_file)
}

corruption_cpi_df <- corruption_df %>% 
  select(country, ISO3, Region, contains("cpi")) %>% 
  mutate(cpi_2005 = as.numeric(cpi_2005))


# scale up the years 2000 - 2011
times_ten <- paste0("cpi_", 2011:2000)
corruption_cpi_df[, times_ten] <- corruption_cpi_df[, times_ten] * 10
colnames(corruption_cpi_df) <- gsub("cpi_", "", colnames(corruption_cpi_df))



# Load in press_freedom data
raw_press_freedom_index <- read.csv("clean_data/press_freedom_index.csv", stringsAsFactors = FALSE)

# Load in wb data
gdp_indic <- "NY.GDP.PCAP.CD"
updated_cache <- wbcache()
gdp_per_capita <- wb(country="countries_only", indicator = gdp_indic, mrv = 20, cache = updated_cache)
gdp_per_capita_wide <- gdp_per_capita %>% 
  spread(
    key = date, 
    value = value
  )
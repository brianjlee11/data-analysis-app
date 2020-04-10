##############################################
#
# Load in all data
#
##############################################
options(scipen = 999)

# Load in press_freedom data
corruption_data <- read.csv("clean_data/corruption.csv", stringsAsFactors = FALSE)

# Load in press_freedom data
raw_press_freedom_index <- read.csv("clean_data/press_freedom_index.csv", stringsAsFactors = FALSE)

# Load in wb data
gdp_per_capita <- read.csv("clean_data/gdp.csv", stringsAsFactors = FALSE)
gdp_per_capita_wide <- gdp_per_capita %>% 
  spread(
    key = date, 
    value = value
  )

GDP_df <- gdp_per_capita

# helper function to make bins for choropleth data 
bin_it <- function(data_vector, bin_size = 10) {
  bins <- seq(0, 100, by = bin_size)
  if (bins[length(bins)] != 100) {
    bins <- c(bins, 100)
  }
  labels <- paste0(bins, " to ", bins[-1])
  labels <- labels[-length(labels)]
  return(cut(data_vector, breaks = bins, labels = labels))
}

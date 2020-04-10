library("dplyr")
library("tidyr")
library("wbstats")
library("ggplot2")
library("stringr")
library("maps")
library("mapproj")
library("scales")

###################################################
#
# Data Extract and Analysis for Press Freedom Index
#
###################################################

# select columns we want to use
press_freedom_index <-  raw_press_freedom_index %>% 
  filter(Indicator == "Press Freedom Index") %>%
  select(-Subindicator.Type)

# create a sample dataframe to display
years_to_filter <- paste0("X",c(2001:2009, 2012))
sample_press_freedom_data <- press_freedom_index %>% top_n(5, X2019) %>% 
  select(-years_to_filter)

press_freedom_long <- press_freedom_index %>% 
  gather(key = date,
         value = pf_index,
         -Country.ISO3,
         -Country.Name,
         -Indicator.Id,
         -Indicator) %>% 
  filter(pf_index < 100)

# rename a couple of columns and clean the year column
colnames(press_freedom_long)[1] <-"iso3c"
colnames(press_freedom_long)[5] <-"year"
press_freedom_long$year <- substr(press_freedom_long$year, 2, 5)

# Create central tendency data by year alone
central_tend_for_press_freedom <- press_freedom_long %>% 
  group_by(year) %>% 
  summarize(average = mean(pf_index, na.rm = TRUE), 
            median = median(pf_index, na.rm = TRUE))

# create central tendency data by year and country
central_tend_for_press_freedom_by_countries <- press_freedom_long %>%
  group_by(iso3c, Country.Name) %>% 
  summarize(average = mean(pf_index, na.rm = TRUE),
            median = median(pf_index, na.rm = TRUE),
            maximum = max(pf_index, na.rm = TRUE),
            minimum = min(pf_index, na.rm = TRUE))

# For UI and Server
colnames(gdp_per_capita)[2] <-"year"
colnames(gdp_per_capita)[3] <-"gdp"
gdp_per_capita$year <- as.character(gdp_per_capita$year)

gdp_w_pfi <- left_join(press_freedom_long, gdp_per_capita, c("iso3c", "year", "Country.Name" = "country")) %>% 
  rename(country = Country.Name) %>% filter(!is.na(gdp))

sum_gdp_w_pfi <- gdp_w_pfi %>% group_by(iso3c, country) %>% 
  summarize(avg_pf = mean(pf_index, na.rm = TRUE), avg_gdp = mean(gdp, na.rm = TRUE))

colnames(gdp_w_pfi)[grep("pf_index", colnames(gdp_w_pfi))] <- "Press Freedom Index"
colnames(gdp_w_pfi)[grep("gdp", colnames(gdp_w_pfi))] <- "GDP per Capita"


# Introduction Tab
introduction_panel <- tabPanel(
  title = "Home",
  titlePanel(title = "Study on Corruption, Freedom of Press, and GDP"),
  h3("Introduction"),
  p("With the current political climate, issues surrounding press freedom and governmental corruption are at an ever
    increasing forefront of peoples' thoughts. In this report, we wanted to analyze how these facets of governments
    can affect everyday people. As such, we are looking at an analyzing data that relates to the corruption levels,
    gdp per capita, and press freedom levels for countries all over the world. In this data set, the CPI is a corruption
    index score for countries given by Transparency International and takes into account various factors for how public
    sector resources are used for private gains. GDP per capita is the gross domestic product per person in a country.
    We are using it as a representation of the economic ramifications on everyday people within a country. Finally,
    the press freedom index is from reporters without borders and represents the ability for media to freely report
    and express to the public. By looking at these various measures, we can approximate effects of these indexes on
    each other to show their impact and indirect impacts on everyday life."),
  tags$p("In our analysis, we used three different data sources."),
  tags$ol(
    tags$li(strong("World Bank GDP per capita")," - Gross Domestic Product per Capita (GDP per Capita) is calculated by dividing a
            country's GDP by their total population, yielding a good measure of a country's economic output with respect
            to population. World Bank's GDP per Capita dataset contains GDP per Capita information on 212 countries spanning
            from 1999-2018. By analyzing this dataset, we can use GDP per Capita as an indicator of the standard of living
            and quality of life of countries around the world.", tags$a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD", "The data can be located here")),
    tags$li(strong("Press Freedom Index"), "- Freedom of the Press is the right for media outlets to freely report and express their
            thoughts to the public, free of influence and pressure from governments or corporations. TCdata360's Press
            Freedom Index attempts to measure levels of press freedom for countries around the world on a 1-100 index, with
            a lower value indicating a more-free press. This index is calculated based on the intensity of abusive and violent
            acts against media outlets, collected by specialists around the world.", tags$a(href = "https://tcdata360.worldbank.org/indicators/h3f86901f?country=BRA&indicator=32416&viz=line_chart&years=2001,2019", "The data can be located here")),
    tags$li(strong("Corruption Perceptions Index")," - The Corruption Perceptions Index aims to measure the levels of public sector and
            government corruption for 180 countries around the word. The dataset comes from Transparency International, an organization
            aiming at ridding the world of corruption, and is calculated by compiling 13 different surveys and expert assessments
            regarding world corruption. Corruption levels are measure on a 1-100 index, with low scores indicating high government
            corruption.", tags$a(href = "https://www.transparency.org/research/cpi/overview", "The data can be located here"))
  ),
  p("We hope that by examining the relationships between these three variables, that we may inspire people to take a closer look at how their
    freedoms may be exploited for economic or political gain, motivating them to learn and be more self-aware of those who are in positions of power."),
  p("This app was created in a collaborative effort by: Michael Grahm, Brian Lee, Thomas Serrano, Derek Wu, and Yifan Wu"),
  p("Our original report may be found", tags$a(href = "https://info201a-wi20.github.io/project-report-brianjlee11/", "here.")),
)

##################
# Q1 Data Wrangle#
##################

q1_country_names <- sum_gdp_w_pfi %>% pull(country)
q1_years <- gdp_w_pfi %>% pull(year) %>% unique()

#########
# Q1 Tab#
#########
q1_mainPanel <- mainPanel(
  DT::dataTableOutput(outputId = "sum_gdp_w_pfi_filtered"),
  plotOutput(outputId = "gdp_pfi_plot"),
  textOutput(outputId = "notify"),
  br(),
  tableOutput(outputId = "q1_cor")
)

q1_sidePanel <- sidebarPanel(
  sliderInput(inputId = "q1_years", label = "years", min = as.numeric(q1_years[1]), 
              max = as.numeric(q1_years[length(q1_years)]),value = q1_years, sep = ""),
  
  selectizeInput(inputId = "q1_country_name", label = "Countries", choices = q1_country_names,
                 options = list(placeholder = 'select country names', maxItems = 175)),
  
  checkboxInput(inputId = "q1_trend_line", label = "Show Trendline", value = TRUE),
  checkboxInput(inputId = "select_all", label = "Add All Countries", value = FALSE)
)

q1_panel <- tabPanel(
  title = "Freedom of Press and GDP",
  titlePanel(title = "Is there correlation between freedom of press and GDP per Capita?"),
  
  p("This question works to find out if there is a correlation between level of freedom of press around 
    the world and GDP per Capita around the world. Freedom of press is measured in Press Freedom Index 
    and GDP (Gross Domestic Product) per Capita is measured in dollars. (See problem domain section for 
    more details on these datasets)"),
  p("For the data analysis, data for GDP per Capita and press freedom index were joined together. Then, 
    I took the press freedom index scores from 2001 to 2018 and compared it to GDP per capita amounts 
    from 2001 to 2018 in order to find if there was a correlation."),
  
  sidebarLayout(
    sidebarPanel = q1_sidePanel,
    mainPanel = q1_mainPanel
  ),
  
  tags$hr(),
  strong(p(textOutput(outputId = "cor_text"))),
  br(),
  p("Taking overall dataset into account, there is a downward trend. As press freedom index increases 
    (freedom of press gets worse), GDP also declines. After the downward trend passes press freedom index of 25,
    it almost flat lines. There is also a strong concentration data that has low GDP and has varying degree of 
    press freedom index. The highest GDP country, Liechtenstein, had GDP of 178845.63 and had press freedom 
    index of 17.67. However, the country with a highest press freedom index, Turkmenistan, had GDP of 1283.91 
    and had press freedom index of 99.83. While these two data points do not justify that there is a correlation,
    it goes to show there is some relationship between press freedom index and GDP per Capita."),
  br(),
  p("Statistical analysis of the correlation shows that the t-value of the correlation was -18.4 and when tested
    for statistical significance, the p-value was less than 0.001 thus the correlation coefficient, which was 
    -0.35, was statistically significant at 0.01 significance level, proving that there is correlation between 
    Press Freedom Index and GDP Per Capita. This supported my assumption as countries with lower GDP would have
    higher restriction on press freedom as most countries with low GDP are developing nations with little freedom
    of press. However, this correlation does not prove that GDP directly affects freedom of press or press of freedom
    affects GDP. There may be other factors at play that affect these values and this analysis cannot 100% prove that
    the question is true."),
  tags$hr()
)

# Q2 Tab
q2_panel <- tabPanel(
  title = "Corruption and GDP",
  titlePanel(title = "How often do countries experience different combinations of CPI Rises and GDP Falls?"), 
  p("This question is important because by using it, we can gage how the CPI changes with changes in GDP. Is a group of countries 
    more likely to have an increase in GDP when their CPI score falls? Do certain countries tend to experience the same change over and over? This is the
    type of question this data exploration wishes to answer."),
  p("This data was aggregated by first, finding the difference in GDP pc per country from one year to the next as well as the difference in CPI score per country from one
    year to the next. Then, we combined the data, so each country has a year with a positive or negative value for CPI and GDP pc showing their rises or falls. These were then 
    converted to categorical variables and counted per year, allowing us to see what countries do what for a given. Countries without both values for a given year will not be included in that year's count."),
  tags$hr(),
  p(strong("The most frequent categories are CPI Rise/Fall and GDP Fall."), "Viewing the total number of CPI/GDP pc rises and falls lets us see that these two
           variables are fairly independent. While it is true that the GDP pc is more likely to fall than rise, the probability that it occurs at the same time as 
           a countries CPI score Rising or Falling is about the same. Retrospectively, this means that countries should be able to commit to increasing their CPI score (making their
    countries less corrupt) as it has no impact on their GDP pc. It is in everyone's best interest to decrease corruption, especially if it does not harm the economy."),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("ascent_descent_switch",
                          label = h3("Show Category"), 
                          choices = list("CPI Fall, GDP Fall" = "fallfall",
                                         "CPI Fall, GDP Rise" = "fallrise",
                                         "CPI Rise, GDP Fall" = "risefall",
                                         "CPI Rise, GDP Rise" = "riserise"),
                          selected = c("fallfall", "fallrise", "risefall", "riserise")
                          ),
      h3("Select Countries"),
      DT::dataTableOutput(outputId = "country_table"),
      actionButton(inputId = "clear", label = "Clear Selection")
      ),
    mainPanel(
      plotOutput(outputId = "stacked_bar", click = "stacked_click"),
      htmlOutput(outputId = "all_year_summary"),
      fluidRow(
        h3(textOutput(outputId = "selected_year")),
        fluidRow(
          column(6,
                 DT::dataTableOutput(outputId = "year_summary")
          ),
          column(6, 
                 plotOutput(outputId = "category_summary"),
                 #INCLUDE TEXTUAL SUMMARY ABOUT WHAT THING OCCURRED THE MOST ON AVERAGE
          ),
        ),
      )
    )
  )
)

# Q3 Tab
#Q3 data wrangle
corruption_data_2 <- corruption_data 
colnames(corruption_data_2)[-1:-3] <- colnames(corruption_data_2)[-1:-3]%>%
  gsub("X", "", .) %>%
  as.numeric()

corruption_long_df <- gather(corruption_data_2, key = Year, value = CPI, -country, -ISO3, -Region,
                             na.rm = TRUE, factor_key = FALSE) %>%
  mutate(Year = as.numeric(Year))

regions <- corruption_data$Region
q3_years <- colnames(corruption_data) %>%
  gsub("X", "", .) %>%
  as.numeric()
  
q3_sidePanel <- sidebarPanel(
  sliderInput(inputId = "q3_years", label = "Years", min = min(q3_years, na.rm = T), 
              max = max(q3_years, na.rm = T), value = q3_years, sep = ""),
  
  selectizeInput(inputId = "q3_region", label = "Regions", choices = regions,
                 options = list(placeholder = 'select region names', maxItems = 10)),
  
  checkboxInput(inputId = "q3_trend_line", label = "Show Trendline", value = TRUE),
  checkboxInput(inputId = "q3_select_all", label = "Add All Regions", value = TRUE)
)

q3_mainPanel <- mainPanel(
  DT::dataTableOutput(outputId = "corruption_filtered"),
  plotOutput(outputId = "corruption_plot"),
  br(),
  h4(p(textOutput(outputId = "q3_cor_text")))

)

q3_panel <- tabPanel(
  title = "Global Corruption Trend",
  titlePanel(title = "How Does Corruption Change Globally over time?"),
  
  p("This question tracks the changes in corruption over time. It takes into account the corruptions for each country
    and tracks them over time. This allows for the tracking of corruption of the globe as a whole, as well as tracking
    by regions to look for localized changes in specific parts of the world."),
  p("The data was first aggregted into the CPI scores for each country, and then divided into each country each year.
    Next this data is compared to the same subsets data from previous years to see how it changes over time. By 
    keeping the categories the same, we can control for facters that would influence the changes in base levels
     for countries as well as see how changes are affecting governments in real time."),
  tags$hr(),
  p("As we can see, over the whole time frame of 2000-2019, the overall change in global corruption is effectively
    zero. The decrease in corruption index from 2000 to 2007 in AME, AP, and MENA compose the global decrease in 
    those years. The recent small increases are coming from the ECA, SSA, AP, and AME regions. 
    In general, the corruption index of the globe remains fairly consistent across time, but varies within regions. 
    Overall, the global corruption seems to oscilate around 43 GPI points."),
  tags$hr(),
  
  sidebarLayout(
    sidebarPanel = q3_sidePanel,
    mainPanel = q3_mainPanel
  )
)

##################
# Q4 Data Wrangle#
##################
q4_country_names <- GDP_df %>% pull(country)
q4_years <- GDP_df %>% pull(date) %>% unique()
q4_values <- GDP_df %>% pull(value) %>% unique()

#########
# Q4 Tab#
#########
q4_mainPanel <- mainPanel(
  
)


q4_panel <- tabPanel(
  title = "GDP and Country's Appearance",
  titlePanel(title = "How well does the GDP per capita reflect a contry's region and size?"),
  p("This question is important because the user can easily find out the relationship between the GDP per capita value and
    a country's region and size by defining a specific GDP per capita range and year. By giving different year and value range, 
    there will be a patterns, such as a specific regions of world always have hightest GDP value, or country with 
    small size always have higher GDP per capita as well."),
  p("For the data analysis, we first collected the each country's GDP per capita in different year,throughout 
    1999-2018.Then we filter the data for different GDP per capita range and get highest and lowest values with
    correpsponding country's nanme."),
  tags$hr(),
  p("It makes sense that countries in Europe are very developed and on the higher ends of the GDP per capita spectrum. They are small
  ]in size and population. Which can generate higher
    GDP per Capita. Comparing to China, eventhough it has the second total GDP in the world, its large population base(1,433,783,686)
    dilutes GDP per Capital value. Therefore, GDP per Capita does not reflect quiet well on a contry's overall presence, but it takes
    a country's economic output, population, and the country's size as a whole thing into account."),
  tags$hr(),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "q4_years", label = "Pick a year", min = min(as.numeric(q4_years)), 
                  max = as.numeric(q4_years[length(q4_years)]),value = 2018, sep = "",step = 1),
      sliderInput(inputId = "q4_values", label = "Define a GDP value range", min = floor(min(as.numeric(q4_values))),
                  max = ceiling(max(as.numeric(q4_values))), value = c(0, max(q4_values, na.rm = TRUE)),sep = "")
    ),
    mainPanel(plotOutput(outputId = "q4_map"),
              htmlOutput(outputId = "q4_summary"))) 
)



###############
# Q5 Wranglin #
###############

# Q5 Tab
country_names <- corruption_data$country
country_input <- selectizeInput(inputId = "country_input", label = "Countries", choices = country_names, multiple = TRUE, selected = c("Russia", "United States of America"))

year_input <- sliderInput(inputId = "year_range", label = "Year", min = 2001, max = 2019, value = c(2001,2019), sep = "")
trendline_input <- checkboxInput(inputId = "show_trend", label = "Show Trendline", value = TRUE)

q5_panel <- tabPanel(
  title = "Corruption and Press Freedom",
  titlePanel(title = "What is the relationship between corruption level and press freedom?"),
  p("By posing this question, we're looking to determine if there is a correlation between a country's level of private sector and government corruption and
    how 'free' their media outlets are. By analyzing the relationship between press freedom and corruption, we can discover
    trends in countries with unusually high or low media censorship rates, as well as bring awareness to media abuse around 
    the world.The datasets we're using in this analysis are the 'Corruption Perceptions Index'
    and the 'Press Freedom Index', both of which use 1-100 indices in their measures, with low numbers indicating high 
    corruption and greater press freedom. (See our problem domain for more details on these datasets)"),
  
  p(strong("For the sake of logic and readability, we inversed CPI values for the following visualizations. This means that in the analysis below,
    a high CPI value equates to high government corruption. ")),
  tags$hr(),
  p(paste("From our own statistical analysis, we believe we've found that private sector corruption and press freedom are strongly correlated with eachother at a
    worldwide level. Looking at various plots, we've found that countries with high cpi trends tend to have high pfi trends as well. The same goes with low trending
    countries. Using average indice values, we calculated a correlation coefficient of 0.5715556 between the corruption and press freedom indices. When testing 
    for significance, the correlation's p-value was less than .001, meaning its coefficient is statistically significant at a level of .01. Although, while private
          sector and government corruption may have a correlation with press freedom levels, this is not neccessarily a causal relationship.")),
  tags$hr(),
  
  
  fluidRow(
    
    column(
      6,
      h4("Select Your Country/Countries"),
      country_input
    ),
    
    column(
      6,
      h4("Select Year Range"),
      year_input
    )
  ),
  trendline_input,
  plotOutput(outputId = "averages_plot"),
  
  mainPanel(
    h3("Summary Statistics"),
    DT::dataTableOutput(outputId = "summary_statistics"),
    p(htmlOutput(outputId = "summary_text")),
    p(htmlOutput(outputId = "q5_correlation_summary"))
  )
)
# The UI is the result of calling the `fluidPage()` layout function
my_ui <- navbarPage(theme = shinytheme("yeti"),
  title = "    ",
  introduction_panel,
  q1_panel,
  q2_panel,
  q3_panel,
  q4_panel,
  q5_panel
)


filtered_cpi_data <- corruption_data %>%
  select(-Region, -X2000) %>% 
  gather(key = year, value = value, -ISO3, -country) %>% 
  filter(value <= 100) %>% 
  mutate(indicator = "cpi", value = 100 - value)

filtered_pfi_data <- raw_press_freedom_index %>%
  filter(Indicator == "Press Freedom Index") %>% 
  rename(country = Country.Name, ISO3 = Country.ISO3) %>% 
  select(-Indicator.Id, -Subindicator.Type, -Indicator) %>% 
  gather(key = year, value = value, -ISO3, -country) %>% 
  filter(value <= 100) %>% 
  mutate(indicator = "pfi")

my_server <- function(input, output) {
  v <- reactiveValues()
  v$clicked_group <- NULL
  v$year_ranges <- year_ranges
  
  observe({
    change_data <- faceted_groups
    if (!is.null(input$country_table_rows_selected)) {
      country_names <- country_dataframe[input$country_table_rows_selected, "country"] %>% 
        pull(country)
      print(country_names)
      change_data <-change_data %>% 
        filter(country %in% country_names)
    }
    if (!("fallfall" %in% input$ascent_descent_switch)){
      change_data <- change_data %>% 
        filter(status != "CPI Fall, GDP Fall")
    }
    if (!("fallrise" %in% input$ascent_descent_switch)){
      change_data <- change_data %>% 
        filter(status != "CPI Fall, GDP Rise")
    }
    if (!("risefall" %in% input$ascent_descent_switch)){
      change_data <- change_data %>% 
        filter(status != "CPI Rise, GDP Fall")
    }
    if (!("riserise" %in% input$ascent_descent_switch)){
      change_data <- change_data %>% 
        filter(status != "CPI Rise, GDP Rise")
    }
    v$plot_data <- change_data
    v$year_ranges <- unique(pull(change_data, year_range)) %>% 
      stringr::str_sort()
    
  })
  
  output$all_year_summary <- renderText({
    year_minmax <- paste(min(v$plot_data$start), max(v$plot_data$stop), sep = "-")
    plot_data_summary <- v$plot_data %>% 
      group_by(status) %>% 
      summarize(total = n())
    max_data <- plot_data_summary %>% 
      filter(total == max(total))
    min_data <- plot_data_summary %>% 
      filter(total == min(total))
    print(plot_data_summary)
    total_points <- sum(plot_data_summary$total)
    
    text_summary <- paste0("In for the years in the range <b>",
                           year_minmax,
                           "</b> there were <b>",
                           total_points, 
                           "</b> different observations of changing GDP per capita and CPI score for the selected countries and categories.",
                           " The most common change was <b> ",
                           max_data$status,
                           "</b> which occurred <b>",
                           round(max_data$total/total_points* 100),
                           "%</b> of the time. The least common change was <b>",
                           max_data$status,
                           "</b> which occurred <b>",
                           round(min_data$total/total_points* 100),
                           "%</b> of the time."
                           )
    return(text_summary)
    
  })
  
  output$stacked_bar <- renderPlot({
    plot <- v$plot_data %>% 
      group_by(status, year_range) %>% 
      summarize(total = n()) %>% 
      ggplot(mapping = aes(x = year_range,y=total, fill = status)) + 
      geom_col(position="fill") + 
      scale_color_brewer(palette = "Set1") +
      labs(title = "Country status changes by percent of total. Click to inspect columns", x ="Year", y = "Percent of available Countries") +
      theme(axis.text.x = element_text(size = 12, angle = 90)) 
    
    return(plot)
  })
  
  output$selected_year <- renderText({
    text <- "Lifetime Summary"
    
    if (!is.null(v$clicked_year)) {
      text <- paste(v$clicked_year, " Summary")
    }
    return(text)
  })
  
  # Q1: Data Table
  output$sum_gdp_w_pfi_filtered <- DT::renderDataTable({
    if (input$select_all) {
      v$filtered_gdp_pfi <- gdp_w_pfi %>% filter(year >= input$q1_years[1] & year <= input$q1_years[2]) %>% 
        select(-Indicator.Id, -Indicator, -iso2c, -indicator, -indicatorID)
    } else {
      v$filtered_gdp_pfi <- gdp_w_pfi %>% filter(country %in% input$q1_country_name & year >= input$q1_years[1]
                                                 & year <= input$q1_years[2]) %>% 
        select(-Indicator.Id, -Indicator, -iso2c, -indicator, -indicatorID)
    }
    return(v$filtered_gdp_pfi) 
  })

  # Q1: Correlation Table
  output$q1_cor <- renderTable({
    if (nrow(v$filtered_gdp_pfi) > 3) {
      q1_cor_test <- 
        cor.test(v$filtered_gdp_pfi$`Press Freedom Index`, v$filtered_gdp_pfi$`GDP per Capita`, use = "complete.obs")
      v$cor_t_value <- round(unname(q1_cor_test$statistic), 2)
      v$cor_p_value <- round(q1_cor_test$p.value, 4)
      v$cor_r_value <- round(unname(q1_cor_test$estimate), 2)
      v$cor_df <- trunc(unname(q1_cor_test$parameter))
      v$cor_table <- data.frame(c(v$cor_t_value, v$cor_p_value, v$cor_r_value, v$cor_df), row.names = c("t Value", "p Value", "Correlation Coefficient", "Degree of Freedom")) %>%   
      rename("Correlation Test" = "c.v.cor_t_value..v.cor_p_value..v.cor_r_value..v.cor_df.")
      return(v$cor_table)
    }
  }, include.rownames=TRUE, width = paste0(100, "%"))
  
  # Q1: Correlation Graph
  output$gdp_pfi_plot <- renderPlot({
    q1_plot <- ggplot(data = v$filtered_gdp_pfi, mapping = aes(x = `Press Freedom Index`, y = `GDP per Capita`)) + 
      geom_point() + 
      ggtitle("Press Freedom Index and GDP per Capita") +
      labs(
        x = "Press Freedom Index",
        y = "GDP per Capita ($)"
      ) + 
      theme(
        plot.title = element_text(size = 10, hjust = 0.5)
      ) + scale_y_continuous(labels = comma)
    
    if(input$q1_trend_line == TRUE) {
      q1_plot <- q1_plot + geom_smooth(method = "lm", se = FALSE, color = "RED")
    }
    
    return(q1_plot)
  })
  
  output$cor_text <- renderText({
    if (nrow(v$filtered_gdp_pfi) > 3) {
      p_value <- v$cor_p_value
      r_value <- v$cor_r_value
      df <- v$cor_df
      significance <- "significant"
      if (r_value < 0.3 & r_value > -0.3) {
        return(paste0("There were no statistically significant correlation between press freedom index 
                      and GDP per capita ", 
                      "r(", df, ")", " = ", r_value, ", p = ", p_value))
      } else if (r_value <= -0.3 & r_value > -0.5) {
        significance <- "weakly negatively correlated"
      } else if (r_value <= -0.5 & r_value > -0.7) {
        significance <- "moderately negatively correlated"
      } else if (r_value <= -0.7 & r_value >= -1.0) {
        significance <- "strongly negatively correlated"
      } else if (r_value <= 0.3 & r_value > 0.5) {
        significance <- "weakly postively correlated"
      } else if (r_value <= 0.5 & r_value > 0.7) {
        significance <- "moderately positively correlated"
      } else if (r_value <= 0.7 & r_value >= 1.0) {
        significance <- "strongly positively correlated"
      } 
      return(paste0("Press Freedom Index and GDP per Capita were found to be ", significance, 
                    ", " , "r(", df, ")", " = ", r_value, ", p = ", p_value))
    }  
  })
  
  output$notify <- renderText({
    if (nrow(v$filtered_gdp_pfi) < 3) {
      return("Increase the number of data to at least 3 points to see correlation information table and summary.")
    } 
  })
  
  output$averages_plot <- renderPlot({
    
    selected_countries <- input$country_input
        selected_isos <- corruption_data %>% 
          filter(country %in% selected_countries) %>% 
          pull(ISO3)
    selected_range <- input$year_range[1]:input$year_range[2]
    
    cpi_pfi_df <- rbind(filtered_cpi_data, filtered_pfi_data)
    
    cpi_pfi_df <- cpi_pfi_df %>% 
      filter(year %in% paste('X', selected_range, sep = "") & ISO3 %in% selected_isos) %>% 
      group_by(country)
    
    the_plot <- ggplot(data = cpi_pfi_df, mapping = aes(x = substr(year, 2, 5), y = value, color = indicator, group = indicator)) +
      geom_point() +
      geom_line() +
      facet_wrap(facets = ~ISO3) +
      labs(x = "Year", y = "Index Value", color = "Index") + 
      theme(axis.text.x = element_text(size = 6, angle = 90)) +
      scale_color_discrete(labels = c("Corruption Perception Index", "Press Freedom Index"))
    
    if(input$show_trend == TRUE) {
      the_plot <- the_plot +
        geom_smooth(se = FALSE)
    }
    
     return(the_plot)
    
  })
  
  output$summary_statistics <- DT::renderDataTable({
    selected_countries <- input$country_input
    selected_isos <- corruption_data %>% 
      filter(country %in% selected_countries) %>% 
      pull(ISO3)
    
    selected_range <- input$year_range[1]:input$year_range[2]
    
    cpi_wide <- filtered_cpi_data %>%
      filter(year %in% paste('X', selected_range, sep = "") & ISO3 %in% selected_isos, value <= 100) %>% 
      spread(indicator, value)
    
    pfi_wide <- filtered_pfi_data %>% 
      filter(year %in% paste('X', selected_range, sep = "") & ISO3 %in% selected_isos, value <= 100) %>% 
      spread(indicator, value) %>% 
      select(-country)
    
    
    cpi_pfi_summary_df <- left_join(cpi_wide, pfi_wide, by = c("ISO3", "year"))
    cpi_pfi_summary_df <- cpi_pfi_summary_df %>% 
      group_by(country)
    
    summary_stats_df <- cpi_pfi_summary_df %>% 
      group_by(country) %>% 
      summarize(
        avg_cpi = mean(cpi, na.rm = TRUE),
        avg_pfi = mean(pfi, na.rm = TRUE),
        max_cpi = max(cpi, na.rm = TRUE),
        min_cpi = min(cpi, na.rm = TRUE),
        max_pfi = max(pfi, na.rm = TRUE),
        min_pfi = min(pfi, na.rm = TRUE)
      ) 
    
    v$summary_stats_df  <- summary_stats_df
    v$cpi_pfi_summary_df <- cpi_pfi_summary_df

    colnames(summary_stats_df) <- c("Country", "Average CPI", "Average PFI", "Max CPI", "Min CPI", "Max PFI", "Min PFI")
    
    return(summary_stats_df)
  })
 
  output$summary_text <- renderText({
    max_avg_cpi <- v$summary_stats_df %>% 
      filter(avg_cpi == max(v$summary_stats_df$avg_cpi))
      
    
    max_avg_pfi <- v$summary_stats_df %>% 
      filter(avg_pfi == max(v$summary_stats_df$avg_pfi))
    
    output_text <- paste("Of the countries selected,", strong(max_avg_cpi$country), "has the most corrupt public sector with an average CPI of",
                         strong(max_avg_cpi$avg_cpi), "over the year range.", strong(max_avg_pfi$country), "has the least-free press with an average PFI of",
                         strong(max_avg_pfi$avg_pfi),".")
    
    return(output_text)
  })
  
  output$q5_correlation_summary <- renderText({
    country_avgs <- v$cpi_pfi_summary_df %>% 
      group_by(country) %>% 
      summarize(avg_cpi = mean(cpi, na.rm = TRUE), avg_pfi = mean(pfi, na.rm = TRUE))
    
    if(nrow(country_avgs) >= 3) {
      selected_correlation_coeff <- cor(country_avgs$avg_cpi, country_avgs$avg_pfi, use = "complete.obs")
      selected_cor_test <- cor.test(country_avgs$avg_cpi, country_avgs$avg_pfi, use = "complete.obs")
      selected_cor_t_value <- selected_cor_test[["statistic"]][["t"]]
      print(selected_cor_test)
      
      output_text <- paste("The correlation coefficient between the corruption and press freedom indices for the selected countries is", strong(selected_correlation_coeff), ".")
      
    } else {
      output_text <- "Correlation analysis requires at least 3 observations."
    }
    
    return(output_text)
  })

  observe({
    if (!is.null(input$stacked_click$x) & !is.null(input$stacked_click$y)){
      xcoord <- input$stacked_click$x 
      if (xcoord > .5) {
        xcoord <- xcoord - .5
      }
      v$clicked_year <- isolate(v$year_ranges[floor(xcoord)+1])
    }
  })
  
  output$year_summary <- DT::renderDataTable({
    desired_countries <- v$plot_data
    cpi_swings <- swing_cpi
    wb_swings <- swing_wb
      
    if (!is.null(v$clicked_year)) {
      desired_countries <- desired_countries %>% 
        filter(year_range == v$clicked_year)
      
      cpi_swings <- cpi_swings %>%  
        filter(year_range == v$clicked_year)
      wb_swings <- wb_swings %>%  
        filter(year_range == v$clicked_year)
    }
      desired_countries <- desired_countries %>% 
      pull(country)
    
    cpi_swings <- cpi_swings %>% 
      filter(country %in% desired_countries)
    cpi_max <- max(cpi_swings$cpi_swing, na.rm=TRUE)
    cpi_min <- min(cpi_swings$cpi_swing, na.rm=TRUE)
    cpi_max_countries <- cpi_swings %>% 
      filter(cpi_swing == cpi_max) %>% 
      pull(country) %>% 
      paste(collapse = ", ")
    cpi_min_countries <- cpi_swings %>% 
      filter(cpi_swing == cpi_min) %>% 
      pull(country) %>% 
      paste(collapse = ", ")
    
    wb_swings <- wb_swings %>% 
      filter(country %in% desired_countries)
    wb_max <- max(wb_swings$wb_swing, na.rm=TRUE)
    wb_min <- min(wb_swings$wb_swing, na.rm=TRUE)
    wb_max_countries <- wb_swings %>% 
      filter(wb_swing == wb_max) %>% 
      pull(country) %>% 
      paste(collapse = ", ")
    wb_min_countries <- wb_swings %>% 
      filter(wb_swing == wb_min) %>% 
      pull(country) %>% 
      paste(collapse = ", ")
    
    values <- c(
      round(cpi_max),
      round(cpi_min),
      round(wb_max),
      round(wb_min)
    )
    
    countries <- c(
      cpi_max_countries,
      cpi_min_countries,
      wb_max_countries,
      wb_min_countries
    )
    
    labels <- c(
      "Max CPI Change",
      "Min CPI Change",
      "Max GDP pc Change",
      "Min GDP pc Change"
    )
    
    return(data.frame(values, countries, row.names = labels))
  })
  
  output$category_summary <- renderPlot({
    data <- v$plot_data
    if (!is.null(v$clicked_year)) {
      data <- data %>% 
      filter(year_range == v$clicked_year)
    }
    data <- data %>% 
      group_by(status) %>% 
      summarize(total = n())
    
    data <- data %>% 
      ggplot() +
      geom_col(mapping = aes(x = status, y = total, fill = status)) +
      geom_text(mapping = aes(x = status, y = total, label = total))
    return(data)
  })
  
  output$country_table <- DT::renderDataTable({
    return(DT::datatable(country_dataframe, options = list()))
  })
  
  observeEvent(input$clear, {
    proxy <- DT::dataTableProxy(outputId = "country_table")
    DT::selectRows(proxy, NULL)
  })
  
  # Q3: Data Table
  output$corruption_filtered <- DT::renderDataTable({
    if (input$q3_select_all) {
      v$filtered_corruption <- corruption_long_df %>% 
        filter(Year >= input$q3_years[1] & Year <= input$q3_years[2])
    } else {
      v$filtered_corruption <- corruption_long_df %>% 
        filter(Region %in% input$q3_region & Year >= input$q3_years[1]
                                                 & Year <= input$q3_years[2])
    }
    return(v$filtered_corruption) 
  })
  
  # Q3: Correlation Graph for corruption
  output$corruption_plot <- renderPlot({
    q3_plot <- ggplot(data = v$filtered_corruption, mapping = aes(x = `Year`, y = `CPI`)) + 
      geom_point() + 
      ggtitle("CPI per year in", input$q3_region) +
      labs(
        x = "Year",
        y = "Corruption Perceptions Index"
      ) + 
      theme(
        plot.title = element_text(size = 10, hjust = 0.5)
      ) + scale_y_continuous(labels = comma)
    
    if(input$q3_trend_line == TRUE) {
      q3_plot <- q3_plot + geom_smooth(method = "lm", se = FALSE, color = "RED")
    }
    return(q3_plot)
  })
  
  #Q3: Correlations text
  output$q3_cor_text <- renderText({
    if (nrow(v$filtered_corruption) > 3) {
      v$q3_cor_test <- cor(v$filtered_corruption$Year, v$filtered_corruption$CPI)
      write_region <- "all regions"
      if(length(input$q3_region) < 1) {
        write_region <- input$q3_region
      }
      #p_value <- v$q3_cor_p_value
      r_value <- v$q3_cor_test
      df <- v$q3_cor_df
      significance <- "significant"
      if (r_value < 0.3 & r_value > -0.3) {
        return(paste0("There were no statistically significant correlation between Year 
                      and CPI between the years ", input$q3_years[1], " and ", input$q3_years[2], " in the region(s) ",
                      write_region, ". The r value is: ", r_value))
      } else if (r_value <= -0.3 & r_value > -0.5) {
        significance <- "weakly negatively correlated"
      } else if (r_value <= -0.5 & r_value > -0.7) {
        significance <- "moderately negatively correlated"
      } else if (r_value <= -0.7 & r_value >= -1.0) {
        significance <- "strongly negatively correlated"
      } else if (r_value <= 0.3 & r_value > 0.5) {
        significance <- "weakly postively correlated"
      } else if (r_value <= 0.5 & r_value > 0.7) {
        significance <- "moderately positively correlated"
      } else if (r_value <= 0.7 & r_value >= 1.0) {
        significance <- "strongly positively correlated"
      } 
      return(paste0("CPI and Year were found to be ", significance, 
                    "between the years ", input$q3_years[1], " and ", input$q3_years[2], " in the region(s)", write_region,
                    " the r value is: ", r_value))
      }  
  })

  output$q4_map <- renderPlot ({
  countries_map <- map_data("world") %>% 
  mutate(iso3c = iso.alpha(region, n=3))
  
  q4_plot <- GDP_df %>% 
    filter(date == input$q4_years) %>% 
    filter(value <= input$q4_values[2] & value>=input$q4_values[1]) 
  print(q4_plot)
  v$q4_plot <- q4_plot
  
  choropleth_map <- left_join(countries_map,q4_plot, by = "iso3c") %>% 
  ggplot() +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = value))+
    scale_fill_distiller(palette = "RdYlGn")+
    labs(title = "World GDP per Capita 1999-2018")+
    coord_quickmap()+
    theme_void()
  q4_plot <- input$q4_years
  return(choropleth_map)   
 })
  output$q4_summary <- renderText({
   q4_country_highest <- v$q4_plot %>% 
     filter(value == max(value))
  
   q4_country_lowest <- v$q4_plot %>% 
     filter(value == min(value)) 
    
    text_highest_GDP <- paste(q4_country_highest$country, "has the highest GDP value of",  round(q4_country_highest$value,2))
    text_lowest_GDP <- paste(q4_country_lowest$country, "has the lowest GDP value of" , round(q4_country_lowest$value,2))
    return(paste(text_highest_GDP, "<br>", text_lowest_GDP))
 })
}

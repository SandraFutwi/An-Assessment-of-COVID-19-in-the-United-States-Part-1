# An-Assessment-of-COVID-19-in-the-United-States-Part-1

#' **importing libraries**
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("tidyverse")
library(reshape2)
library(readr)
library(knitr)
library(kableExtra)
#'
#'
#'
#'
#'
#'**Reading the file**
setwd("C:\\Users\******\Project 1")
cases <- read_csv("COVID-19_cases_plus_census.csv")
cases
#'
#'
#' **Make character factors for analysis**
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
#'
#' **Filtering Texas**
cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)
summary(cases_TX[,1:10])
#'
cases_TX_filtered <- cases_TX
#' **Listing the name of All Columns**
view(colnames(cases_TX))
column_types <- data.frame(Column = colnames(cases_TX), Type = sapply(cases_TX, class))
print(column_types)
#'
columns_of_interest <- c("bachelors_degree", "bachelors_degree_2", "not_hispanic_pop", 
                         "hispanic_pop", "black_pop", "white_pop", "median_age", 
                         "two_cars", "commuters_by_bus")
sapply(cases_TX[, columns_of_interest], class)
#
#' *Finding Missing Values*
missing_values <- colSums(is.na(cases))
missing_data <- missing_values[missing_values > 0]
missing_data_table <- data.frame(
  Attribute = names(missing_data),
  Missing_Values = missing_data,
  stringsAsFactors = FALSE
)
row.names(missing_data_table) <- NULL
knitr::kable(missing_data_table, col.names = c("Attributes with Missing Values", "Number of Missing Values"))
#' *Finding the duplicated values*
duplicated(names(cases))
#'
#'
#'
#'
#'
#'
#' **BOX PlOtS**
#' *Figire 7: Total Confirmed Cases*
boxplot(cases$confirmed_cases, 
        main = "Total confirmed cases by county", 
        ylab = "Confirmed Cases", 
        col = "red", 
        outline = TRUE)
#'
#'
#'
#'
#' *Figure 8: Total Death by Country*
boxplot(cases$deaths, 
        main = "Total deaths by county", 
        ylab = "Deaths", 
        col = "red", 
        outline = TRUE)
#'
#'
#'
#'
#'
#' **Figure9 : Categorizing the counties based on their population**
cases_TX_filtered <- cases_TX
cases_TX_filtered <- cases_TX_filtered %>%
  mutate(confirmed_cases_per_1000 = (confirmed_cases / total_pop) * 1000)
cases_TX_filtered <- cases_TX_filtered %>%
  mutate(population_category = cut(total_pop, 
                                   breaks = quantile(total_pop, probs = seq(0, 1, by = 0.25), na.rm = TRUE), 
                                   labels = c("Low Population", "Medium Population", "High Population", "Very High Population"),
                                   include.lowest = TRUE))
ggplot(cases_TX_filtered, aes(x = population_category, y = confirmed_cases_per_1000, fill = population_category)) +
  geom_boxplot(outlier.colour = "black", outlier.size = 2) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "red") +  # Highlight the mean
  geom_text(stat = 'count', aes(label = scales::percent(..count../sum(..count..))), 
            y = max(cases_TX_filtered$confirmed_cases_per_1000) + 10, vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "Confirmed COVID-19 Cases per 1000 People by Population Category",
    x = "Population Category",
    y = "Confirmed Cases per 1000 People"
  ) +
  scale_fill_manual(values = c("red", "green", "blue", "orange")) +
  theme_minimal()
#'
#'
#'
#'
#'
#' **Figure 10: atagorizing the countes based on their covid deaths**
cases_TX_filtered <- cases_TX_filtered %>%
  mutate(deaths_per_1000 = (deaths / total_pop) * 1000)
ggplot(cases_TX_filtered, aes(x = population_category, y = deaths_per_1000, fill = population_category)) +
  geom_boxplot(outlier.colour = "black", outlier.size = 2) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "red") +  # Highlight the mean
  geom_text(stat = 'count', aes(label = scales::percent(..count../sum(..count..))), 
            y = max(cases_TX_filtered$deaths_per_1000) + 0.5, vjust = -0.5, color = "black", size = 5) +
  labs(
    title = "COVID-19 Deaths per 1000 People by Population Category",
    x = "Population Category",
    y = "Deaths per 1000 People"
  ) +
  scale_fill_manual(values = c("red", "green", "blue", "orange")) +
  theme_minimal()
#'
#
#'
#'
#'
#' **Figure 11: calculating the deathrate in all counties**
cases_TX <- cases_TX %>%
  mutate(death_rate = deaths / confirmed_cases)
cases_TX <- cases_TX %>%
  mutate(death_rate_category = cut(
    death_rate,
    breaks = quantile(death_rate, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
    labels = c("Very Low", "Low", "Medium", "High"),
    include.lowest = TRUE
  ))
ggplot(cases_TX, aes(x = death_rate_category, y = confirmed_cases)) +
  geom_boxplot(aes(fill = death_rate_category)) +
  labs(
    title = "Confirmed Cases by Death Rate Category",
    x = "Death Rate Category",
    y = "Confirmed Cases"
  ) +
  theme_minimal()
#'
#'
#'
#'
#'
#'
#'
#' **Figure 25: the clustering of median income and confirmed cases per 1000**
income_data <- cases_TX_filtered %>% select(median_income, confirmed_cases_per_1000)
wss <- sapply(1:7, function(k) {
  kmeans(income_data, centers = k, nstart = 20)$tot.withinss  # Total within-cluster sum of squares
})
plot(1:7, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal K")
set.seed(123)  # Set seed for reproducibility
k_optimal <- 3  # Use the optimal k based on the elbow plot
kmeans_result <- kmeans(income_data, centers = k_optimal, nstart = 20)
cases_TX_filtered$cluster <- kmeans_result$cluster
library(ggrepel)
ggplot(cases_TX_filtered, aes(x = median_income, y = confirmed_cases_per_1000, color = factor(cluster))) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = county_name), size = 3, max.overlaps = 10) +  # Add county names
  labs(
    title = "Clustering of Median Income and Confirmed Cases per 1000 in Selected Texas Counties",
    x = "Median Income",
    y = "Confirmed Cases per 1000",
    color = "Cluster"
  ) +
  theme_minimal()
#'
#'
#'
#'
#'
#'
#' **Figure 13**
colnames(cases) <- gsub("^total_pop_", "total_population_", colnames(cases))
colnames(cases)
str(cases)
#'
ggplot(cases_TX, mapping = aes(confirmed_cases)) + 
  geom_histogram(bins = 20) + 
  labs(x = "Confirmed Cases of Covid 19 in Texas Counties", 
       y = "Count of Counties") +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  theme(
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold")   # Bold y-axis label
  )
#'
#'
#'
#'**Figure 14**
ggplot(cases_TX, mapping = aes(deaths)) + 
  geom_histogram(bins = 20) + 
  labs(x = "Deaths in Covid 19 in Texas Counties", 
       y = "Count of Counties") +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  theme(
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold"),  # Bold y-axis label
    plot.caption = element_text(hjust = 0.5, face = "italic")  # Center caption and italicize
  ) 
#'
#'
#'
#'
#'
#' ** Finding which counties have more than 100'000 Confirmed Cases**
cases_TX_filtered <- cases_TX %>%
  filter(confirmed_cases > 100000)
view(cases_TX_filtered)
#'
#'
#'
#'
#'
#'
#' ** Finding Cases death per 1000 population**
cases_TX_filtered <- data.frame(
  county_name = c("Tarrant County", "Bexar County", "Dallas County", "El Paso County", "Harris County"),
  total_pop = c(1983675, 1892004, 2552213, 834825, 4525519),
  confirmed_cases = c(195518, 152231, 234625, 107552, 286356),
  deaths = c(1798, 2040, 2453, 1940, 3825)
)
cases_TX_filtered$cases_per_1000 <- cases_TX_filtered$confirmed_cases / cases_TX_filtered$total_pop * 1000
cases_TX_filtered$deaths_per_1000 <- cases_TX_filtered$deaths / cases_TX_filtered$total_pop * 1000
cases_TX_filtered$death_rate_per_case <- cases_TX_filtered$deaths / cases_TX_filtered$confirmed_cases * 100  # As a percentage
cases_TX_filtered
#'
#'
#'
#'
#'
#'
#'
#' ** Figure 15: Drawing a graph on the data**
cases_long <- gather(cases_TX_filtered, key = "Metric", value = "Value", cases_per_1000, deaths_per_1000, death_rate_per_case)
ggplot(cases_long, aes(x = county_name, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Cases and Deaths in Texas Counties",
       x = "County", y = "Value", fill = "Metric") +
  scale_fill_manual(values = c("grey", "#DDCC77", "#CC6677")) +
  theme_minimal()
#'
#'
#'
#' ** Figure 16: What is the relationship between cases and deaths?**
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, size = total_pop)) + 
  geom_point() +
  labs(
    x = "Confirmed Cases of Covid 19 in Texas Counties", 
    y = "Deaths in Texas Counties"
  ) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
  theme(
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold")   # Bold y-axis label
  )
#'
#'
#'
#'
#'
#'
#'
#'**Figure 17**
ggplot(cases_TX, mapping = aes(x = confirmed_cases, y = deaths, label = county_name)) + 
  geom_smooth(method = lm) +  # Add a trendline
  geom_point(mapping = aes(size = total_pop), color = "grey") +  
  geom_text_repel(data = subset(cases_TX, deaths >= 1000)) +  
  scale_x_continuous(labels = scales::label_number(accuracy = 1)) + 
  labs(x = "Confirmed Cases of Covid-19 in Texas Counties", 
       size = "Total Population")
#'
#'
#'
#'
#'
#' **Figure 19**
cases_TX_select <- cases_TX %>%
  filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income) %>%
  mutate(
    cases_per_1000 = confirmed_cases / total_pop * 1000, 
    deaths_per_1000 = deaths / total_pop * 1000, 
    death_per_case = deaths / confirmed_cases
  )
ggplot(cases_TX_select, mapping = aes(x = cases_per_1000, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95))) +
  labs(x = "confirmed cases", y = "deaths per 1000", size = "total population")
#'
#'
#' *Summing up the total population, confirmed cases, and deaths for Texas*
#' **and Scaling down the values for better visibility (dividing by 1000)**
total_population_tx <- sum(cases_TX$total_pop, na.rm = TRUE)
total_confirmed_cases_tx <- sum(cases_TX$confirmed_cases, na.rm = TRUE)
total_deaths_tx <- sum(cases_TX$deaths, na.rm = TRUE)
scaled_population_tx <- total_population_tx / 1000
scaled_confirmed_cases_tx <- total_confirmed_cases_tx / 1000
scaled_deaths_tx <- total_deaths_tx / 1000
data_ratios <- data.frame(
  Category = c("Population (in thousands)", "Confirmed Cases (in thousands)", "Deaths (in thousands)"),
  Total = c(scaled_population_tx, scaled_confirmed_cases_tx, scaled_deaths_tx)
)
data_ratios$Percentage <- round((data_ratios$Total / sum(data_ratios$Total)) * 100, 2)
ggplot(data_ratios, aes(x = "", y = Total, fill = Category)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Comparison of Scaled Population, COVID-19 Cases, and Deaths in Texas") +
  theme_void() +  # Remove background and axes
  scale_fill_manual(values = c("#E5F", "yellow", "blue")) +  # Custom colors
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5))  # Add percentage labels
#'
#'
cases_TX_select <- cases_TX %>% filter(confirmed_cases > 100) %>% 
  arrange(desc(confirmed_cases)) %>%    
  select(county_name, confirmed_cases, deaths, total_pop, median_income)
cases_TX_select <- cases_TX_select %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases)
head(cases_TX_select)
#'
#
#'*Very interesting table*
datatable(cases_TX_select) %>% formatRound(6:7, 4) %>% formatPercentage(8, 2)
#'
#'**Figure 18: population to death per case**
ggplot(cases_TX_select, mapping = aes(x= total_pop, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point(mapping = aes(size = total_pop), color = "grey") + 
  geom_text_repel(data = subset(cases_TX_select, deaths_per_1000 > quantile(deaths_per_1000, .95))) +
  labs(x = "confirmed cases", y = "deaths per 1000", size = "total population")
#'
#'
#'
#'
#'
#'
#' **Figure 20: Plot the relationship between median income and deaths per 1000**
lowest_income <- cases_TX_select %>% filter(median_income == min(median_income, na.rm = TRUE))
highest_income <- cases_TX_select %>% filter(median_income == max(median_income, na.rm = TRUE))
ggplot(cases_TX_select, mapping = aes(x = median_income, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point() + 
  geom_text_repel(data = rbind(lowest_income, highest_income), aes(label = county_name)) +
  labs(title = "Relationship between Median Income and Deaths per 1000 in Texas Counties",
       x = "Median Income", y = "Deaths per 1000") +
  theme_minimal()
#'
#'
#' **Figure 21: Median Income and Death Rate Across All Counties**
brooks_county <- cases_TX_select %>% filter(county_name == "Brooks County")
fort_bend_county <- cases_TX_select %>% filter(county_name == "Fort Bend County")
income_range_brooks <- 5000 
income_range_fort_bend <- 10000
near_brooks <- cases_TX_select %>% filter(abs(median_income - brooks_county$median_income) < income_range_brooks)
near_fort_bend <- cases_TX_select %>% filter(abs(median_income - fort_bend_county$median_income) < income_range_fort_bend)
highlighted_counties <- rbind(brooks_county, fort_bend_county, near_brooks, near_fort_bend)
ggplot(cases_TX_select, mapping = aes(x = median_income, y = deaths_per_1000, label = county_name)) + 
  geom_smooth(method = lm) +
  geom_point() + 
  geom_text_repel(data = highlighted_counties, aes(label = county_name)) +  
  labs(title = "Relationship between Median Income and Deaths per 1000 in Texas Counties",
       x = "Median Income", y = "Deaths per 1000") +
  theme_minimal()
#'
#'
#
#' **Figure 22: What Variables Are Correlated**
cor_TX <- cor(cases_TX_select[,-1])
ggcorrplot(cor_TX, p.mat = cor_pmat(cases_TX_select[,-1]), insig = "blank", hc.order = TRUE)
#'
colnames(cases)
#' **Going deeper into other variables for Texas**
columns_of_interest <- c("bachelors_degree", "bachelors_degree_2", "not_hispanic_pop", 
                         "hispanic_pop", "black_pop", "white_pop", "median_age", 
                         "two_cars", "commuters_by_bus", "deaths_per_1000")
cases_TX_filtered <- cases_TX_select[, columns_of_interest]
cor_matrix <- cor(cases_TX_filtered, use = "complete.obs")
corrplot(cor_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(200),
         addCoef.col = "black", tl.col = "black", tl.srt = 45, title = "Correlation Matrix", mar = c(0,0,2,0))
#'
#'
#'
#'
#'
#'
#' **Calculate correlations between the selected columns and deaths_per_1000**
cases_TX$deaths_per_1000 <- cases_TX$deaths / cases_TX$total_pop * 1000
correlation_matrix <- cor(cases_TX[, c(columns_of_interest, "deaths_per_1000")], use = "complete.obs")
print(correlation_matrix)
#'
#'
#'
#'
#'
#' **Figure 23 - Relationship Between Education Level (Bachelors Degrees) and Deaths per 1000 in Selected Texas Counties**
selected_counties <- c("Brooks County", "Fort Bend County", "Dallas County", "El Paso County", "Cameron County", "Harris County", "Rockwall County", "Collin County")
cases_TX_filtered <- cases_TX %>% filter(county_name %in% selected_counties)
# Create the plot comparing median_age with deaths_per_1000
cases_TX_filtered <- cases_TX_filtered %>%
  mutate(education_combined = bachelors_degree_2 + bachelors_degree)
ggplot(cases_TX_filtered, aes(x = education_combined, y = deaths_per_1000, label = county_name)) +
  geom_point(color = "black", size = 3) +
  geom_text_repel() +
  labs(title = "Relationship Between Education Level (Bachelors Degrees) and Deaths per 1000 in Selected Texas Counties",
       x = "Bachelors Degree",
       y = "Deaths per 1000") +
  theme_minimal()
#'
#'
#'
#'
#' **Figure 24 - Bachelors Degree & Income Relationship**
cases_TX_filtered <- cases_TX %>%
  filter(county_name %in% selected_counties) %>%
  mutate(
    education_combined_per_1000 = (bachelors_degree_2 + bachelors_degree) / total_pop * 1000,  # Education per 1000
    median_income_per_1000 = median_income / total_pop * 1000  # Median income per 1000 (optional)
  )
# Plot comparing education level per 1000 people with median income
ggplot(cases_TX_filtered, aes(x = education_combined_per_1000, y = median_income, label = county_name)) +
  geom_point(color = "black", size = 3) +
  geom_text_repel() +
  labs(
    title = "Relationship Between Education Level (Bachelors Degrees) Per 1000 People and Median Income in Selected Texas Counties",
    x = "Bachelors Degree (per 1000 people)",
    y = "Median Income"
  ) +
  theme_minimal()
#' 
#' 
#' 
#'
#'
#' **figure 27: Clustering commutters by bus using K-means**
cases_TX <- cases_TX %>%
  mutate(commuters_by_bus_per_1000 = (commuters_by_bus / total_pop) * 1000)

kmeans_data <- cases_TX %>%
  select(commuters_by_bus_per_1000, median_income) %>%
  drop_na() 
kmeans_data_scaled <- scale(kmeans_data)
set.seed(123)
kmeans_result <- kmeans(kmeans_data_scaled, centers = 3)
cases_TX$cluster <- factor(kmeans_result$cluster)
ggplot(cases_TX, aes(x = commuters_by_bus_per_1000, y = median_income, color = cluster)) +
  geom_point(size = 3) +  # Scatter plot of points colored by cluster
  labs(
    title = "K-means Clustering of Public Transportation Usage and Median Income",
    x = "Commuters by Bus per 1000 People",
    y = "Median Income",
    color = "Cluster"
  ) +
  theme_minimal()
#' 
#' 
#' 
#' 
#' #' ** Figure 28: Relationship between public transportation usage and confirmed cases per 1000 in selected counties**
cases_TX_filtered <- cases_TX_filtered %>%
  mutate(confirmed_cases_per_1000 = (confirmed_cases / total_pop) * 1000)
cases_TX_filtered <- cases_TX_filtered %>%
  mutate(density = dnorm(confirmed_cases_per_1000) * dnorm(commuters_by_bus_per_1000))
ggplot(cases_TX_filtered, aes(x = commuters_by_bus_per_1000, y = confirmed_cases_per_1000, label = county_name)) +
  geom_point(aes(size = density), color = "red") +  # Size based on density
  geom_text_repel() +  # Add labels to the counties
  scale_size_continuous(range = c(2, 10), guide = "none") +  # Control the point size range
  labs(
    title = "Relationship Between Public Transportation Usage and Confirmed Cases per 1000 in Selected Counties",
    x = "Commuters by Bus per 1000 People",
    y = "Confirmed Cases per 1000 People"
  ) +
  theme_minimal()
#' 
#' 
#' 
#' 
#' 
#' 
#' ** Figure 30: Which County has the highest black and white population** 
highest_black_per_1000 <- cases_TX_filtered %>%
  arrange(desc(black_pop_per_1000)) %>%
  select(county_name, black_pop_per_1000) %>%
  slice(1)
print(highest_black_per_1000)
highest_white_per_1000 <- cases_TX_filtered %>%
  arrange(desc(white_pop_per_1000)) %>%
  select(county_name, white_pop_per_1000) %>%
  slice(1)
print(highest_white_per_1000)
#'
#'
#'
selected_counties_income <- cases_TX_filtered %>%
  filter(county_name %in% c("Dallas County", "Rockwall County")) %>%
  select(county_name, median_income, black_pop_per_1000, white_pop_per_1000)
print(selected_counties_income)
selected_counties_income_long <- gather(selected_counties_income, key = "Population_Type", value = "Population_Value", black_pop_per_1000, white_pop_per_1000)
ggplot(selected_counties_income_long, aes(x = Population_Type, y = median_income, fill = Population_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~county_name, scales = "free_x") +  # Create separate plots for each county
  labs(
    title = "Comparison of Median Income for White and Black Populations in Dallas and Rockwall Counties",
    x = "Population Type",
    y = "Median Income",
    fill = "Population Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#'
#'
#'
#'
#'
#' **Figure 29: Calculate population percentages and confirmed cases per 100 people**
cases_TX <- cases_TX %>%
  mutate(black_pop_percent = (black_pop / total_pop) * 100,
         white_pop_percent = (white_pop / total_pop) * 100,
         hispanic_pop_percent = (hispanic_pop / total_pop) * 100,
         confirmed_cases_per_100 = (confirmed_cases / total_pop) * 100)  
cases_long <- cases_TX %>%
  select(confirmed_cases_per_100, black_pop_percent, white_pop_percent, hispanic_pop_percent) %>%
  gather(key = "Population_Group", value = "Population_Percent", -confirmed_cases_per_100)
ggplot(cases_long, aes(x = Population_Percent, y = confirmed_cases_per_100, color = Population_Group)) +
  geom_line(size = 1) + 
  labs(title = "Comparison of Black, White, and Hispanic Population Percentages with COVID-19 Cases in TX",
       x = "Population Percentage (%)", 
       y = "Confirmed COVID-19 Cases per 100 People",
       color = "Population Group") +
  theme_minimal()
#'
#'
#'
#'
#' **Figure 31: Put the data on the map**
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))
cases_TX <- cases_TX_select %>% mutate(county = county_name %>% str_to_lower() %>% 
                                         str_replace('\\s+county\\s*$', ''))
counties_TX <- counties_TX %>% left_join(cases_TX %>% 
                                           select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))
ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  # geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  #    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", 
       subtitle = "Only counties reporting 100+ cases",
       fill = "cases per 1000")
#'
#'
#'
#' **Figure 24: Look at Dallas County over time. Are we flattening the curve?**
cases_TX <- read_csv("COVID-19_cases_TX.csv")
cases_TX
cases_Dallas <- cases_TX %>% filter(county_name == "Dallas County" & state == "TX")
dim(cases_Dallas)
ggplot(cases_Dallas, aes(x = date, y = confirmed_cases)) + 
  geom_line() + 
  geom_smooth() +
  labs(y = "confirmed cases")
#'
#'
#'
#' **Figure 34: Deaths Per Day in Texas vs Confirmed cases Spikes Relationship**
cases_Texas <- cases_TX %>%
  filter(state == "TX") %>%
  group_by(date) %>%
  summarise(
    confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE)
  ) %>%
  arrange(date) %>%
  mutate(new_cases_per_day = confirmed_cases - lag(confirmed_cases, default = 0),
         new_deaths_per_day = deaths - lag(deaths, default = 0))
ggplot(cases_Texas, aes(x = date)) + 
  geom_line(aes(y = new_cases_per_day), color = "black", linetype = "solid") + 
  #geom_smooth(aes(y = new_cases_per_day), method = "loess", color = "blue") + 
  geom_line(aes(y = new_deaths_per_day * 10), color = "red", linetype = "solid", size=0.8) +  # Scale by 10
  #geom_smooth(aes(y = new_deaths_per_day * 10), method = "loess", color = "orange", linetype = "dashed") +
  labs(
    title = "New Daily COVID-19 Cases and Deaths in Texas (Deaths Scaled)",
    x = "Date", 
    y = "Count (Deaths scaled by 10)",
    color = "Legend"
  ) +
  theme_minimal()
#'
#'
#'
#'
#' **Figure 33: Deaths Per Day in Dallas Country vs Confirmed cases Spikes Relationship**
cases_Dallas <- cases_TX %>%
  filter(county_name == "Dallas County" & state == "TX") %>%
  arrange(date) %>%
  mutate(new_cases_per_day = confirmed_cases - lag(confirmed_cases, default = 0),
         new_deaths_per_day = deaths - lag(deaths, default = 0))
ggplot(cases_Dallas, aes(x = date)) + 
  geom_line(aes(y = new_cases_per_day), color = "black", linetype = "solid") + 
  geom_line(aes(y = new_deaths_per_day * 10), color = "red", linetype = "solid", size = 0.8) +
  labs(
    title = "New Daily COVID-19 Cases and Deaths in Dallas County (Deaths Scaled)",
    x = "Date", 
    y = "Count (Deaths scaled by 10)",
    color = "Legend"
  ) +
  theme_minimal()
#'
#'


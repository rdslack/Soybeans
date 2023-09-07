#
# R Script for Climate Change vs. Soybeans Yield Analysis
#

#
# Set up environment by calling required libraries
library(bigrquery)# Library to connect to BigQuery
library(dplyr)
library(ggplot2)  
library(sf) # Library required to create map plots
library(ggpubr) # Library for displaying adjacent plots
library(tidyverse)
library(mice) # Library for data imputation


## Script to connect to Big Query from R Studio desktop

# Initiate Connection: required arguments:
# 1. project id, data set name, project id to which query billed
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "xxxxx",
  dataset = "soybeans_project",
  billing = "xxxxx"
)


###
# SOYBEAN PRODUCTION DATA
###

#
# Top 10 Crops Globally (FAO)
top10_crops_2021.df <- read.csv("top10_crops_2021.csv") %>% 
  arrange(desc(billion_tons))


# Global Soybean Production Pct 2022 (USDA)
#
# Top 5 Soy Countries
top5_countries <- c('Brazil', 'United States', 'China', 'Argentina', 'India')

# Pct of Soy Production
top5_countries_pct <- c(42, 31, 7, 5, 3)

# Create Top 5 Soy Countries df
top5_countries.df <- data.frame(top5_countries, top5_countries_pct) %>% 
  arrange(desc(top5_countries_pct))


# Use dplyr to connect to soybean production table in Big Query
con_soy_yield <- tbl(con_bq, 'prod_yield_top11_states')

# TOTAL YIELD DATA
total_soy_yield <- select(con_soy_yield, year, state, measure, quantity) %>%
  collect()


# Use dplyr to connect to top 11 soybean producing states table in Big Query
con_soy_pct <- tbl(con_bq, 'top_11_soy_states')

# TOP11 PCT TOTAL US SOY PRODUCTION 2000-2022
total_top11_soy_pct <- select(con_soy_pct, state, st_abv, pct_total_bushels) %>%
  collect()


# ANNUAL YIELD PER ACRE - COMBINED STATES
# create df of soybeans bushel per acre for Combined States for year 2000-2022
agg_soy_yield <- total_soy_yield %>%
  filter(measure == "bushels_acre") %>%
  group_by(year) %>% 
  summarise(mean_bu_ac_yr = round(mean(quantity), 1)) 


###
# SOYBEAN PRICES
###

# Use dplyr to create link soybeans prices table in Big Query
con_soy_prices <- tbl(con_bq, 'soybean_prices_monthly_top11_states')


# TOTAL DATA for SOYBEANS PRICE
total_soy_price <-  select(con_soy_prices, price_period, state, st_abv, usd_bushel) %>% 
  collect()


# ANNUAL MEAN PRICE - COMBINED STATES
# Create df of USD per bushel for Top11 Soybeans States for year 2000-2022
agg_soy_price <- total_soy_price %>% 
  mutate(year = year(price_period)) %>% 
  group_by(year) %>% 
  summarise(mean_price = round(mean(usd_bushel),2))


###
# WEATHER DATA
###

# Use dply to connect to Top 11 States weather table in Big Query
con_weather <- tbl(con_bq, 'weather_top11_states_nulls')


# TOTAL WEATHER DATA
#### ALREADY CREATED DF, SO COMMENTING TO AVOID RUNNING AGAIN AND INCURRING CHARGES
#Create df for weather data for Top11 Soybeans States for 2000-2022
total_weather_data <- select(con_weather, station, weather_date, location, state,
                            st_abv, mean_temp, max_temp, min_temp, precip) %>%
 collect()


#
# IMPUTE NULL WEATHER DATA USING MICE
#

# Initiate MICE imputation
imputed_total_weather_data <- mice(total_weather_data, method = "pmm", m = 5)

# Extract Imputed Dataframe
imputed_total_weather_data <- complete(imputed_total_weather_data, action = "long")



## ANNUAL AVERAGE MEAN TEMP - COMBINED STATES
# Create df of ANNUAL AVERAGE MEAN TEMP for Combined States for year 2000-2022
agg_mean_temp_yr <- imputed_total_weather_data %>% 
  mutate(year = year(weather_date)) %>% 
  group_by(year) %>% 
  summarise(mean_temp_yr = round(mean(mean_temp),2))


## ANNUAL AVERAGE MAX TEMP - COMBINED STATES
# Create df of ANNUAL AVERAGE MAX TEMP for Combined States for year 2000-2022
agg_max_temp_yr <- imputed_total_weather_data %>% 
  mutate(year = year(weather_date)) %>% 
  group_by(year) %>% 
  summarise(avg_max_temp_yr = round(mean(max_temp),2))


## ANNUAL AVERAGE MIN TEMP - COMBINED STATES
# Create df of ANNUAL AVERAGE MIN TEMP for Combined States for year 2000-2022
agg_min_temp_yr <- imputed_total_weather_data %>% 
  mutate(year = year(weather_date)) %>% 
  group_by(year) %>% 
  summarise(avg_min_temp_yr = round(mean(min_temp),2))


## ANNUAL PRECIP - COMBINED STATES
# Create df of ANNUAL PRECIP for Combined States for year 2000-2022
agg_precip_yr <- imputed_total_weather_data %>% 
  mutate(year = year(weather_date)) %>% 
  group_by(year) %>% 
  summarise(avg_precip_yr = mean(precip) * n_distinct(station))


###
# Plots
###


#
# Plot Top10 Crops
plot.top10_crops <- ggplot(data = top10_crops_2021.df,
                           aes(x = fct_inorder(crop), y = billion_tons,
                               fill = crop=="Soya beans")) +
  geom_col(width = 0.75) + 
  # Title, Subtitle, Caption Texts
  labs(title = "Top 10 Global Crops (2021)",
       y = "Billion Tons",
       caption = "Data Source: FAO") +
  guides(x = guide_axis(angle = 45)) +
  theme(legend.position = "None",
    axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, colour = "black", face = "bold"),
        axis.title.y = element_text(face = "bold"))+
  scale_fill_manual(values = c("blue4", "gold"))

# Generate plot
plot.top10_crops



# Plot top5 Countries
#
plot.top5 <- ggplot(data = top5_countries.df, 
                    aes(x = fct_inorder(top5_countries), y=top5_countries_pct,
                        fill = top5_countries == "United States")) +
  geom_col(width = 0.75) +
  # Title, Subtitle, Caption Texts
  labs(title = "Top Soybean Producing Countries (2022)",
       y = "Pct  Global  Production",
       caption = "Data Source: USDA/FAS/IPAD") +
  theme(legend.position = "None",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, colour = "black", face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  scale_fill_manual(values = c("blue4", "gold"))
  

# generate plot
plot.top5


# Plot map of Top 11 States
#
# Read Shape File of States into R from BigQuery Census dataset
# File in Zipped folder "tl_2022_us_state
shapefile_states <- st_read("tl_2022_us_state.shx")

# Filter to include only Contiguous States
shapefile_48 <- shapefile_states %>% 
  filter(!STUSPS %in% c('AK', 'HI', 'VI', 'MP', 'GU', 'AS', 'PR'))

# Join States Shapefile with Top11 dataframe
shapefile_soy_states <- shapefile_48 %>% 
  left_join(total_top11_soy_pct, by = c("STUSPS" = "st_abv"))

# Map of Top 11 Soy Producing States
plot.map_top11 <- ggplot(shapefile_soy_states) +
  geom_sf(aes(fill = pct_total_bushels)) +
  # Reverse the color scale 
  scale_fill_continuous(trans = 'reverse') +  
  # Title, Subtitle, Caption Texts
  labs(title = "Top Soybean Producing States",
       subtitle = "83% of US Soybean Production (2000-2022)",
       caption = "Data Source: USDA/NASS",
       fill = "Pct of Total") +
  # No background grid
  theme(panel.grid = element_blank()) +
  # No grid labels
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank())

#Generate Map Plot
plot.map_top11


#
#Combined Line Plots of Temps, Precip, Yield, Price
#
plot.combined_lines <- ggplot() +
  # avg max temp line graph
  geom_line(data=agg_max_temp_yr, aes(x=year, y=avg_max_temp_yr), color="red3", 
            linewidth=1.25) + 
  # avg mean temp line graph
  geom_line(data=agg_mean_temp_yr, aes(x=year, y=mean_temp_yr), color="firebrick1", 
            linewidth=1.25) + 
  geom_line(data=agg_min_temp_yr, aes(x=year, y=avg_min_temp_yr), color="tomato1", 
            linewidth=1.25) + 
  geom_line(data=agg_precip_yr, aes(x=year, y= avg_precip_yr), color="orange",
            linewidth=1.25) +
  # soy yield line graph
  geom_line(data=agg_soy_yield, aes(x=year, y=mean_bu_ac_yr), color="blue1",
            linewidth=1.25)+
  # soy price line graph
  geom_line(data=agg_soy_price, aes(x = year, y = mean_price), color="blue3", 
            linewidth = 1.25) +
  # Label max temp
  annotate(geom = "label", x=2001, y=65, label=" Max Temp ", color="white",
              label.size=NA, fill = "red3", size=4, fontface="bold")+
  # Label mean temp
  annotate(geom = "label", x=2001, y=54, label="Mean Temp", color="white", 
           label.size=NA, fill = "firebrick1", size=4, fontface="bold")+ 
  # Label min temp
  annotate(geom = "label", x=2001, y=44, label=" Min Temp ", color="white", 
           label.size=NA, fill = "tomato1", size=4, fontface="bold")+
  # Label precip
  annotate(geom = "label", x=2001, y=20, label = "  Precip ", color = "white",
           label.size = NA, fill = "orange", size = 4, fontface = "bold") +
  # Label soybean yield
  annotate(geom = "label", x=2001, y=34, label=" Soy Yield ", color="white", 
           label.size=NA, fill = "blue1", size=4, fontface="bold")+
  # Label soybean price
  annotate(geom = "label", x=2001, y=8, label=" Soy Price ", color="white", 
           label.size=NA, fill = "blue3", size=4, fontface="bold")+
  # Title, Subtitle, Caption Texts
  labs(title = "Temps, Precip, Yield, & Price",
       subtitle = "2000-2022: Top 11 States",
       caption = "Data Sources: USDA/NASS and NOAA/GSOD" ) +
  # No axis titles
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Generate Combined Plot
plot.combined_lines


#
# Bi-variate plot: mean temp vs. yield
# 

# Regression Analysis of Mean Temp vs. Soy Yield
regress.mean_t.soy_yield <- lm(agg_soy_yield$mean_bu_ac_yr ~ 
                                 agg_mean_temp_yr$mean_temp_yr)

# Regression Summary Mean Temp vs. Soy Yield - results are NOT statistically significant
summary(regress.mean_t.soy_yield)

# Mean Temp vs. Yield Pearson Correlation Coefficent
pears.mean_t.soy_yield <- cor(x = agg_mean_temp_yr$mean_temp_yr, 
                              y = agg_soy_yield$mean_bu_ac_yr, 
                              method = "pearson")

# Show Mean Temp Pearson Value
pears.mean_t.soy_yield

# Plot regression Mean Temp vs Soy Yield
plot.mean_t.soy_yield <- ggplot(regress.mean_t.soy_yield,
                                aes(x = agg_mean_temp_yr$mean_temp_yr, 
                                    y = agg_soy_yield$mean_bu_ac_yr)) +
  geom_point() +    # Scatter plot of data points
  geom_abline(slope = coef(regress.mean_t.soy_yield)[2], intercept = coef(regress.mean_t.soy_yield)[1],
              color = "blue", linewidth = 1) +  # Add regression line
  labs(title = "Mean Temp vs. Yield",
       subtitle = "R^2 = .0004,   P-Value = .93",
       x = "Avg Mean Temp",
       y = "Avg Yield (bu/ac)")

# Generate Regression Plot Mean Temp vs. Soy Yield
plot.mean_t.soy_yield


#
# Bi-variable plot: max temp vs. yield
# 

# Regression Analysis of Max Temp vs. Soy Yield
regress.max_t.soy_yield <- lm(agg_soy_yield$mean_bu_ac_yr ~ 
                                agg_max_temp_yr$avg_max_temp_yr)

# Regression Summary Max Temp vs. Soy Yield - results are NOT statistically significant
summary(regress.max_t.soy_yield)


# Max Temp vs. Yield Pearson Correlation Coefficent
pears.max_t.soy_yield <- cor(x = agg_max_temp_yr$avg_max_temp_yr, 
                              y = agg_soy_yield$mean_bu_ac_yr, 
                              method = "pearson")

# Show Max Temp Pearson Value
pears.max_t.soy_yield


# Plot regression Max Temp vs Soy Yield
plot.max_t.soy_yield <- ggplot(regress.max_t.soy_yield, 
                               aes(x = agg_max_temp_yr$avg_max_temp_yr, 
                                    y = agg_soy_yield$mean_bu_ac_yr)) +
  geom_point() +    # Scatter plot of data points
  geom_abline(slope = coef(regress.max_t.soy_yield)[2], intercept = coef(regress.max_t.soy_yield)[1],
              color = "blue", linewidth = 1) +  # Add regression line
  labs(title = "Max Temp vs. Yield",
       subtitle = "R^2 = .002,   P-Value = .83",
       x = "Avg Max Temp",
       y = "Avg Yield (bu/ac)")

# Generate Regression Plot Max Temp vs. Soy Yield
plot.max_t.soy_yield


#
# Bi-variate plot: min temp vs. yield
# 

# Regression Analysis of Min Temp vs. Soy Yield
regress.min_t.soy_yield <- lm(agg_soy_yield$mean_bu_ac_yr ~ 
                                agg_min_temp_yr$avg_min_temp_yr)

# Regression Summary Min Temp vs. Soy Yield - results are NOT statistically significant
summary(regress.min_t.soy_yield)


# Min Temp vs. Yield Pearson Correlation Coefficent
pears.min_t.soy_yield <- cor(x = agg_min_temp_yr$avg_min_temp_yr, 
                              y = agg_soy_yield$mean_bu_ac_yr, 
                              method = "pearson")

# Show Min Temp Pearson Value
pears.min_t.soy_yield


# Plot of Regression Line for Min Temp vs. Soy Yield Regression Analysis
plot.min_t.soy_yield <- ggplot(regress.min_t.soy_yield, 
                               aes(x = agg_min_temp_yr$avg_min_temp_yr, 
                                    y = agg_soy_yield$mean_bu_ac_yr)) +
  geom_point() +                     # Scatter plot of data points
  geom_abline(slope = coef(regress.min_t.soy_yield)[2], intercept = coef(regress.min_t.soy_yield)[1],
              color = "blue", linewidth = 1) +  # Add regression line
  labs(title = "Min Temp vs. Yield",
       subtitle = "R^2 = .0002,   P-Value = .95",
       x = "Avg Min Temp",
       y = "Avg Yield (bu/ac)")

# Generate Regression Plot Min Temp vs. Soy Yield
plot.min_t.soy_yield


#
# Bi-variable plot: precip vs. yield
# 

# Regression Analysis of Avg Precip vs. Soy Yield
regress.avg_prec.soy_yield <- lm(agg_soy_yield$mean_bu_ac_yr ~ 
                                agg_precip_yr$avg_precip_yr)

# Regression Summary Precip vs. Soy Yield - results ARE statistically significant
summary(regress.avg_prec.soy_yield)


# Precip vs. Yield Pearson Correlation Coefficent
pears.precip.soy_yield <- cor(x = agg_precip_yr$avg_precip_yr, 
                              y = agg_soy_yield$mean_bu_ac_yr, 
                              method = "pearson")

# Show Precip Pearson Value
pears.precip.soy_yield


# Plot of Regression Line for Avg Precip vs. Soy Yield Regression Analysis
plot.avg_prec.soy_yield <- ggplot(regress.avg_prec.soy_yield, 
                               aes(x = agg_precip_yr$avg_precip_yr, 
                                   y = agg_soy_yield$mean_bu_ac_yr)) +
  geom_point() +                     # Scatter plot of data points
  geom_abline(slope = coef(regress.avg_prec.soy_yield)[2], intercept = coef(regress.avg_prec.soy_yield)[1],
              color = "blue", linewidth = 1) +  # Add regression line
  labs(title = "Precip vs. Yield",
       subtitle = "R^2 = .67,   P-Value = .001",
       x = "Avg Precip (inches)",
       y = "Avg Yield (bu/ac)")

# Generate Regression Plot Avg Precip vs. Soy Yield
plot.avg_prec.soy_yield


#
# Bi-variable plot: yield vs. price
# 

# Regression Analysis of Soy Yield vs. Soy Price
regress.soy_yield.soy_price <- lm(agg_soy_price$mean_price ~ agg_soy_yield$mean_bu_ac_yr)

# Regression Summary Soy Yield vs. Soy Price - 
# results are 0.05 statistically significant but R-squared only 0.17, 
# i.e. yield predicts only 17% of price
summary(regress.soy_yield.soy_price)


# Yield vs. Price Pearson Correlation Coefficent
pears.soy_yield.soy_price <- cor(x = agg_soy_yield$mean_bu_ac_yr, 
                              y = agg_soy_price$mean_price, 
                              method = "pearson")

# Show Yield v Price Pearson Value
pears.soy_yield.soy_price


# Plot of Regression Line for Soy Yield vs. Soy Price Regression Analysis
plot.soy_yield.soy_price <- ggplot(regress.soy_yield.soy_price, 
                                   aes(x = agg_soy_yield$mean_bu_ac_yr, 
                                    y = agg_soy_price$mean_price)) +
  geom_point() +                     # Scatter plot of data points
  geom_abline(slope = coef(regress.soy_yield.soy_price)[2], intercept = coef(regress.soy_yield.soy_price)[1],
              color = "blue", linewidth = 1) +  # Add regression line
  labs(title = "Yield vs. Price",
       subtitle = "R^2 = .17,   P-Value = .05",
       x = "Avg Yield (bu/ac)",
       y = "Avg Price ($/bu)")

# Generate Regression Plot Soy Yield vs. Soy Price
plot.soy_yield.soy_price

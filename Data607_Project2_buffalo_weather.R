library(tidyverse)
library(dplyr)
library(ggplot2)

weather_df <- read_csv("/Users/alliewrubel/Desktop/buffalo_weather.csv")

print(weather_df)

#handle rows with missing values
weather_df_missing <- weather_df %>%
  filter(!if_all(JUL:ANNUAL, is.na))
    
View(weather_df_missing)

#get rid of rows that match header rows
weather_df_rows <- weather_df_missing %>%
  filter(if_any(JUL:ANNUAL, ~ !grepl("[A-Z]", .x)))

View(weather_df_rows)

#replace T values with 0
weather_df_rows_num <- weather_df_rows %>%
  mutate(across(JUL:ANNUAL, ~ ifelse(.x == "T", 0, .x)))

View(weather_df_rows_num)

#solve for problematic input
problematic_rows <- weather_df_rows_num %>%
  mutate(across(JUL:ANNUAL, ~ as.numeric(.x))) %>%
  filter(if_any(JUL:ANNUAL, is.na))

View(problematic_rows)

weather_df_rows_num <- weather_df_rows_num %>%
  mutate(across(JUL:ANNUAL, ~ ifelse(.x == "2,.5", 2.5, .x)))

View(weather_df_rows_num)


#change to numeric and replace missing value 
weather_df_missing_final <- weather_df_rows_num %>%
  mutate(across(JUL:ANNUAL, ~ as.numeric(.x)))%>%
  mutate(across(JUL:ANNUAL, ~ replace_na(.x, 0)))

View(weather_df_missing_final)


#before pivoting need to remove ANNUAL column
weather_df_pivot <- weather_df_missing_final %>%
  select(-ANNUAL)

View(weather_df_pivot)

#pivot data to long format
weather_pivot <- weather_df_pivot %>%
  pivot_longer(
    cols = JUL:JUN,
    names_to="Month",
    values_to="Snowfall"
  )

View(weather_pivot)

#simplify seasons by changing to start year (get rid of range)
weather_df_final <- weather_pivot %>%
  mutate(StartYear = as.numeric(str_extract(SEASON, "^[0-9]{4}")))%>%
  select(-SEASON)

View(weather_df_final)

#create factor so that the months are organized by seasaon, JUL-JUN
weather_df_final$Month <- factor(weather_df_final$Month, 
                                 levels = c("JUL", "AUG", "SEP", "OCT", "NOV", "DEC", 
                                            "JAN", "FEB", "MAR", "APR", "MAY", "JUN"))


#calculate total annual snowfall
total_snowfall_by_year <- weather_df_final %>%
  group_by(StartYear) %>%
  summarize(TotalSnowfall = sum(Snowfall, na.rm=TRUE))

#show total snowfall
ggplot(weather_df_final, aes(x=StartYear, y = Snowfall))+
  geom_bar(stat = "identity") +
  labs(x="Season", y="Total Snowfall (inches)", title="Total Annual Snowfall")
  

#snowfall distribution by month over all years
ggplot(weather_df_final, aes(x = Month, y = Snowfall)) +
  geom_boxplot() +
  labs(x = "Month", y = "Snowfall (inches)", title = "Snowfall Distribution by Month")

# Scatterplot of monthly snowfall over time
ggplot(weather_df_final, aes(x = StartYear, y = Snowfall, color = Month)) +
  geom_point(size = 2) +
  labs(x = "Start Year", y = "Snowfall (inches)", title = "Monthly Snowfall Over Time") 


#filter snowfall
filtered_df <- weather_df_final %>%
  filter(Snowfall > 5)

# Scatterplot of monthly snowfall over time
ggplot(filtered_df, aes(x = StartYear, y = Snowfall, color = Month)) +
  geom_point(size = 2) +
  labs(x = "Start Year", y = "Snowfall (inches)", title = "Monthly Snowfall Over Time") 

#get top 5% for snowfall
top_5_threshold <- quantile(weather_df_final$Snowfall, 0.95, na.rm = TRUE)

# Filter the data to get the top 5% of snowfall and sort descending order
top_5_snowfall <- weather_df_final %>%
  filter(Snowfall >= top_5_threshold)%>%
  arrange(desc(Snowfall))

# View the filtered data (Month, StartYear, Snowfall)
View(top_5_snowfall)



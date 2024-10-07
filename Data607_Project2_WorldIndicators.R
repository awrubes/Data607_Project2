library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)

world_df <- read_csv("/Users/alliewrubel/Desktop/worldindicators.csv")

View(world_df)

#get rid of country code, is redundant
world_df_col <- world_df %>%
    select(-`Country Code`)

View(world_df_col)

#remove rows missing values
world_df_missing <- world_df_col %>%
  filter(!if_all(everything(), is.na))

View(world_df_missing)

#remove last two rows
world_df_last <- world_df_missing[-((nrow(world_df_missing)-1):nrow(world_df_missing)), ]

View(world_df_last)

#pivot longer
world_df_pivot <- world_df_last %>%
  pivot_longer(
    cols = `2014 [YR2014]`:`2023 [YR2023]`,
    names_to="Year",
    values_to="Indicator"
  )

View(world_df_pivot)

#filter out "..." values and reformat the years
world_df_filter <- world_df_pivot %>%
  filter(!grepl("\\.\\.", Indicator))%>%
  mutate(
    Year = sub("^([0-9]{4}).*", "\\1", Year)
  )
  
View(world_df_filter)

#look at urban growth, filter out unneeded columns
world_df_urban <- world_df_filter %>%
  filter(grepl("^SP\\.URB\\.GROW$", `Series Code`))

View(world_df_urban)

# calculate average growth rate by country
world_df_num <- world_df_urban %>%
  mutate(Indicator_Numeric = as.numeric(Indicator))%>%
  select(-Indicator)%>%
  mutate(`Country Name` = trimws(`Country Name`))

str(world_df_num)

avg_growth_by_country <- world_df_num %>%
  group_by(`Country Name`) %>%
  summarize(Average_Growth = base::mean(Indicator_Numeric, na.rm=TRUE))%>%
  arrange(desc(Average_Growth))

View(avg_growth_by_country)


# Extract top 5 countries
top_5_countries <- avg_growth_by_country %>%
  arrange(desc(Average_Growth)) %>%
  slice_head(n = 5)

# Extract bottom 5 countries
bottom_5_countries <- avg_growth_by_country %>%
  arrange(Average_Growth) %>%
  slice_head(n = 5)

# Combine them for easier plotting
top_bottom_countries <- bind_rows(top_5_countries, bottom_5_countries)

ggplot(top_bottom_countries, aes(x = reorder(`Country Name`, Average_Growth), y = Average_Growth, fill = Average_Growth > 1)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for easier reading
  labs(x = "Country", y = "Average Urban Growth Rate (Annual %)", title = "Top 5 and Bottom 5 Countries by Urban Growth Rate") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green"), guide = "none")  # Different colors for top and bottom
  
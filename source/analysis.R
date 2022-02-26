data <- read.csv("../incarceration-trends/incarceration_trends.csv")

library("dplyr")
library("ggplot2")
library("plotly")
library("leaflet")
library("knitr")
library("stringr")


#Which county currenlt have the highest black jail population
high_black_jail_pop_county <- data %>% 
  filter(year == max(year)) %>% 
  group_by(county_name) %>% 
  summarize(black_jail_pop = max(black_jail_pop)) %>% 
  filter(black_jail_pop == max(na.omit(black_jail_pop))) %>% 
  pull(county_name)

#Which county currently have the lowest black jail population
low_black_jail_prop_county <- data %>% 
  filter(year == max(year)) %>% 
  group_by(county_name) %>% 
  summarize(black_jail_pop = min(black_jail_pop)) %>% 
  filter(black_jail_pop == min(na.omit(black_jail_pop))) %>% 
  head(1) %>% 
  pull(county_name)

# Average black jail population across county
average_black_jail_prop <- data %>% 
  filter(year == max(year)) %>% 
  group_by(county_name) %>% 
  summarize(mean(na.omit(black_jail_pop))) 
  
num_average_black_jail_prop <- na.omit(average_black_jail_prop$`mean(na.omit(black_jail_pop))` )%>% 
  mean()

#Which county have the highest proportion of black person in jail
high_prop_black <- data %>% 
  select(county_name, year, black_jail_pop, total_jail_pop) %>% 
  filter(year == max(year)) %>% 
  group_by(county_name) %>% 
  summarize(max_bl_rate = na.omit(max(black_jail_pop / total_jail_pop)))

county_high_prop_black <- high_prop_black %>% 
  filter(max_bl_rate == max(high_prop_black$max_bl_rate)) %>% 
  pull(county_name)


#How has the black jail popluation in Los Angeles County changed compared to 10 years ago?
black_prop_10_years <- data %>% 
  filter(year == max(year) - 10) %>% 
  group_by(county_name) %>% 
  summarise(na.omit(black_jail_pop / total_jail_pop)) 

LA__black_prop_10_year <- black_prop_10_years %>% 
  filter(county_name == "Los Angeles County") %>% 
  pull(`na.omit(black_jail_pop/total_jail_pop)`)
  
LA__black_prop_now <- high_prop_black %>% 
  filter(county_name == "Los Angeles County") %>% 
  pull(max_bl_rate)
  
LA_10_year_change <- LA__black_prop_now - LA__black_prop_10_year
LA_10_year_change
# chart: How people of different race population changed in Los Angeles County
LA_data <- data %>% 
  filter(county_name == "Los Angeles County") %>% 
  filter(year >= 1985)

LA_plot <- ggplot(LA_data, aes(x = year)) +
  geom_line(aes(y = black_jail_pop, color = "black")) +
  geom_line(aes(y = aapi_jail_pop, color = "Asian American / Pacific Islander")) +
  geom_line(aes(y = latinx_jail_pop, color = "Lantio")) +
  geom_line(aes(y = native_jail_pop, color = "Native American")) +
  geom_line(aes(y = white_jail_pop, color = "White")) +
  ggtitle("Jail population in LA county by race") +
  labs(x = "Year", y = "Population", color = "Race")


#The relationship between total population and black in jail pop in top 10 populated counties in different states
Top_10_pop_county <- data %>% 
  arrange(desc(total_pop)) %>% 
  filter(year == max(year)) %>% 
  group_by(state) %>% 
  mutate(black_ratio = black_jail_pop / total_jail_pop)

Black_Top_10_pop_county_plot <- ggplot(head(Top_10_pop_county, 10), aes(x = total_pop)) +
  geom_point(aes(y = black_ratio)) +
  ggtitle("Proportion of Black people in jail and county's population correlation chart") +
  labs(y = "Proportion of Black people in jail",x = "total population" )

# Maps of black prop in jail across the U.S
black_ratio_across_US <- data %>% 
  filter(year == max(year)) %>% 
  group_by(state) %>% 
  summarise(rate = mean(na.omit(black_jail_pop / total_jail_pop)))
black_ratio_across_US$state <- tolower(state.name[match(black_ratio_across_US$state, state.abb)])

# Join eviction data to the U.S. shapefile
state_shape <- map_data("state") %>% # load state shapefile
  rename(state = region) %>% # rename for joining
  left_join(black_ratio_across_US, by="state") # join eviction data

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Draw the map setting the `fill` of each state using its eviction rate
U.Smap <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = rate),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Black People in Jail Ratio") +
  blank_theme # variable containing map styles (defined in next code snippet)

  
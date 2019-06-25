##-----------------------------------------------------------------------##
##                    #TIDYTUESDAY: METEORITE IMPACTS                    ##
##-----------------------------------------------------------------------##


## R version 3.5.3 (2019-03-11)

## Author: Lisa Hehnke (dataplanes.org | @DataPlanes)


#-------#
# Setup #
#-------#

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(extrafont, gganimate, maps, tidyverse)

# Retrieve data
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")


#-----------#
# Set theme #
#-----------#

# Theme for map
map_theme <- theme(axis.line = element_blank(), 
                   axis.text.x = element_blank(), 
                   axis.text.y = element_blank(), 
                   axis.ticks = element_blank(), 
                   axis.ticks.length = unit(0, "pt"),
                   legend.position = "none", 
                   panel.background = element_rect(fill = "#212121"), 
                   panel.border = element_blank(), 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), 
                   plot.background = element_rect(fill = "#212121", colour = NA), 
                   plot.caption = element_text(colour = "#ffffff", size = 12), 
                   plot.margin = margin(10, 2, 2, 10),
                   plot.subtitle = element_text(colour = "#ffffff", size = 14), 
                   plot.title = element_text(face = "bold", colour = "#ffffff", size = 18), 
                   text = element_text(family = "Lato"))


#----------------#
# Data wrangling #
#----------------#

# Add decade and filter data
met_df <- meteorites %>%
  #filter(fall == "Fell") %>%
  select(year, long, lat, mass) %>%
  mutate(decade = floor(year / 10) * 10) %>%
  filter(decade >= 1900 & year <= 2010) %>%
  na.omit()


#------------------#
# Create dot frame #
#------------------#

# Generate a data frame with all dots
## Approach by Taras Kaduk (https://taraskaduk.com/2017/11/26/pixel-maps/).
lat <- tibble(lat = seq(-90, 90, by = 1))
long <- tibble(long = seq(-180, 180, by = 1))
dots <- lat %>% 
  merge(long, all = TRUE) %>%
  mutate(country = map.where("world", long, lat),
         lakes = map.where("lakes", long, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)
  

#------------#
# Static map #
#------------#

# Plot static map
## Inspired by Taras Kaduk (https://taraskaduk.com/2017/11/26/pixel-maps/).
ggplot() +
  borders("world") +
  geom_point(data = dots, aes(x = long, y = lat), col = "grey45", size = 0.7) + 
  geom_point(data = met_df, aes(x = long, y = lat, size = 0.8, colour = "red")) +
  geom_point(data = met_df, aes(x = long, y = lat, size = sqrt(mass)), colour = "red", alpha = 0.4) +
  labs(title = "\n#TidyTuesday: Meteorite impacts from 1900-2010", 
       subtitle = "Point sizes indicating meteorite mass in grams", 
      caption = "dataplanes.org | @DataPlanes \nInspired by @taraskaduk \nData source: NASA \n", x = NULL, y = NULL) + 
  coord_cartesian(ylim = c(-50, 90)) + map_theme

# Save map
ggsave("meteorite-impacts/meteorite_map.png", width = 18, height = 9, units = "in", dpi = 100)


#--------------#
# Animated map #
#--------------#

# Plot animated map using gganimate
meteorite_map <- ggplot() +
  borders("world", colour = "#353535", fill = "#2b2b2b") +
  geom_point(data = met_df, aes(x = long, y = lat, size = sqrt(mass)), colour = "red", alpha = 0.5) +
  transition_states(met_df$decade, transition_length = 1, state_length = 3, wrap = FALSE) +
  shadow_mark() + enter_fade() + exit_fade() + ease_aes("quadratic-in-out") +
  labs(title = "#TidyTuesday: Meteorite impacts from 1900-{closest_state} by decade", 
       subtitle = "Point sizes indicating meteorite mass in grams", 
       caption = "dataplanes.org | @DataPlanes \nData source: NASA", x = NULL, y = NULL) + 
  coord_cartesian(ylim = c(-50, 90)) + map_theme + 
  theme(plot.caption = element_text(size = 16), 
        plot.subtitle = element_text(size = 20), 
        plot.title = element_text(size = 28))

# Save GIF
anim_save("meteorite-impacts/meteorite_map_animated.gif", meteorite_map, width = 1800, height = 900)

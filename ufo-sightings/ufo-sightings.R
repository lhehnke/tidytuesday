##-----------------------------------------------------------------------##
##                      #TIDYTUESDAY: UFO SIGHTINGS                      ##
##-----------------------------------------------------------------------##


## R version 3.5.3 (2019-03-11)

## Author: Lisa Hehnke (dataplanes.org | @DataPlanes)


#-------#
# Setup #
#-------#

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(emo, extrafont, grid, jpeg, maps, threejs, tidyverse)
## Additional fonts: Exan (https://www.behance.net/gallery/36169711/Exan-3-Free-Font) | Orbitron (https://www.theleagueofmoveabletype.com/orbitron)

# Retrieve data
ufo_df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")


#-------------#
# UFO mappin' #
#-------------#

# Download NASA night lights image (grayscale)
download.file("https://eoimages.gsfc.nasa.gov/images/imagerecords/144000/144897/BlackMarble_2016_01deg_gray.jpg", 
              destfile = "ufo-sightings/BlackMarble_2016_01deg_gray.jpg", mode = "wb")

# Load jpg and render
earth <- readJPEG("ufo-sightings/BlackMarble_2016_01deg_gray.jpg", native = TRUE)
earth <- rasterGrob(earth, interpolate = TRUE)

# Plot UFO sightings around the world
ggplot() +
  annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
  geom_point(data = ufo_df, aes(x = longitude, y = latitude), alpha = 0.8, size = 0.1, colour = "green") + # Alien: 1f47d | UFO: 1f6f8
  theme(axis.title = element_blank(), 
      axis.text = element_blank(), 
      axis.ticks.length = unit(0, "cm"),
      legend.position = "none",
      panel.background = element_rect(fill = "#05050f", colour = "#05050f"), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm")) +
  annotate("text", x = -160, y = -18, hjust = 0, size = 14,
           label = paste(str_c("UFO sightings ", emo::ji("alien"))), color = "green", family = "Exan", alpha = 0.9) + # Open Sans
  annotate("text", x = -160, y = -26, hjust = 0, size = 7, 
           label = paste(str_c("Data: National UFO Reporting Center")), color = "white", family = "Orbitron", alpha = 0.8) +
  annotate("text", x = -160, y = -30, hjust = 0, size = 6, 
           label = paste("@DataPlanes"), color = "white", family = "Orbitron", alpha = 0.5) +
  coord_equal()

ggsave("ufo-sightings/UFO_map.png", width = 36, height = 18, units = "in", dpi = 100)


#----------#
# 3D globe #
#----------#

# Plot UFO sightings on 3D globe
globejs(img = "ufo-sightings/BlackMarble_2016_01deg_gray.jpg",
        bg = "black",
        lat = ufo_df$latitude,
        long = ufo_df$longitude,
        val = 3,
        color = "#32CD32",
        pointsize = 0.5,
        atmosphere = TRUE)
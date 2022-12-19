# 1.0) PACKAGES & SETUP ---------------------------------------------------

# setwd("~/Projects/Dive Log")
setwd("D:/Alejandro/Documents/R Projects/Dive Logs")

library(tidyverse)
library(ggplot2)
library(readr)
library(sf)
library(ggrepel)
library(OpenStreetMap)
library(showtext)

font_import() # takes a few minutes
loadfonts(device="postscript")

# IBM PLEX SANS
font_add(family = "IBM Plex Sans", # Name you want to use
         regular = "C:/Users/Alejandro/AppData/Local/Microsoft/Windows/Fonts/IBMPLEXSANS-REGULAR.ttf",
         bold = "C:/Users/Alejandro/AppData/Local/Microsoft/Windows/Fonts/IBMPlexSans-Bold.ttf")

# IBM PLEX SANS LIGHT (FOR PLOT TEXT)
font_add(family = "IBM Plex Sans Light", # Name you want to use
         regular = "C:/Users/Alejandro/AppData/Local/Microsoft/Windows/Fonts/IBMPlexSans-Light.ttf")

showtext_auto()

# read in dive data
data <- read_csv('Dive Log.csv') %>%
  separate(Coordinates, c('LAT', 'LONG'), sep = ',') %>%
  mutate(across(c('LAT', 'LONG'), ~str_trim(.)),
         across(c('LAT', 'LONG'), ~as.numeric(.)))


# Cozumel & Tulum
topL = c(20.8, -87.5)
botR = c(20.2,-86.65)


city <- tibble(city = c('Cozumel', 'Tulum'),
               LAT = c(20.44002169159058, 20.20767665898033),
               LONG = c(-86.81635271414827, -87.46579745482802))

# map polygon
osm <- openmap(upperLeft = topL, lowerRight = botR, zoom = 10,
               type = "bing", mergeTiles = TRUE) %>%
  openproj()




# 2.0) Map ---------------------------------------------------------------
# text labels for dive sites
names <- data %>%
  filter(Site %in% c('Palancar Jardines', 'Paraiso', 'Santa Rosa') |
           Location == 'Tulum') %>%
  distinct(Site, .keep_all = T)


yucatan <- autoplot.OpenStreetMap(osm)+
  geom_point(data = data %>%
               filter(Location %in% c('Cozumel', 'Tulum')), 
             aes(x = LONG, y = LAT), 
             shape = 21,
             size = 1,
             fill = 'red', color = 'white',
             alpha = 1)+
  geom_text_repel(data = city, size = 10,
                  family = 'IBM Plex Sans', color = 'white', fontface = 'bold',
                  aes(label = city, x = LONG, y = LAT))+
  geom_text_repel(data = names, size = 6,
                  family = 'IBM Plex Sans Light', color = 'white',
                  force=1, point.padding=unit(1,'lines'),
                  direction = 'both',
                  nudge_y = 0.02,
                  nudge_x = -0.03,
                  segment.size=0.1,
                  aes(label = Site, x = LONG, y = LAT))+
  xlab("Longitude") + ylab("Latitude")+
  ggtitle("Cozumel & Tulum Dives")+
  theme(text = element_text(family="IBM Plex Sans"))


ggsave(plot = yucatan,
       filename = paste0("Yucatan Dives.png"), 
       width = 6, height = 4, units = "in",
       dpi = 300, type = "cairo")



# 3.0) Stats --------------------------------------------------------------
# scatter
data %>%
  mutate(Dive = Dive %/% 10) %>%
  ggplot(aes(Time, Depth))+
  stat_ellipse(geom = 'polygon',
               aes(color = factor(Dive), fill = factor(Dive)), 
               linetype = 'dashed', alpha = 0.1)+
  geom_point(aes(fill = factor(Dive)),
             shape = 21, color = 'black', size = 4)+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  theme_bw()

# contour
data %>%
  mutate(Dive = Dive %/% 10) %>%
  ggplot(aes(Time, Depth,))+
  geom_density_2d_filled(bins = 5, color = 'black')+
  geom_point(shape = 21, color = 'black', size = 4)+
  scale_fill_viridis_d()


# stamen ------------------------------------------------------------------

library(ggmap)

# Cozumel & Tulum
topL = c(20.8, -87.5)
botR = c(20.2,-86.65)

st_map <- get_stamenmap(bbox = c(-87.5, 20.2, -86.65, 20.8),
                      zoom = 13,
                      maptype = 'terrain')

yucatan_st <- st_map %>%
  ggmap()+
  geom_point(data = data %>%
               filter(Location %in% c('Cozumel', 'Tulum')), 
             aes(x = LONG, y = LAT), 
             shape = 21,
             size = 1,
             fill = 'red', color = 'white',
             alpha = 1)+
  geom_text_repel(data = city, size = 10,
                  family = 'IBM Plex Sans', color = 'white', fontface = 'bold',
                  aes(label = city, x = LONG, y = LAT))+
  geom_text_repel(data = names, size = 6,
                  family = 'IBM Plex Sans Light', color = 'white',
                  force=1, point.padding=unit(1,'lines'),
                  direction = 'both',
                  nudge_y = 0.02,
                  nudge_x = -0.03,
                  segment.size=0.1,
                  aes(label = Site, x = LONG, y = LAT))+
  xlab("Longitude") + ylab("Latitude")+
  ggtitle("Cozumel & Tulum Dives")+
  theme(text = element_text(family="IBM Plex Sans"))

ggsave(plot = yucatan_st,
       filename = paste0("Yucatan Dives (stamen).png"), 
       width = 6, height = 4, units = "in",
       dpi = 300, type = "cairo")


# AUS ---------------------------------------------------------------------
names <- data %>%
  filter(Site %in% c('Yongala', "Steve's Bommie", 'Admiralty Anchor', 'Cod Hole', 'Hastings Reef')) %>%
  distinct(Site, .keep_all = T)

st_map <- get_stamenmap(bbox = c(left = 140, bottom = -20, 
                                 right = 148, top = -13.5),
                        zoom = 8,
                        maptype = 'terrain')

aus_st <- st_map %>%
  ggmap()+
  geom_point(data = data %>%
               filter(Country == 'Australia'), 
             aes(x = LONG, y = LAT), 
             shape = 21,
             size = 1,
             fill = 'red', color = 'white',
             alpha = 0.9)+
  geom_text_repel(data = names, size = 6,
                  family = 'IBM Plex Sans Light', color = 'white',
                  force=1, point.padding=unit(1,'lines'),
                  direction = 'both',
                  nudge_y = 0.02,
                  nudge_x = -0.03,
                  segment.size=0.1,
                  aes(label = Site, x = LONG, y = LAT))+
  xlab("Longitude") + ylab("Latitude")+
  ggtitle("Australia Dives")+
  theme(text = element_text(family="IBM Plex Sans"))

ggsave(plot = aus_st,
       filename = paste0("Australia Dives (stamen).png"), 
       width = 6, height = 4, units = "in",
       dpi = 300, type = "cairo")

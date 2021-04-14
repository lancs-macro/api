library(ggplot2)
library(dplyr)

countries <- ihpdr::ihpd_countries()
countries <- countries[-c(26:27)]
countries[c(14,20,21)] <- c("South Korea", "USA", "South Africa")

world_map <- map_data('world') %>% filter(region != "Antarctica")
world_countries <- world_map %>% pull(region) %>% unique()
world_map_wcountries <- world_map %>% filter(region %in% countries)

# countries %>% 
#   jsonlite::write_json("countries.json")

ggplot() + 
  geom_polygon(
    data = world_map, aes(x = long, y = lat, group = group),
    fill = "#808080", colour = "white") +
  geom_polygon(
    data = world_map_wcountries, aes(x = long, y = lat, group = group),
    fill = "#175797",colour = "white") +
  theme_void()
# ggsave("world-map.png")


library(ggplot2)
library(dplyr)

countries <- ihpdr::ihpd_countries()
countries <- countries[-c(26:27)]
countries[c(14,20,21)] <- c("South Korea", "USA", "South Africa")

world_map <- map_data('world') %>% filter(region != "Antarctica") %>% 
  mutate(is_region = if_else(region %in% countries, TRUE, FALSE))

library(plotly)

gg <- ggplot(world_map, aes(x = long, y = lat, group = group, fill = is_region, text = region)) +
  geom_polygon(colour = "white") + 
  scale_fill_manual(values = c("#808080", "#175797")) +
  theme_void() +
  theme(
    legend.position = "none"
  )
gg

plt <- plotly::ggplotly(gg, tooltip = c("text")) %>% 
  plotly::layout(
    yaxis = list(showgrid = FALSE, showline = FALSE),
    xaxis = list(showgrid = FALSE, showline = FALSE)
  ) %>% 
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("pan2d", "toggleSpikelines")
  )

htmlwidgets::saveWidget(
  widget = plotly::partial_bundle(plt), 
  file = paste0("public/visualizations/int/", ret_filename, ".html"),
  selfcontained = FALSE,
  libdir = "libs"
)
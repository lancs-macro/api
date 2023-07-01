

# TODO
# 1. Graph for UK website (x2)
# 1.1. Graph comparing hopi and uklr
# 2. Graph for int website (x2)

# Custom Controls
# https://rpubs.com/dvillasanao/Custom_controls_using_Plotly

# DO THAT
# https://jtr13.github.io/spring19/community_contribution_group17.html
# https://plotly.com/r/hover-text-and-formatting/


source("uk/01-download-uk.R")

library(tidyverse)
library(plotly)
library(transx)
options(transx.display = FALSE)

plot_index_uk <- function(data, returns = FALSE, save = FALSE) {
  ret <- if(returns) ".2f%" else ".2f"
  ret_title <- if(returns) "Year on Year (%)" else "Index"
  ret_filename <- if(returns) "rhpi_yoy" else "rhpi"
  
  plt <- data %>% 
    pivot_longer(c(`Greater London`, `United Kingdom`), names_to = "vars1", values_to = "vals1") %>% 
    pivot_longer(-c(Date, vars1, vals1), names_to = "vars2", values_to = "vals2") %>% 
    plot_ly(x = ~ Date, y = ~ vals1, color = ~ vars1, type = "scatter", mode = "line", colors = "viridis")%>% 
    add_trace(y= ~ vals2, color = ~ vars2,  mode = "line", visible = "legendonly") %>% 
    plotly::layout(
      title = "Real House Prices",
      hovermode = 'x unified',
      yaxis = list( hoverformat = ret, title = ret_title),
      xaxis = list(type = "date", tickformat="%Y-Q%q", title = "", showgrid = FALSE),
      hoverlabel = list(namelength = -1),
      legend = list(orientation = "h", xanchor = "center",  x = 0.5)
    ) %>% 
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("pan2d", "toggleSpikelines")
    )
  if(save) {
    htmlwidgets::saveWidget(
      widget = plotly::partial_bundle(plt), 
      file = paste0("public/visualizations/uk/", ret_filename, ".html"),
      selfcontained = FALSE,
      libdir = "libs"
    )
  }
  plt
}


rhpi %>% 
  plot_index_uk(save = TRUE)
rhpi %>% 
  mutate(across(-Date, ldiffx, 4)*100) %>% 
  drop_na() %>% 
  plot_index_uk(returns = TRUE, save = TRUE)


# uklr-hopi ---------------------------------------------------------------


lr <- uklr::ukhp_get("england-and-wales") %>% 
  mutate(region = str_to_title(region)) %>% 
  select(Date = date, region, "Land Registry (HPI)" = housePriceIndex)

ho <- ukhp_get(classification = "aggregate") %>% 
  select(Date, England) %>% 
  mutate(Date = lubridate::myd(Date, truncated = 1)) %>% 
  tidyr::pivot_longer(-Date, names_to = "region", values_to = "Housing Observatory (HOPI)")

lr_ho <- full_join(lr, ho, by = c("Date", "region")) %>% 
  filter(Date >= "1995-02-01") %>% 
  select(-region) %>% 
  mutate(across(-Date, ldiffx, 4)*100)


comparsison_plt <- lr_ho %>% 
  plot_ly(x = ~Date, y = ~`Land Registry (HPI)`, type = 'scatter', mode = 'lines', name = "Land Registry (HPI)", line = list(color = "#a6d71c")) %>% 
  add_trace(y = ~`Housing Observatory (HOPI)`, name = "Housing Observatory (HOPI)", line = list(color = "#992F2F")) %>% 
  plotly::layout(
    title = "England & Wales",
    hovermode = 'x unified',
    yaxis = list(hoverformat = ".2f%",title = "House Price Growth (YoY, %)"),
    xaxis = list(title = "", showgrid = FALSE),
    legend = list(y = 0.10, x = 0.01),
    hoverlabel = list(namelength = -1)
  ) %>% 
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("pan2d", "toggleSpikelines")
  )
htmlwidgets::saveWidget(
  widget = plotly::partial_bundle(comparsison_plt), 
  file = paste0("public/visualizations/uk/comparison.html"),
  selfcontained = FALSE,
  libdir = "libs"
)
  
  
  
  
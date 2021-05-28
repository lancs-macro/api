


# overview ----------------------------------------------------------------

# TODO to include as an iframe in uk and int static website

# gg <- rhpi %>% 
#   pivot_longer(-Date) %>% 
#   # group_by(name) %>%
#   # highlight_key(~name) %>% 
#   ggplot(aes(Date, value, col = name, text = paste(name))) +
#   geom_line() + 
#   labs(y = "Real House Prices", x = "") +
#   theme_bw() +
#   theme(
#     legend.title = element_blank()
#   ) 

# ggplotly(gg, tooltip = c("x", "y", "text")) %>% 
#   layout(
#     hovermode = 'x unified',
#     hovertemplate = "%{text}: %{y}"
#   ) %>% 
#   config(
#     displaylogo = FALSE,
#     modeBarButtonsToRemove = c("pan2d", "toggleSpikelines")
#   )
# 
# rhpi %>% 
#   pivot_longer(-Date) %>% 
#   group_by(name) %>%
#   highlight_key(~name) %>%
#   plot_ly() %>%
#   add_lines(x = ~Date, y = ~value, color= ~name, type = "line", mode = "lines", #, visible="legendonly"#,
#             # hovertemplate = paste( '%{x}', '%{y}')
#   ) %>% 
#   layout(
#     hovermode = 'x unified',
#     yaxis = list( hoverformat = '.2f', title = "Real House Prices"),
#     xaxis = list(title = "")
#   ) %>% 
#   config(
#     displaylogo = FALSE#,
#     modeBarButtonsToRemove = c("pan2d", "toggleSpikelines")
#   )

# just plot ---------------------------------------------------------------


make_plotly <- function(z, x, y, ctry = snames[i]) {
  ds_data <- datestamp(x, y) %>% 
    tidy() %>% 
    filter(id == ctry)
  zdata <- z %>% 
    pivot_longer(-Date, values_to = "Real House Price") %>% 
    filter(name == ctry) 
  lim <- zdata %>% 
    pull(`Real House Price`)
  gg <- zdata %>% 
    ggplot() +
    geom_line(aes(Date, `Real House Price`), size = 0.8) +
    geom_rect(
      data = ds_data, inherit.aes = FALSE, fill = "grey", alpha = 0.5,
      aes_string(xmin = "Start", xmax = "End", ymin = NULL, ymax = NULL), ymin=min(lim),ymax=max(lim) #min(lim), ymax = max(lim))
    ) +
    ggtitle(ctry) +
    theme_exuber()
  plotly::ggplotly(gg, tooltip = c("y", "x")) %>%
    layout(hovermode = 'compare')
}


for(i in 1:length(snames)) {
  aplt <- make_plotly(rhpi, radf_rhpi, mc_cv, snames[i])
  htmlwidgets::saveWidget(
    widget = plotly::partial_bundle(aplt), 
    file = paste0("public/visualizations/uk/rhpi/", file_snames[i], ".html"),
    selfcontained = FALSE,
    libdir = "libs",
    title = snames[i]
  )
}
for(i in 1:length(snames)) {
  aplt <- make_plotly(pti, radf_pti, mc_cv, snames[i])
  htmlwidgets::saveWidget(
    widget = plotly::partial_bundle(aplt), 
    file = paste0("public/visualizations/uk/pti/", file_snames[i], ".html"),
    selfcontained = FALSE,
    libdir = "libs",
    title = snames[i]
  )
}



# exuber ------------------------------------------------------------------


make_autoplotly <- function(x, y, ctry = snames[i]) {
  ds_data <- datestamp(x, y) %>% 
    tidy() %>% 
    filter(id == ctry)
  lim <- augment(x) %>% 
    filter(id == ctry) %>% 
    pull(bsadf)
  # line_data
  gg <- augment_join(x, y) %>% 
    filter(id == ctry, name == "bsadf", sig == 95) %>% 
    select(index, tstat, crit) %>% 
    set_names(c("Date", "Statistic", "Critical Value")) %>% 
    ggplot() +
    geom_line(aes(Date, Statistic), col = "blue", size = 0.8) +
    geom_line(aes(Date, `Critical Value`), col = "red", linetype = "dashed", size = 0.7) +
    ggtitle(ctry) +
    geom_rect(
      data = ds_data, inherit.aes = FALSE, fill = "grey", alpha = 0.5,
      aes_string(xmin = "Start", xmax = "End", ymin = min(lim), ymax = max(lim))
    ) +
    scale_exuber_manual() +
    theme_exuber()
  plotly::ggplotly(gg, tooltip = c("y", "x", "colour")) %>%
    plotly::layout(hovermode = 'compare') %>% 
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
    )
}
autoplotly(radf_rhpi, mc_cv)


for(i in 1:length(snames)) {
  aplt <- make_autoplotly(radf_rhpi, mc_cv, snames[i])
  htmlwidgets::saveWidget(
    widget = plotly::partial_bundle(aplt), 
    file = paste0("public/visualizations/uk/exuber-rhpi/", file_snames[i], ".html"),
    selfcontained = FALSE,
    libdir = "libs",
    title = snames[i]
  )
}
for(i in 1:length(snames)) {
  aplt <- make_autoplotly(radf_pti, mc_cv, snames[i])
  htmlwidgets::saveWidget(
    widget = plotly::partial_bundle(aplt), 
    file = paste0("public/visualizations/uk/exuber-pti/", file_snames[i], ".html"),
    selfcontained = FALSE,
    libdir = "libs",
    title = snames[i]
  )
}